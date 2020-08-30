open Kqueue
open Ctypes

module Counter = struct
  let make () =
    let count = ref Int64.zero in
    let rec next () =
      let current = !count in
      let next' = Int64.succ current in
      if !count == current then (
        count := next' ;
        current )
      else next ()
    in
    next
end

module Syntax = struct
  module R = struct
    let ( let+ ) t f = Result.map f t

    let ( let* ) t f = Result.bind t f
  end
end

module Util = struct
  let fd_to_int : Unix.file_descr -> int = Obj.magic

  let fd_of_int : int -> Unix.file_descr = Obj.magic
end

type interest = [`R | `W | `RW]

type event = {fd: Unix.file_descr; interest: interest}

type t =
  { kqueue: Kqueue.t
  ; counter: unit -> Int64.t
  ; eventlist: Bindings.Kevent.t Ctypes.CArray.t }

type error = [Kqueue.error | `Unknown_event_kind of int]

exception Unknown_event of int

let make ?(eventlist_size = 1024) () =
  let open Syntax.R in
  let+ kqueue = Kqueue.kqueue () in
  { kqueue
  ; counter= Counter.make ()
  ; eventlist= CArray.make Bindings.Kevent.t eventlist_size }

type action = Add | Del

let make_events action {fd; interest} udata =
  let flags =
    match action with
    | Add ->
        Unsigned.UInt16.logor Bindings.ev_add Bindings.ev_clear
    | Del ->
        Bindings.ev_delete
  in
  let filters =
    match interest with
    | `R ->
        [Bindings.evfilt_read]
    | `W ->
        [Bindings.evfilt_write]
    | `RW ->
        [Bindings.evfilt_read; Bindings.evfilt_write]
  in
  CArray.of_list Bindings.Kevent.t
    (List.map
       (fun filter ->
         Kevent.make
           ~ident:(Uintptr.of_int (Util.fd_to_int fd))
           ~filter ~flags ~fflags:Unsigned.UInt32.zero ~data:Intptr.zero
           ~udata:(Uintptr.of_int64 udata))
       filters)

let register event t =
  let open Syntax.R in
  let id = t.counter () in
  let changelist = make_events Add event id in
  let+ _ =
    kevent t.kqueue ~changelist ~eventlist:(CArray.make Bindings.Kevent.t 0)
  in
  id

let delete fd t =
  let open Syntax.R in
  let changelist = make_events Del {interest= `RW; fd} 0L in
  let+ _ =
    kevent t.kqueue ~changelist ~eventlist:(CArray.make Bindings.Kevent.t 0)
  in
  ()

let poll ?timeout t () =
  let open Syntax.R in
  let* count =
    kevent ?timeout t.kqueue
      ~changelist:(CArray.make Bindings.Kevent.t 0)
      ~eventlist:t.eventlist
  in
  let events = CArray.sub t.eventlist ~pos:0 ~length:count |> CArray.to_list in
  try
    let res =
      List.map
        (fun ev ->
          let fd = Util.fd_of_int (Kevent.ident ev |> Uintptr.to_int) in
          let udata = Kevent.udata ev |> Uintptr.to_int64 in
          let interest =
            match Kevent.filter ev with
            | f when f = Bindings.evfilt_read ->
                `R
            | f when f = Bindings.evfilt_write ->
                `W
            | f ->
                raise_notrace (Unknown_event f)
          in
          (udata, {fd; interest}))
        events
    in
    Ok res
  with Unknown_event f -> Error (`Unknown_event_kind f)
