type t

type error = [Kqueue.error | `Unknown_event_kind of int]

type interest = [`R | `W | `RW]

type event = {fd: Unix.file_descr; interest: interest}

val make : ?eventlist_size:int -> unit -> (t, [> error]) result

val register : event -> t -> (Int64.t, [> error]) result

val delete : Unix.file_descr -> t -> (unit, [> error]) result

val poll :
     ?timeout:Kqueue.Timespec.t
  -> t
  -> unit
  -> ((Int64.t * event) list, [> error]) result
