let retransmit fdin fdout =
  let buf_size = 4096 in
  let buffer = Bytes.create buf_size in
  let rec copy () =
    let bc = Unix.read fdin buffer 0 buf_size in
    if bc >= 0 then (
      ignore (Unix.write fdout buffer 0 bc) ;
      copy () )
  in
  copy ()

let run socket_id t =
  let rec loop () =
    let events = Kqueue_io.poll t () |> Result.get_ok in
    List.iter
      (fun (ident, {Kqueue_io.fd; _}) ->
        if ident = socket_id then
          let rec accept () =
            try
              let client_fd, _ = Unix.accept ~cloexec:true fd in
              Unix.set_nonblock client_fd ;
              ignore
                ( Kqueue_io.register {Kqueue_io.fd= client_fd; interest= `R} t
                |> Result.get_ok ) ;
              accept ()
            with
            | Unix.Unix_error (Unix.EAGAIN, _, _)
            | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
            ->
              ()
          in
          accept ()
        else
          try retransmit fd fd ; Unix.close fd
          with
          | Unix.Unix_error (Unix.EAGAIN, _, _)
          | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
          ->
            ())
      events ;
    loop ()
  in
  loop ()

let () =
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock socket ;
  Unix.setsockopt socket Unix.SO_REUSEADDR true ;
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind socket (Unix.ADDR_INET (addr, 8080)) ;
  Unix.listen socket 1024 ;
  let t = Kqueue_io.make () |> Result.get_ok in
  let socket_id =
    Kqueue_io.register {Kqueue_io.fd= socket; interest= `R} t |> Result.get_ok
  in
  run socket_id t
