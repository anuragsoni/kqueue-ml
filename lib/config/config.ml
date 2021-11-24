module C = Configurator.V1
module SMap = Map.Make (String)

let kqueue_available vars =
  SMap.exists
    (fun _ v ->
      match v with
      | C.C_define.Value.Switch true -> true
      | _ -> false)
    vars
;;

let () =
  C.main ~name:"kqueue.conf" (fun conf ->
      let operating_systems =
        [ "__APPLE__", C.C_define.Type.Switch
        ; "__FreeBSD__", C.C_define.Type.Switch
        ; "__OpenBSD__", C.C_define.Type.Switch
        ; "__DragonFly__", C.C_define.Type.Switch
        ; "__NetBSD__", C.C_define.Type.Switch
        ]
      in
      let results =
        SMap.of_seq (List.to_seq (C.C_define.import conf ~includes:[] operating_systems))
      in
      let vars =
        [ "KQUEUE_AVAILABLE", C.C_define.Value.Switch (kqueue_available results)
        ; "FREEBSD", SMap.find "__FreeBSD__" results
        ; "OPENBSD", SMap.find "__OpenBSD__" results
        ; "DRAGONFLY", SMap.find "__DragonFly__" results
        ; "NETBSD", SMap.find "__NetBSD__" results
        ]
      in
      C.C_define.gen_header_file conf ~fname:"config.h" vars)
;;
