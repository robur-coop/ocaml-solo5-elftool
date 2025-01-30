let map_binary file =
  let fd = Unix.openfile file [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  let stat = Unix.fstat fd in
  let map () ~pos len =
    let len = Int.min (stat.Unix.st_size - pos) len in
    let pos = Int64.of_int pos in
    let barr =
      Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
    in
    Bigarray.array1_of_genarray barr
  in
  Cachet.make ~map ()

let query_manifest file =
  map_binary file
  |> Solo5_elftool.query_manifest
  |> Result.fold
    ~ok:(fun mft ->
        Fmt.pr "%a\n" Solo5_elftool.pp_mft mft)
    ~error:(fun (`Msg e) ->
        Fmt.epr "%s\n" e)

let query_abi file =
  map_binary file
  |> Solo5_elftool.query_abi
  |> Result.fold
    ~ok:(fun abi -> Fmt.pr "%a\n" Solo5_elftool.pp_abi abi)
    ~error:(fun (`Msg e) ->
        Fmt.epr "%s\n" e)

let file =
  let doc = "Solo5 executable" in
  Cmdliner.Arg.(required & pos 0 (some file) None &
                info ~doc ~docv:"EXECUTABLE" [])

let query_manifest_cmd =
  let doc = "query solo5 manifest" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info ~doc "query-manifest")
    Cmdliner.Term.(const query_manifest $ file)

let query_abi_cmd =
  let doc = "query solo5 abi" in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info ~doc "query-abi")
    Cmdliner.Term.(const query_abi $ file)

let default_cmd =
  let open Cmdliner.Term in
  ret (const (fun man_format -> `Help (man_format, None)) $ Cmdliner.Arg.man_format)

let () =
  let cmd =
    Cmdliner.Cmd.group ~default:default_cmd
       (Cmdliner.Cmd.info "osolo5-elftool")
       [query_manifest_cmd; query_abi_cmd]
  in
  exit (Cmdliner.Cmd.eval cmd)
