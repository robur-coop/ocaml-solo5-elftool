let read_binary file =
  let ic = open_in_bin file in
  let res = Buffer.create 16384 in
  let buf = Bytes.create 16384 in
  let rec loop () =
    let len = input ic buf 0 16384 in
    if len > 0 then
      let () = Buffer.add_subbytes res buf 0 len in
      loop ()
  in
  loop ();
  close_in_noerr ic;
  Buffer.contents res

let query_manifest file =
  read_binary file
  |> Solo5_elftool.query_manifest
  |> Result.fold
    ~ok:(fun mft ->
        Fmt.pr "%a\n" Solo5_elftool.pp_mft mft)
    ~error:(fun (`Msg e) ->
        Fmt.epr "%s\n" e)

let query_abi file =
  read_binary file
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
