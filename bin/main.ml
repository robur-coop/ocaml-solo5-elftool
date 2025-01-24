let query_manifest file =
  Owee_buf.map_binary file
  |> Solo5_elftool.query_manifest
  |> Result.iter (fun mft ->
      Fmt.pr "%a\n" Solo5_elftool.pp_mft mft)

let query_abi file =
  Owee_buf.map_binary file
  |> Solo5_elftool.query_abi
  |> Result.iter (Fmt.pr "%a\n" Solo5_elftool.pp_abi)

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
