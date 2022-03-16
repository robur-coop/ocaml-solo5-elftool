let query_manifest file =
  Owee_buf.map_binary file
  |> Solo5_elftool.query_manifest
  |> Result.iter (fun mft ->
      Fmt.pr "%a\n" Solo5_elftool.pp_mft mft)

let query_abi file =
  Owee_buf.map_binary file
  |> Solo5_elftool.query_abi
  |> Result.iter (Fmt.pr "%a\n" Solo5_elftool.pp_abi)

open Cmdliner

let file =
  let doc = "Solo5 executable" in
  Arg.(required & pos 0 (some file) None &
       info ~doc ~docv:"EXECUTABLE" [])

let query_manifest_cmd =
  let doc = "query solo5 manifest" in
  Cmd.v
    (Cmd.info ~doc "query-manifest")
    Term.(const query_manifest $ file)

let query_abi_cmd =
  let doc = "query solo5 abi" in
  Cmd.v
    (Cmdliner.Cmd.info ~doc "query-abi")
    Term.(const query_abi $ file)

let info = Cmd.info "osolo5-elftool"

let default_cmd =
  Term.(ret (const (fun man_format -> `Help (man_format, None)) $ Arg.man_format))

let () =
  let cmd =
    Cmd.group
      ~default:default_cmd info
      [query_manifest_cmd; query_abi_cmd]
  in
  Cmd.eval cmd |> exit
