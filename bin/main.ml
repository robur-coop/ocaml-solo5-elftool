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
  Cmdliner.Term.(
    pure query_manifest $ file,
    info ~doc "query-manifest")

let query_abi_cmd =
  let doc = "query solo5 abi" in
  Cmdliner.Term.(
    pure query_abi $ file,
    info ~doc "query-abi")

let default_cmd =
  Cmdliner.Term.(
    ret (pure (fun man_format -> `Help (man_format, None)) $ man_format),
    info "osolo5-elftool")

let () =
  ignore (Cmdliner.Term.eval_choice
            default_cmd
            [query_manifest_cmd; query_abi_cmd])
