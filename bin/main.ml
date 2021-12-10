let query_manifest file =
  Owee_buf.map_binary file
  |> Solo5_elftool.query_manifest
  |> Result.iter (fun mft ->
      Fmt.pr "%a\n" Solo5_elftool.pp_mft mft)

let file =
  let doc = "Solo5 executable" in
  Cmdliner.Arg.(required & pos 0 (some file) None &
                info ~doc ~docv:"EXECUTABLE" [])

let query_manifest_cmd =
  let doc = "query solo5 manifest" in
  Cmdliner.Term.(
    pure query_manifest $ file,
    info ~doc "query-manifest")

let () =
  ignore (Cmdliner.Term.eval query_manifest_cmd)
