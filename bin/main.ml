let hex_dump file =
  Owee_buf.map_binary file
  |> Solo5_elftool.foo
  |> Result.iter (fun mft ->
      Fmt.pr "%d elements\n" (List.length mft);
      Fmt.pr "%a\n" Fmt.(append (any "[") (append (list ~sep:(any ",\n") Solo5_elftool.pp_mft_entry) (any "]")))
        mft)

let file =
  let doc = "Solo5 executable" in
  Cmdliner.Arg.(required & pos 0 (some file) None &
                info ~doc ~docv:"EXECUTABLE" [])

let hex_dump_cmd =
  let doc = "hexdump solo5 manifest" in
  Cmdliner.Term.(
    pure hex_dump $ file,
    info ~doc "query-manifest")

let () =
  ignore (Cmdliner.Term.eval hex_dump_cmd)
