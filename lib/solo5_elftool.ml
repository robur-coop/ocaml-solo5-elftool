type mft_type =
  | Dev_block_basic
  | Dev_net_basic
  | Reserved_first

let int_of_mft_type = function
  | Dev_block_basic -> 1
  | Dev_net_basic -> 2
  | Reserved_first -> 1 lsl 30

let mft_type_of_int = function
  | 1 -> Dev_block_basic
  | 2 -> Dev_net_basic
  | 1073741824 -> Reserved_first
  | _ -> assert false

let ( let* ) = Result.bind

let mft_max_entries = 64

let foo buf =
  let _header, sections = Owee_elf.read_elf buf in
  let* section =
    Owee_elf.find_section sections ".note.solo5.manifest"
    |> Option.to_result ~none:(`Msg "section .note.solo5.manifest not found")
  in
  let body = Owee_elf.section_body buf section in
  let cursor = Owee_buf.cursor body in
  let descsz =
    Owee_elf_notes.read_desc_size cursor
      ~expected_owner:"Solo5"
      ~expected_type:0x3154464d
  in
  let desc = Owee_buf.Read.fixed_string cursor descsz in
  Ok desc
