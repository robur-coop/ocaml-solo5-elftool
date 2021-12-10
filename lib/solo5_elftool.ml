type mft_type =
  | Dev_block_basic
  | Dev_net_basic
  | Reserved_first

let int_of_mft_type = function
  | Dev_block_basic -> 1l
  | Dev_net_basic -> 2l
  | Reserved_first -> Int32.shift_left 1l 30

let mft_type_of_int = function
  | 1l -> Dev_block_basic
  | 2l -> Dev_net_basic
  | 1073741824l -> Reserved_first
  | _ -> assert false

type mft_entry =
  | Reserved_first
  | Dev_block_basic of {
      name : string;
    }
  | Dev_net_basic of {
      name : string;
    }

type mft = {
  version : int;
  entries : mft_entry array;
}

let pp_mft_entry ppf = function
  | Reserved_first ->
    Fmt.pf ppf "MFT_RESERVED_FIRST"
  | Dev_block_basic { name; _ } ->
    Fmt.pf ppf {|{@[<1>@ "name": %S,@ "type": "BLOCK_BASIC"@]@ }|} name
  | Dev_net_basic { name; _ } ->
    Fmt.pf ppf {|{@[<1>@ "name": %S,@ "type": "NET_BASIC"@]@ }|} name

let pp_mft ppf { version; entries } =
  Fmt.pf ppf
    {|{@[<1>@ "type": "solo5.manifest",@ "version": %d,@ "devices": [@[<1>@ %a@]@ ]@]@ }|}
    version Fmt.(array ~sep:(append (any ",") sp) pp_mft_entry) entries

let sizeof_mft_entry = 104

let parse_mft_entry buf =
  assert (Cstruct.length buf = sizeof_mft_entry);
  let name_raw = Cstruct.sub buf 0 68 in
  let typ = Cstruct.LE.get_uint32 buf 68 in
  let u = Cstruct.sub buf 72 16 in
  let b = Cstruct.sub buf 88 8 in
  let attached = Cstruct.get_uint8 buf 96 <> 0 in
  let name =
    Cstruct.cut ~sep:(Cstruct.create 1) name_raw
    |> Option.map (fun (name, _) -> Cstruct.to_string name)
  in
  assert (Cstruct.for_all ((=) '\000') u);
  assert (Cstruct.for_all ((=) '\000') b);
  assert (not attached);
  match mft_type_of_int typ with
  | Reserved_first ->
    assert (Cstruct.for_all ((=) '\000') name_raw);
    Reserved_first
  | Dev_block_basic ->
    let name = Option.get name in
    Dev_block_basic { name; }
  | Dev_net_basic ->
    let name = Option.get name in
    Dev_net_basic { name; }

let parse_mft buf =
  let buf = Cstruct.of_string buf in
  (* FIXME: explanation why solo5 adds this padding *)
  let buf = Cstruct.shift buf 4 in
  let version = Cstruct.LE.get_uint32 buf 0
  and entries = Cstruct.LE.get_uint32 buf 4
  in
  assert (version = 1l);
  let buf = Cstruct.shift buf 8 in
  Printf.printf "MFT%ld[%ld]\n" version entries;
  let entries =
    Array.init (Int32.unsigned_to_int entries |> Option.get (* XXX: assume 64 bit *))
      (fun i -> parse_mft_entry (Cstruct.sub buf (i * sizeof_mft_entry) sizeof_mft_entry))
  in
  { version = Int32.to_int version; entries }

let ( let* ) = Result.bind

let mft_max_entries = 64
let mft1_note_name = "Solo5"

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
      ~expected_owner:mft1_note_name
      ~expected_type:0x3154464d
  in
  let desc = Owee_buf.Read.fixed_string cursor descsz in
  assert (Owee_buf.at_end cursor);
  Ok (parse_mft desc)
