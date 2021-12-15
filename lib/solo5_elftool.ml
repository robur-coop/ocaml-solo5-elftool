type mft_type =
  | Dev_block_basic
  | Dev_net_basic
  | Reserved_first

type mft_entry =
  | Dev_block_basic of string
  | Dev_net_basic of string

type mft = {
  version : int;
  entries : mft_entry list;
}

let mft_type_of_int : int32 -> mft_type = function
  | 1l -> Dev_block_basic
  | 2l -> Dev_net_basic
  | 1073741824l -> Reserved_first
  | _ -> assert false

let pp_mft_entry ppf = function
  | Dev_block_basic name ->
    Fmt.pf ppf {|{@[<1>@ "name": %S,@ "type": "BLOCK_BASIC"@]@ }|} name
  | Dev_net_basic name ->
    Fmt.pf ppf {|{@[<1>@ "name": %S,@ "type": "NET_BASIC"@]@ }|} name

let pp_mft ppf { version; entries } =
  Fmt.pf ppf
    {|{@[<1>@ "type": "solo5.manifest",@ "version": %d,@ "devices": [@[<1>@ %a@]@ ]@]@ }|}
    version Fmt.(list ~sep:(append (any ",") sp) pp_mft_entry) entries

let ( let* ) = Result.bind
let guard m b = if not b then Error (`Msg m) else Ok ()

let sizeof_mft_entry = 104
let mft_max_entries = 64l

let parse_mft_entry buf =
  (* invariant: Cstruct.length buf = sizeof_mft_entry *)
  let name_raw = Cstruct.sub buf 0 68 in
  let typ = Cstruct.LE.get_uint32 buf 68 in
  let u = Cstruct.sub buf 72 16 in
  let b = Cstruct.sub buf 88 8 in
  let attached = Cstruct.get_uint8 buf 96 <> 0 in
  let* name =
    Cstruct.cut ~sep:(Cstruct.create 1) name_raw
    |> Option.map (fun (name, _) -> Cstruct.to_string name)
    |> Option.to_result ~none:(`Msg "unterminated device name")
  in
  let* () = guard "non-zero mft_entry.u" (Cstruct.for_all ((=) '\000') u) in
  let* () = guard "non-zero mft_entry.b" (Cstruct.for_all ((=) '\000') b) in
  let* () = guard "non-zero mft_entry.attached" (not attached) in
  match mft_type_of_int typ with
  | Reserved_first ->
    let* () = guard "non-zero RESERVED_FIRST" (Cstruct.for_all ((=) '\000') name_raw) in
    Ok `Reserved_first
  | Dev_block_basic ->
    Ok (`Dev_block_basic name)
  | Dev_net_basic ->
    Ok (`Dev_net_basic name)

let parse_mft buf =
  let buf = Cstruct.of_string buf in
  let* () = guard "manifest too small"
      (Cstruct.length buf >= 4 + 8 + sizeof_mft_entry)
  in
  (* Solo5 defines a struct mft1_note consisting of the ELF note header
   * followed by a struct mft for reading and writing the ELF note. The note
   * header is 20 bytes long, so to get 8-byte alignment the note header is
   * padded with 4 bytes. See {[solo5/mft_abi.h]}. *)
  let buf = Cstruct.shift buf 4 in
  let version = Cstruct.LE.get_uint32 buf 0
  and entries = Cstruct.LE.get_uint32 buf 4
  in
  let* () = guard "unsupported manifest version" (version = 1l) in
  let* () = guard "zero manifest entries" (Int32.unsigned_compare entries 0l > 0) in
  (* this implicitly checks [Int32.to_int entries > 0] *)
  let* () = guard "too many manifest entries"
      (Int32.unsigned_compare entries mft_max_entries <= 0)
  in
  (* We have checked that entries interpreted unsigned is between 0 and
   * mft_max_entries, so this is safely equivalent to:
   *   (Option.get (Int32.unsigned_to_int entries) *)
  let entries = Int32.to_int entries in
  let buf = Cstruct.shift buf 8 in
  let* () = guard "unexpected note size"
      (Cstruct.length buf = entries * sizeof_mft_entry)
  in
  let* () =
    match parse_mft_entry (Cstruct.sub buf 0 sizeof_mft_entry) with
    | Ok `Reserved_first -> Ok ()
    | _ -> Error (`Msg "expected RESERVED_FIRST")
  in
  let buf = Cstruct.shift buf sizeof_mft_entry in
  let entries =
    Array.init (entries - 1)
      (fun i -> Cstruct.sub buf (i * sizeof_mft_entry) sizeof_mft_entry)
  in
  let* entries =
    Array.fold_left
      (fun r buf ->
         let* acc = r in
         let* mft_entry = parse_mft_entry buf in
         match mft_entry with
         | `Dev_block_basic name -> Ok (Dev_block_basic name :: acc)
         | `Dev_net_basic name -> Ok (Dev_net_basic name :: acc)
         | `Reserved_first -> Error (`Msg "found RESERVED_FIRST not as first entry"))
      (Ok [])
      entries
    |> Result.map List.rev
  in
  Ok { version = Int32.to_int version; entries }

let ( let* ) = Result.bind

let mft1_note_name = "Solo5"

let query_manifest buf =
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
      ~expected_type:0x3154464d (* MFT1 *)
  in
  let desc = Owee_buf.Read.fixed_string cursor descsz in
  let* () = guard "extra data" (Owee_buf.at_end cursor) in
  parse_mft desc
