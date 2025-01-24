exception Elf_error

(* only the bits we care about *)
type header = {
  e_shoff : int;
  e_shentsize : int;
  e_shnum : int;
  e_shstrndx : int;
}

type section = {
  sh_offset : int;
  sh_size : int;
  sh_name_off : int;
  sh_name : string;
}

let section_manifest = ".note.solo5.manifest"
let section_abi = ".note.solo5.abi"
let note_name = "Solo5"
let typ_mft1 = 0x3154464d
let typ_abi1 = 0x31494241

let get_uint16 = function
  | `LE -> String.get_uint16_le
  | `BE -> String.get_uint16_be

let get_uint32 en s off =
  let get = match en with
    | `LE -> String.get_int32_le
    | `BE -> String.get_int32_be
  in
  Int32.to_int (get s off) land 0xFFFF_FFFF

let get_uint64 en s off =
  let get = match en with
    | `LE -> String.get_int64_le
    | `BE -> String.get_int64_be
  in
  match Int64.unsigned_to_int (get s off) with
  | None -> raise Elf_error
  | Some n -> n

let c_string s off maxlen =
  let rec scan_c_string i =
    if String.length s < off + i || i = maxlen then
      raise Elf_error
    else if s.[i+off] = '\000' then
      i
    else
      scan_c_string (succ i)
  in
  String.sub s off (scan_c_string 0)

let read_magic s off =
  if String.length s < off + 4 then
    raise Elf_error;
  let valid =
    String.get_uint8 s off = 0x7f &&
    s.[off+1] = 'E' && s.[off+2] = 'L' && s.[off+3] = 'F'
  in
  if not valid then
    raise Elf_error;
  off+4

let elfclass64 = 2

let read_identification s off =
  if String.length s < off + 12 then
    raise Elf_error;
  let elf_class = String.get_uint8 s off in
  let elf_data = String.get_uint8 s (off+1) in
  let _elf_version = String.get_uint8 s (off+2) in
  let _elf_osabi = String.get_uint8 s (off+3) in
  let _elf_abiversion = String.get_uint8 s (off+4) in
  (* Check padding *)
  for i = off + 5 to off+11 do
    if s.[i] <> '\000' then
      raise Elf_error
  done;
  (* we only support ELFCLASS64 *)
  if elf_class <> elfclass64 then
    raise Elf_error;
  let endianness =
    match elf_data with
    | 1 -> `LE
    | 2 -> `BE
    | _ -> raise Elf_error
  in
  endianness, off+12

let read_header en s =
  if String.length s < 16 + 48 then
    raise Elf_error;
  let e_shoff = get_uint32 en s 0x28 in
  let e_shentsize = get_uint16 en s 0x3a in
  let e_shnum = get_uint16 en s 0x3c in
  let e_shstrndx = get_uint16 en s 0x3e in
  if Sys.int_size <= 32 then
    raise Elf_error;
  { e_shoff; e_shentsize; e_shnum; e_shstrndx }

let read_section en s hdr i =
  let off = hdr.e_shoff + i * hdr.e_shentsize in
  if String.length s < off + 64 then
    raise Elf_error;
  let sh_name_off = get_uint32 en s off in
  let sh_offset = get_uint64 en s (off + 24) in
  let sh_size = get_uint64 en s (off + 32) in
  { sh_name_off; sh_offset; sh_size; sh_name = "" }

let read_section_name shstrndx s section =
  let off = shstrndx.sh_offset + section.sh_name_off in
  if String.length s < off + 1 then
    raise Elf_error;
  c_string s off (shstrndx.sh_size - section.sh_name_off)

let read_sections en s hdr =
  let sections = Array.init hdr.e_shnum (read_section en s hdr) in
  let shstrndx = sections.(hdr.e_shstrndx) in
  Array.map
    (fun section -> { section with sh_name = read_section_name shstrndx s section })
    sections

let find_section sections name =
  Array.find_opt
    (fun section -> String.equal section.sh_name name)
    sections

let section_body s section =
  if section.sh_offset < 0 || String.length s < section.sh_offset + section.sh_size then
    raise Elf_error;
  String.sub s section.sh_offset section.sh_size

let desc en section_body ~expected_owner ~expected_type =
  if String.length section_body < 12 then
    raise Elf_error;
  let namesz = get_uint32 en section_body 0 in
  let descsz = get_uint32 en section_body 4
  and typ = get_uint32 en section_body 8 in
  if String.length section_body < 12 + namesz + descsz then
    raise Elf_error;
  if typ <> expected_type ||
     String.length expected_owner + 1 <> namesz ||
     not (String.equal
            (expected_owner ^ "\000")
            (String.sub section_body 12 namesz))
  then
    None
  else
    let off = 12 + namesz in
    (* padding *)
    let off = off + ((4 - (off land 3)) land 3) in
    Some (String.sub section_body off descsz)

let find s section_name typ =
  let off = read_magic s 0 in
  let en, _off = read_identification s off in
  let hdr = read_header en s in
  let sections = read_sections en s hdr in
  match find_section sections section_name with
  | None -> None
  | Some section ->
    let body = section_body s section in
    desc en body ~expected_owner:note_name ~expected_type:typ
