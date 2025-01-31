module Bstr = Cachet.Bstr

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
  | `LE -> Cachet.get_uint16_le
  | `BE -> Cachet.get_uint16_be

let get_uint32 en s off =
  let get = match en with
    | `LE -> Cachet.get_int32_le
    | `BE -> Cachet.get_int32_be
  in
  Int32.to_int (get s off) land 0xFFFF_FFFF

let get_uint64 en s off =
  let get = match en with
    | `LE -> Cachet.get_int64_le
    | `BE -> Cachet.get_int64_be
  in
  match Int64.unsigned_to_int (get s off) with
  | None -> raise Elf_error
  | Some n -> n

let c_string seq maxlen =
  let res = Buffer.create maxlen in
  let rec scan i = function
    | Seq.Nil -> raise Elf_error
    | Seq.Cons (s, seq) ->
      match String.index_opt s '\000' with
      | None ->
        let i = i + String.length s in
        if i >= maxlen then
          raise Elf_error;
        Buffer.add_string res s;
        scan i (seq ())
      | Some l ->
        let i = i + l in
        if i >= maxlen then
          raise Elf_error;
        Buffer.add_substring res s 0 l;
        Buffer.contents res
  in
  scan 0 (seq ())

let read_magic c =
  if not (Cachet.get_uint8 c 0 = 0x7f &&
          String.equal (Cachet.get_string c ~len:3 1) "ELF")
  then raise Elf_error

let elfclass64 = 2

let read_identification c =
  let elf_class = Cachet.get_uint8 c 4 in
  let elf_data = Cachet.get_uint8 c 5 in
  let _elf_version = Cachet.get_uint8 c 6 in
  let _elf_osabi = Cachet.get_uint8 c 7 in
  let _elf_abiversion = Cachet.get_uint8 c 8 in
  for i = 9 to 15 do
    if Cachet.get_uint8 c i <> 0 then
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
  endianness

let read_header en c =
  let e_shoff = get_uint32 en c 0x28 in
  let e_shentsize = get_uint16 en c 0x3a in
  let e_shnum = get_uint16 en c 0x3c in
  let e_shstrndx = get_uint16 en c 0x3e in
  if Sys.int_size <= 32 then
    raise Elf_error;
  { e_shoff; e_shentsize; e_shnum; e_shstrndx }

let read_section en c hdr i =
  let off = hdr.e_shoff + i * hdr.e_shentsize in
  let sh_name_off = get_uint32 en c off in
  let sh_offset = get_uint64 en c (off + 24) in
  let sh_size = get_uint64 en c (off + 32) in
  { sh_name_off; sh_offset; sh_size; sh_name = "" }

let read_section_name shstrndx c section =
  let off = shstrndx.sh_offset + section.sh_name_off in
  c_string (Cachet.get_seq c off) (shstrndx.sh_size - section.sh_name_off)

let read_sections en c hdr =
  let sections = Array.init hdr.e_shnum (read_section en c hdr) in
  let shstrndx = sections.(hdr.e_shstrndx) in
  Array.map
    (fun section -> { section with sh_name = read_section_name shstrndx c section })
    sections

let find_section sections name =
  Array.find_opt
    (fun section -> String.equal section.sh_name name)
    sections

let desc en c section ~expected_owner ~expected_type =
  let off = section.sh_offset in
  if section.sh_size < 12 then
    raise Elf_error;
  let namesz = get_uint32 en c off
  and descsz = get_uint32 en c (off + 4)
  and typ = get_uint32 en c (off + 8) in
  if typ <> expected_type ||
     String.length expected_owner + 1 <> namesz ||
     not (String.equal
            (expected_owner ^ "\000")
            (Cachet.get_string c (off+12) ~len:namesz))
  then
    None
  else
    let off = off + 12 + namesz in
    (* padding *)
    let off = off + ((4 - (off land 3)) land 3) in
    Some (Cachet.get_string c off ~len:descsz)

let find c section_name typ =
  let () = read_magic c in
  let en = read_identification c in
  let hdr = read_header en c in
  let sections = read_sections en c hdr in
  match find_section sections section_name with
  | None -> None
  | Some section ->
    desc en c section ~expected_owner:note_name ~expected_type:typ
