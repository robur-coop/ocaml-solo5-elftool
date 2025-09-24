module Bstr = Cachet.Bstr

exception Elf_error

(* only the bits we care about *)
type header = {
  e_phoff : int;
  e_shoff : int;
  e_phentsize : int;
  e_phnum : int;
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
  let e_phoff = get_uint64 en c 0x20 in
  let e_shoff = get_uint64 en c 0x28 in
  let e_phentsize = get_uint16 en c 0x36 in
  let e_phnum = get_uint16 en c 0x38 in
  let e_shentsize = get_uint16 en c 0x3a in
  let e_shnum = get_uint16 en c 0x3c in
  let e_shstrndx = get_uint16 en c 0x3e in
  if Sys.int_size <= 32 then
    raise Elf_error;
  { e_phoff; e_shoff; e_phentsize; e_phnum; e_shentsize; e_shnum; e_shstrndx }

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

type note_data = {
  off : int;
  size : int;
  buf : Buffer.t;
}

type state =
  | Initial of Buffer.t
  | Magic_ok of Buffer.t
  | Identification_ok of {
      buf : Buffer.t;
      en : [ `LE | `BE ]
    }
  | Header_ok of {
      hdr : header;
      pos : int;
      buf : Buffer.t;
      en : [ `LE | `BE ];
    }
  | Program_header_ok of {
      hdr : header;
      notes : note_data list;
      pos : int;
      buf : Buffer.t;
      en : [ `LE | `BE ];
    }
  | Sections_without_name of {
      hdr : header;
      notes : note_data list;
      sections : section array;
      pos : int;
      buf : Buffer.t;
      en : [ `LE | `BE ];
    }
  | Sections of {
      notes : note_data list;
      mft_section : section;
      abi_section : section;
      en : [ `LE | `BE ];
    }
  | Done of { mft : string; abi : string }
  | Fail of [ `Msg of string ]

let fresh_state () = Initial (Buffer.create 64)

let get_uint8 b pos =
  int_of_char (Buffer.nth b pos)

let get_uint16 en b pos =
  let get = match en with
    | `LE -> String.get_uint16_le
    | `BE -> String.get_uint16_be
  in
  get (Buffer.sub b pos 2) 0

let get_uint32 en b pos =
  let get = match en with
    | `LE -> String.get_int32_le
    | `BE -> String.get_int32_be
  in
  Int32.to_int (get (Buffer.sub b pos 4) 0) land 0xFFFF_FFFF

let get_uint64 en b pos =
  let get = match en with
    | `LE -> String.get_int64_le
    | `BE -> String.get_int64_be
  in
  match Int64.unsigned_to_int (get (Buffer.sub b pos 8) 0) with
  | None -> raise Elf_error
  | Some n -> n

let c_string buf off max_size =
  let contents = Buffer.contents buf in
  match String.index_from_opt contents off '\000' with
  | None ->
    raise Elf_error
  | exception Invalid_argument _->
    raise Elf_error
  | Some fin ->
    if fin - off >= max_size then
      raise Elf_error
    else
      String.sub contents off (fin - off)

let rec feed state data =
  match state with
  | Initial b ->
    Buffer.add_string b data;
    if Buffer.length b >= 4 then
      if String.equal (Buffer.sub b 0 4) "\x7fELF" then
        feed (Magic_ok b) ""
      else
        Fail (`Msg "Not an ELF file")
    else Initial b
  | Magic_ok b ->
    Buffer.add_string b data;
    if Buffer.length b >= 16 then
      let elf_class = get_uint8 b 4 in
      let elf_data = get_uint8 b 5 in
      match for i = 9 to 15 do
          if get_uint8 b i <> 0 then
            raise Elf_error
        done with
      | exception Elf_error ->
        Fail (`Msg "bad ELF identification")
      | () ->
        if elf_class <> elfclass64 then
          Fail (`Msg "Unsupported ELF class")
        else
          try
            match elf_data with
            | 1 -> feed (Identification_ok { buf=b; en=`LE }) ""
            | 2 -> feed (Identification_ok { buf=b; en=`BE }) ""
            | _ -> raise Elf_error
          with Elf_error -> Fail (`Msg "Bad endianness")
    else Magic_ok b
  | Identification_ok { buf; en } ->
    Buffer.add_string buf data;
    if Buffer.length buf >= 64 then
      let e_phoff = get_uint64 en buf 0x20 in
      let e_shoff = get_uint64 en buf 0x28 in
      let e_phentsize = get_uint16 en buf 0x36 in
      let e_phnum = get_uint16 en buf 0x38 in
      let e_shentsize = get_uint16 en buf 0x3a in
      let e_shnum = get_uint16 en buf 0x3c in
      let e_shstrndx = get_uint16 en buf 0x3e in
      if Sys.int_size <= 32 then
        raise Elf_error;
      let hdr = { e_phoff; e_shoff; e_phentsize; e_phnum; e_shentsize; e_shnum; e_shstrndx } in
      let pos = Buffer.length buf in
      let buf =
        if pos >= hdr.e_phoff then
          let buf' = Buffer.create 4096 in
          Buffer.add_string buf' (Buffer.sub buf hdr.e_phoff (Buffer.length buf - hdr.e_phoff));
          buf'
        else
          Buffer.create 4096
      in
      feed (Header_ok { hdr; pos; buf; en; }) ""
    else
      Identification_ok { buf; en }
  | Header_ok { hdr; pos; buf; en } ->
    if pos + String.length data >= hdr.e_phoff then begin
      let off = max 0 (hdr.e_phoff - pos) in
      let len = String.length data - off in
      Buffer.add_substring buf data off len
    end;
    let pos = pos + String.length data in
    if Buffer.length buf >= hdr.e_phnum * hdr.e_phentsize then
      (* NOTE: not tail recursive *)
      let rec gather i =
        let off = i * hdr.e_phentsize in
        if i >= hdr.e_phnum then
          []
        else
          let p_type = get_uint32 en buf off in
          if p_type = 0x00000004 (* PT_NOTE *) then
            let p_offset = get_uint64 en buf (off + 0x08) in
            let p_filesz = get_uint64 en buf (off + 0x20) in
            { off = p_offset; size = p_filesz; buf = Buffer.create p_filesz } :: gather (succ i)
          else
            gather (succ i)
      in
      let notes = gather 0 in
      let buf =
        if pos >= hdr.e_shoff then
          let buf' = Buffer.create 4096 in
          Buffer.add_string buf' (Buffer.sub buf (hdr.e_shoff - hdr.e_phoff) (Buffer.length buf - (hdr.e_shoff - hdr.e_phoff)));
          buf'
        else
          Buffer.create 4096
      in
      feed (Program_header_ok { hdr; notes; pos; buf; en; }) ""
    else Header_ok { hdr; pos; buf; en }
  | Program_header_ok { hdr; notes; pos; buf; en } ->
    List.iter (fun { off; size; buf } ->
        let len =
          if pos < off then
            max 0 (pos + String.length data - off)
          else if Buffer.length buf >= size then 0
          else String.length data
        in
        let off' = String.length data - len in
        let len = min (size - Buffer.length buf) len in
        if len > 0 then
          Buffer.add_substring buf data off' len
      )
      notes;
    (* We read the page because the names seem to be put before the section
       table but in the same page as the start. Thus we will need to keep the
       beginning of the page for when we extract the names. It's totally not
       clear to me if the names are always in the same page :/ *)
    let page_off = hdr.e_shoff land (lnot 0xFFFF) in
    if pos + String.length data >= page_off then begin
      let len =
        if pos < page_off then
          max 0 (pos + String.length data - page_off)
        else String.length data
      in
      let off = String.length data - len in
      if len > 0 then
        Buffer.add_substring buf data off len
    end;
    let pos = pos + String.length data in
    assert (pos < page_off || pos - Buffer.length buf = page_off);
    let read_section i =
      let off = hdr.e_shoff - page_off + i * hdr.e_shentsize in
      let sh_name_off = get_uint32 en buf off in
      let sh_offset = get_uint64 en buf (off + 24) in
      let sh_size = get_uint64 en buf (off+32) in
      { sh_name_off; sh_offset; sh_size; sh_name = "" }
    in
    if page_off + Buffer.length buf >= hdr.e_shoff + hdr.e_shnum * hdr.e_shentsize then
      let sections = Array.init hdr.e_shnum read_section in
      feed (Sections_without_name { hdr; notes; sections; pos; buf; en }) ""
    else
      Program_header_ok { hdr; notes; pos; buf; en; }
  | Sections_without_name { hdr; notes; sections; pos; buf; en } ->
    (*assert (List.for_all (fun { size; buf; _ } -> Buffer.length buf = size) notes);*)
    let shstrndx = sections.(hdr.e_shstrndx) in
    assert (pos >= shstrndx.sh_offset + shstrndx.sh_size);
    if pos - Buffer.length buf > shstrndx.sh_offset then
      Fail (`Msg "Oh no, we weren't able to gather section names!")
    else if pos < shstrndx.sh_offset + shstrndx.sh_size then
      Fail (`Msg "Oh no, we weren't able to gather section names!")
    else
      let sections =
        Array.map
          (fun section ->
             let page_off = hdr.e_shoff land (lnot 0xFFFF) in
             let off = shstrndx.sh_offset - page_off + section.sh_name_off
             and max_size = shstrndx.sh_size - section.sh_name_off in
             { section with sh_name = c_string buf off max_size })
          sections
      in
      begin match Array.find_opt (fun s -> String.equal s.sh_name section_manifest) sections,
                  Array.find_opt (fun s -> String.equal s.sh_name section_abi) sections with
      | Some mft_section, Some abi_section ->
        feed (Sections { notes; mft_section; abi_section; en }) ""
      | None, _ ->
        Fail (`Msg (Fmt.str "Did not find %s" section_manifest))
      | _, None ->
        Fail (`Msg (Fmt.str "Did not find %s" section_abi))
      end
  | Sections { notes; mft_section; abi_section; en } ->
    let desc en ({ buf; _ } as note) expected_type =
      let namesz = get_uint32 en buf 0
      and descsz = get_uint32 en buf 4
      and typ = get_uint32 en buf 8 in
      if typ <> expected_type then
        Error (`Msg "not expected type")
      else if String.length note_name + 1 <> namesz ||
         not (String.equal (note_name ^ "\000")
                (Buffer.sub buf 12 namesz)) then
        Error (`Msg "unexpected note name")
      else
        let off = 12 + namesz in
        (* padding *)
        let off = off + ((4 - ((note.off + off) land 3)) land 3) in
        Ok (Buffer.sub buf off descsz)
    in
    begin match List.find_opt (fun { off; _ } -> mft_section.sh_offset = off) notes,
                List.find_opt (fun { off; _ } -> abi_section.sh_offset = off) notes with
    | Some mft, Some abi ->
      assert (Buffer.length mft.buf = mft.size);
      assert (Buffer.length abi.buf = abi.size);
      assert (mft.off = mft_section.sh_offset);
      assert (abi.off = abi_section.sh_offset);
      let mft = desc en mft typ_mft1 in
      let abi = desc en abi typ_abi1 in
      (match mft, abi with
       | Ok mft, Ok abi ->
         Done { mft; abi }
       | Error `Msg e, _ | _, Error `Msg e ->
         Fail (`Msg ("Error getting note data: " ^ e)))
    | None, _ -> Fail (`Msg "Missing mft data")
    | _, None -> Fail (`Msg "Missing abi data")
    end
  | Done _  | Fail _ as donezo -> donezo
