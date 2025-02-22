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

type abi_target =
  | Hvt
  | Spt
  | Virtio
  | Muen
  | Genode
  | Xen

type abi = {
  target : abi_target;
  version : int32;
}

let mft_type_of_int : int32 -> (mft_type, _) result = function
  | 1l -> Ok Dev_block_basic
  | 2l -> Ok Dev_net_basic
  | 1073741824l -> Ok Reserved_first
  | v -> Error (`Msg ("unknown manifest entry type: " ^ Int32.to_string v))

let abi_target_of_int : int32 -> (abi_target, _) result = function
  | 1l -> Ok Hvt
  | 2l -> Ok Spt
  | 3l -> Ok Virtio
  | 4l -> Ok Muen
  | 5l -> Ok Genode
  | 6l -> Ok Xen
  | v -> Error (`Msg ("unknown abi target: " ^ Int32.to_string v))

let pp_mft_entry ppf = function
  | Dev_block_basic name ->
    Fmt.pf ppf {|{@[<1>@ "name": %S,@ "type": "BLOCK_BASIC"@]@ }|} name
  | Dev_net_basic name ->
    Fmt.pf ppf {|{@[<1>@ "name": %S,@ "type": "NET_BASIC"@]@ }|} name

let pp_mft ppf { version; entries } =
  Fmt.pf ppf
    {|{@[<1>@ "type": "solo5.manifest",@ "version": %d,@ "devices": [@[<1>@ %a@]@ ]@]@ }|}
    version Fmt.(list ~sep:(append (any ",") sp) pp_mft_entry) entries

let pp_abi_target ppf = function
  | Hvt -> Format.fprintf ppf "hvt"
  | Spt -> Format.fprintf ppf "spt"
  | Virtio -> Format.fprintf ppf "virtio"
  | Muen -> Format.fprintf ppf "muen"
  | Genode -> Format.fprintf ppf "genode"
  | Xen -> Format.fprintf ppf "xen"

let pp_abi ppf { version; target } =
  Fmt.pf ppf
    {|{@[<1>@ "type": "solo5.abi",@ "target": "%a",@ "version": %lu@ @]@ }|}
    pp_abi_target target version

let ( let* ) = Result.bind
let guard m b = if not b then Error (`Msg m) else Ok ()

let sizeof_mft_entry = 104
let mft_max_entries = 64l

let parse_mft_entry s =
  (* invariant: Cstruct.length buf = sizeof_mft_entry *)
  let name_raw = String.sub s 0 68 in
  let typ = String.get_int32_le s 68 in
  let u = String.sub s 72 16 in
  let b = String.sub s 88 8 in
  let attached = String.get_uint8 s 96 <> 0 in
  let* name =
    String.index_opt name_raw '\000'
    |> Option.map (fun idx -> String.sub name_raw 0 idx)
    |> Option.to_result ~none:(`Msg "unterminated device name")
  in
  let* () = guard "non-zero mft_entry.u" (String.for_all ((=) '\000') u) in
  let* () = guard "non-zero mft_entry.b" (String.for_all ((=) '\000') b) in
  let* () = guard "non-zero mft_entry.attached" (not attached) in
  let* typ = mft_type_of_int typ in
  match typ with
  | Reserved_first ->
    let* () = guard "non-zero RESERVED_FIRST" (String.for_all ((=) '\000') name_raw) in
    Ok `Reserved_first
  | Dev_block_basic ->
    Ok (`Dev_block_basic name)
  | Dev_net_basic ->
    Ok (`Dev_net_basic name)

let parse_mft s =
  let* () = guard "manifest too small"
      (String.length s >= 4 + 8 + sizeof_mft_entry)
  in
  (* Solo5 defines a struct mft1_note consisting of the ELF note header
   * followed by a struct mft for reading and writing the ELF note. The note
   * header is 20 bytes long, so to get 8-byte alignment the note header is
   * padded with 4 bytes. See {[solo5/mft_abi.h]}. *)
  let version = String.get_int32_le s 4
  and entries = String.get_int32_le s 8
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
  let off = 12 in
  let* () = guard "unexpected note size"
      (String.length s = entries * sizeof_mft_entry + 12)
  in
  let* () =
    match parse_mft_entry (String.sub s off sizeof_mft_entry) with
    | Ok `Reserved_first -> Ok ()
    | _ -> Error (`Msg "expected RESERVED_FIRST")
  in
  let off = off + sizeof_mft_entry in
  let entries =
    Array.init (entries - 1)
      (fun i -> String.sub s (off + i * sizeof_mft_entry) sizeof_mft_entry)
  in
  let* entries =
    Array.fold_left
      (fun r s ->
         let* acc = r in
         let* mft_entry = parse_mft_entry s in
         match mft_entry with
         | `Dev_block_basic name -> Ok (Dev_block_basic name :: acc)
         | `Dev_net_basic name -> Ok (Dev_net_basic name :: acc)
         | `Reserved_first -> Error (`Msg "found RESERVED_FIRST not as first entry"))
      (Ok [])
      entries
    |> Result.map List.rev
  in
  Ok { version = Int32.to_int version; entries }

let parse_abi s =
  let* () = guard "abi manifest size mismatch" (String.length s = 4 * 4) in
  let target = String.get_int32_le s 0 in
  let version = String.get_int32_le s 4 in
  let reserved0 = String.get_int32_le s 8 in
  let reserved1 = String.get_int32_le s 12 in
  let* target = abi_target_of_int target in
  let* () = guard "non-zero reserved0" (reserved0 = 0l) in
  let* () = guard "non-zero reserved1" (reserved1 = 0l) in
  (* XXX: should we check version = 1l ? *)
  Ok { target; version }

let query_manifest c =
  match Elf.find c Elf.section_manifest Elf.typ_mft1 with
  | None -> Error (`Msg "manifest not found")
  | Some desc -> parse_mft desc
  | exception Elf.Elf_error | exception Cachet.Out_of_bounds _ ->
    Error (`Msg "error during ELF parsing")

let query_abi c =
  match Elf.find c Elf.section_abi Elf.typ_abi1 with
  | None -> Error (`Msg "manifest not found")
  | Some desc -> parse_abi desc
  | exception Elf.Elf_error | exception Cachet.Out_of_bounds _ ->
    Error (`Msg "error during ELF parsing")
