(** An entry in the manifest representing a device. *)
type mft_entry =
  | Dev_block_basic of string
  | Dev_net_basic of string

(** The Solo5 manifest *)
type mft = {
  version : int;
  (** [version] is at the moment always 1. *)
  entries : mft_entry list;
  (** [entries] in the manifest. *)
}

(** The known solo5 targets *)
type abi_target =
  | Hvt
  | Spt
  | Virtio
  | Muen
  | Genode
  | Xen

(** abi taget and abi version *)
type abi = {
  target : abi_target; (** abi target *)
  version : int32; (** abi version *)
}

val pp_mft_entry : Format.formatter -> mft_entry -> unit
val pp_mft : Format.formatter -> mft -> unit
(** Pretty-prints the manifest as JSON in a similar style as the Solo5 command
 * line tool {[solo5-elftool query-manifest]}. *)

val pp_abi_target : Format.formatter -> abi_target -> unit
val pp_abi : Format.formatter -> abi -> unit
(** Pretty-prints the manifest as JSON in a similar style as the Solo5 command
 * line tool {[solo5-elftool query-abi]}. *)

val query_manifest : 'fd Cachet.t -> (mft, [> `Msg of string ]) result
(** [query_manifest cachet] is the solo5 manifest of [cachet], or an error message. *)

val query_abi : 'fd Cachet.t -> (abi, [> `Msg of string ]) result
(** [query_abi cachet] is the solo5 abi of [cachet], or an error message. *)
