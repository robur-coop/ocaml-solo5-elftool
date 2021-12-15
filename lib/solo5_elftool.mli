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

val pp_mft_entry : Format.formatter -> mft_entry -> unit
val pp_mft : Format.formatter -> mft -> unit
(** Pretty-prints the manifest as JSON in a similar style as the Solo5 command
 * line tool {[solo5-elftool query-manifest]}. *)

val query_manifest : Owee_buf.t -> (mft, [> `Msg of string ]) result
(** [query_manifest buf] is the solo5 manifest of [buf], or an error message.
 * @raise Owee_buf.Invalid_format If [buf] does not contain valid ELF format *)
