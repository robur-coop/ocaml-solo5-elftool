type mft_entry =
  | Dev_block_basic of {
      name : string;
    }
  | Dev_net_basic of {
      name : string;
    }

type mft = {
  version : int;
  entries : mft_entry list;
}

val pp_mft_entry : Format.formatter -> mft_entry -> unit
val pp_mft : Format.formatter -> mft -> unit

val query_manifest : Owee_buf.t -> (mft, [> `Msg of string ]) result
