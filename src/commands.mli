(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Big_int
open Hash
open Mathdata
open Assets
open Signat
open Tx
open Ctre
open Logic

(***
val addnode : string -> int -> bool
***)

val walletkeys_staking : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletkeys_nonstaking : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletkeys_staking_fresh : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletkeys_nonstaking_fresh : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletendorsements : (payaddr * payaddr * (big_int * big_int) * int * bool * signat) list ref
val walletwatchaddrs : addr list ref
val walletwatchaddrs_offlinekey : addr list ref
val walletwatchaddrs_offlinekey_fresh : addr list ref
val stakingassets : (p2pkhaddr * hashval * int64 * obligation * int64) list ref

val get_cants_balances_in_ledger : out_channel -> hashval -> int64 * int64 * int64 * int64

val load_txpool : unit -> unit
val save_txpool : unit -> unit
val load_wallet : unit -> unit
val save_wallet : unit -> unit

val printassets : out_channel -> unit
val printassets_in_ledger : out_channel -> hashval -> unit
val printctreeinfo : hashval -> unit
val printctreeelt : hashval -> unit
val printhconselt : hashval -> unit
val printasset : hashval -> unit
val printtx : hashval -> unit

val btctodaliladdr : string -> unit
val importprivkey : out_channel -> string -> string -> unit
val importbtcprivkey : out_channel -> string -> string -> unit
val importendorsement : string -> string -> string -> unit
val importwatchaddr : out_channel -> string -> string -> unit
val importwatchbtcaddr : out_channel -> string -> string -> unit
val generate_newkeyandaddress : hashval -> string -> big_int * p2pkhaddr
val get_fresh_offline_address : out_channel -> addr

val reclassify_staking : out_channel -> string -> bool -> unit

val createtx : jsonval -> jsonval -> tx
val createsplitlocktx : hashval -> payaddr -> payaddr -> addr -> hashval -> int -> int64 -> int64 -> unit

val signtx : out_channel -> hashval -> string -> unit
val savetxtopool : int64 -> hashval -> string -> unit
val validatetx : out_channel -> int64 -> hashval option -> hashval option -> hashval -> string -> unit
val sendtx : out_channel -> int64 -> hashval option -> hashval option -> hashval -> string -> unit

val query_at_block : string -> (hashval * Block.poburn) option -> hashval -> int64 -> jsonval
val query : string -> jsonval
val query_blockheight : int64 -> jsonval

val preassetinfo_report : out_channel -> preasset -> unit
