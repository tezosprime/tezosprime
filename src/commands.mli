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

val walletkeys : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletendorsements : (payaddr * payaddr * (big_int * big_int) * int * bool * signat) list ref
val walletwatchaddrs : addr list ref
val stakingassets : (p2pkhaddr * hashval * int64 * obligation * int64) list ref
val storagetrmassets : (hashval option * trm * stp * hashval * hashval) list ref
val storagedocassets : (pubaddr * hashval * hashval option * pdoc * hashval * hashval) list ref

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
val importprivkey : string -> unit
val importbtcprivkey : string -> unit
val importendorsement : string -> string -> string -> unit
val importwatchaddr : string -> unit
val importwatchbtcaddr : string -> unit
val generate_newkeyandaddress : hashval -> big_int * p2pkhaddr

val createtx : jsonval -> jsonval -> tx
val createsplitlocktx : hashval -> payaddr -> payaddr -> addr -> hashval -> int -> int64 -> int64 -> unit

val signtx : out_channel -> hashval -> string -> unit
val savetxtopool : int64 -> hashval -> string -> unit
val sendtx : out_channel -> int64 -> hashval -> string -> unit

val query : string -> jsonval
