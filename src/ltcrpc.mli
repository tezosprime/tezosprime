(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash
open Sha256
open Block

val ltctestnet : unit -> unit

type ltcdacstatus = LtcDacStatusPrev of hashval | LtcDacStatusNew of (hashval * hashval * hashval * int64 * int64) list list

module DbLtcDacStatus :
    sig
      val dbinit : unit -> unit
      val dbget : hashval -> ltcdacstatus
      val dbexists : hashval -> bool
      val dbput : hashval -> ltcdacstatus -> unit
      val dbdelete : hashval -> unit
    end

val ltcdacstatus_dbget : hashval -> hashval * ((hashval * hashval * hashval * int64 * int64) list list)

module DbHeaderLtcBurn :
    sig
      val dbinit : unit -> unit
      val dbget : hashval -> poburn * hashval option * int64
      val dbexists : hashval -> bool
      val dbput : hashval -> poburn * hashval option * int64 -> unit
      val dbdelete : hashval -> unit
    end

module DbLtcBurnTx :
    sig
      val dbinit : unit -> unit
      val dbget : hashval -> int64 * hashval * hashval
      val dbexists : hashval -> bool
      val dbput : hashval -> int64 * hashval * hashval -> unit
      val dbdelete : hashval -> unit
    end

module DbLtcBlock :
    sig
      val dbinit : unit -> unit
      val dbget : hashval -> hashval * int64 * int64 * hashval list
      val dbexists : hashval -> bool
      val dbput : hashval -> hashval * int64 * int64 * hashval list -> unit
      val dbdelete : hashval -> unit
    end

val ltc_getbestblockhash : unit -> string
val ltc_getblock : string -> string * int64 * int64 * string list
val ltc_gettransactioninfo : string -> int64 * hashval * hashval * string option * int option
val ltc_listunspent : unit -> (string * int * string * string * int64) list

exception InsufficientLtcFunds
val ltc_createburntx : hashval -> hashval -> int64 -> string
val ltc_signrawtransaction : string -> string
val ltc_sendrawtransaction : string -> string

val ltc_process_block : string -> unit

val ltc_bestblock : hashval ref

val ltc_medtime : unit -> int64

val ltc_synced : unit -> bool

val possibly_request_dalilcoin_block : hashval -> unit

val ltc_tx_confirmed : string -> bool

val ltc_tx_poburn : string -> poburn
