(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash

val ltc_getbestblockhash : unit -> string
val ltc_getblock : string -> string * int64 * string list
val ltc_gettransactioninfo : string -> int64 * hashval * hashval
val ltc_listunspent : unit -> (string * int * string * string * int64) list

exception InsufficientLtcFunds
val ltc_createburntx : hashval -> hashval -> int64 -> string
val ltc_signrawtransaction : string -> string
val ltc_sendrawtransaction : string -> string


