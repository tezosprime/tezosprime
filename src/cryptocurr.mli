(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Copyright (c) 2018 The Tezos' (Tezos Prime) developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

(* Most of this code is taken directly from Egal. *)

open Big_int
open Json
open Secp256k1
open Hash

val base58 : big_int -> string
val frombase58 : string -> big_int
val dalilwif : big_int -> bool -> string
val privkey_from_wif : string -> big_int * bool
val privkey_from_btcwif : string -> big_int * bool
val pubkey_hashval : big_int * big_int -> bool -> hashval
val pubkey_md160 : big_int * big_int -> bool -> md160
val md160_from_addrstr : string -> md160
val md160_btcaddrstr : md160 -> string
val addr_daliladdrstr : addr -> string
val daliladdrstr_addr : string -> addr
val btcaddrstr_addr : string -> addr
val tezzies_of_cants : int64 -> string
val cants_of_tezzies : string -> int64
val ltc_of_litoshis : int64 -> string
val litoshis_of_ltc : string -> int64

val addr_from_json : jsonval -> addr
val payaddr_from_json : jsonval -> payaddr
val cants_from_json : jsonval -> int64
val tezzies_from_json : jsonval -> string
val json_tezzies : string -> jsonval
val json_cants : int64 -> jsonval

