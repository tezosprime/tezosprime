(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Json
open Hash
open Db
open Mathdata
open Assets
open Script

type tx = addr_assetid list * addr_preasset list

val hashtx : tx -> hashval
val tx_inputs : tx -> addr_assetid list
val tx_outputs : tx -> addr_preasset list

val no_dups : 'a list -> bool
val tx_inputs_valid : addr_assetid list -> bool
val tx_outputs_valid : addr_preasset list -> bool
val tx_valid : tx -> bool
val tx_valid_oc : out_channel -> tx -> bool

type gensignat_or_ref = GenSignatReal of gensignat | GenSignatRef of int
type txsigs = gensignat_or_ref option list * gensignat_or_ref option list
type stx = tx * txsigs

exception BadOrMissingSignature

val check_spend_obligation_upto_blkh : addr -> big_int -> gensignat -> obligation -> int64 option
val check_spend_obligation : addr -> int64 -> big_int -> gensignat -> obligation -> bool
val check_move_obligation : addr -> big_int -> gensignat -> obligation -> preasset -> addr_preasset list -> bool
val tx_signatures_valid : int64 -> asset list -> stx -> bool
val tx_signatures_valid_asof_blkh : asset list -> stx -> int64 option

val seo_tx : (int -> int -> 'a -> 'a) -> tx -> 'a -> 'a
val sei_tx : (int -> 'a -> int * 'a) -> 'a -> tx * 'a
val seo_txsigs : (int -> int -> 'a -> 'a) -> gensignat_or_ref option list * gensignat_or_ref option list -> 'a -> 'a
val sei_txsigs : (int -> 'a -> int * 'a) -> 'a -> (gensignat_or_ref option list * gensignat_or_ref option list) * 'a
val seo_stx : (int -> int -> 'a -> 'a) -> stx -> 'a -> 'a
val sei_stx : (int -> 'a -> int * 'a) -> 'a -> stx * 'a

val hashtxsigs : txsigs -> hashval

val hashstx : stx -> hashval

module DbSTx :
    sig
      val dbinit : unit -> unit
      val dbget : Hash.hashval -> stx
      val dbexists : Hash.hashval -> bool
      val dbput : Hash.hashval -> stx -> unit
      val dbdelete : Hash.hashval -> unit
    end

val json_tx : tx -> jsonval
val json_txsigs : txsigs -> jsonval
val json_stx : stx -> jsonval

val tx_from_json : jsonval -> tx
