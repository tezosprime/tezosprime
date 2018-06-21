(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Hash
open Db
open Mathdata
open Logic
open Checking

type obligation = (payaddr * int64 * bool) option

type preasset =
  | Currency of int64
  | Bounty of int64
  | OwnsObj of hashval * payaddr * int64 option
  | OwnsProp of hashval * payaddr * int64 option
  | OwnsNegProp
  | RightsObj of hashval * int64
  | RightsProp of hashval * int64
  | Marker
  | DocPublication of payaddr * hashval * doc

val obligation_string : obligation -> string
val preasset_string : preasset -> string

(*** asset is (assetid,birthday,obligation,preasset) ***)
type asset = hashval * int64 * obligation * preasset

val assetid : asset -> hashval
val assetbday : asset -> int64
val assetobl : asset -> obligation
val assetpre : asset -> preasset

val hashobligation : obligation -> hashval option
val hashpreasset : preasset -> hashval
val hashasset : asset -> hashval

type addr_assetid = addr * hashval
type addr_preasset = addr * (obligation * preasset)
type addr_asset = addr * asset

val hash_addr_assetid : addr_assetid -> hashval
val hash_addr_preasset : addr_preasset -> hashval
val hash_addr_asset : addr_asset -> hashval

val new_assets : int64 -> addr -> addr_preasset list -> hashval -> int32 -> asset list
val remove_assets : asset list -> hashval list -> asset list
val get_spent : addr -> addr_assetid list -> hashval list
val add_vout : int64 -> hashval -> addr_preasset list -> int32 -> addr_asset list
val asset_value : int64 -> asset -> int64 option
val asset_value_sum : int64 -> asset list -> int64
val output_doc_uses_objs : addr_preasset list -> (hashval * hashval) list
val output_doc_uses_props : addr_preasset list -> (hashval * hashval) list
val output_creates_objs : addr_preasset list -> (hashval * hashval) list
val output_creates_props : addr_preasset list -> hashval list
val output_creates_neg_props : addr_preasset list -> hashval list
val rights_out_obj : addr_preasset list -> hashval -> int64
val rights_out_prop : addr_preasset list -> hashval -> int64
val count_obj_rights : asset list -> hashval -> int64
val count_prop_rights : asset list -> hashval -> int64
val count_rights_used : (hashval * hashval) list -> hashval -> int
val obj_rights_mentioned : addr_preasset list -> hashval list
val prop_rights_mentioned : addr_preasset list -> hashval list
val rights_mentioned : addr_preasset list -> hashval list
val units_sent_to_addr : addr -> addr_preasset list -> int64
val out_cost : addr_preasset list -> int64

val seo_obligation : (int -> int -> 'a -> 'a) -> obligation -> 'a -> 'a
val sei_obligation : (int -> 'a -> int * 'a) -> 'a -> obligation * 'a

val seo_preasset : (int -> int -> 'a -> 'a) -> preasset -> 'a -> 'a
val sei_preasset : (int -> 'a -> int * 'a) -> 'a -> preasset * 'a

val seo_asset : (int -> int -> 'a -> 'a) -> asset -> 'a -> 'a
val sei_asset : (int -> 'a -> int * 'a) -> 'a -> asset * 'a

val seo_addr_assetid : (int -> int -> 'a -> 'a) -> addr_assetid -> 'a -> 'a
val sei_addr_assetid : (int -> 'a -> int * 'a) -> 'a -> addr_assetid * 'a

val seo_addr_preasset : (int -> int -> 'a -> 'a) -> addr_preasset -> 'a -> 'a
val sei_addr_preasset : (int -> 'a -> int * 'a) -> 'a -> addr_preasset * 'a

val seo_addr_asset : (int -> int -> 'a -> 'a) -> addr_asset -> 'a -> 'a
val sei_addr_asset : (int -> 'a -> int * 'a) -> 'a -> addr_asset * 'a

module DbAsset :
    sig
      val dbinit : unit -> unit
      val dbget : hashval -> asset
      val dbexists : hashval -> bool
      val dbput : hashval -> asset -> unit
      val dbdelete : hashval -> unit
    end

module DbAssetIdAt :
    sig
      val dbinit : unit -> unit
      val dbget : hashval -> addr
      val dbexists : hashval -> bool
      val dbput : hashval -> addr -> unit
      val dbdelete : hashval -> unit
    end

val get_asset : hashval -> asset

val json_obligation : obligation -> jsonval option
val json_preasset : preasset -> jsonval
val json_asset : asset -> jsonval

val obligation_from_json : jsonval option -> obligation
val preasset_from_json : jsonval -> preasset
val asset_from_json : jsonval -> asset
