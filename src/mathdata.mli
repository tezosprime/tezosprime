(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Copyright (c) 2018 The Tezos' (Tezos Prime) developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Hash
open Db
open Logic
open Htree

(** ** pdoc: partical doc, approximating a doc with enough information to compute the hashroot **)
type pdoc =
  | PDocNil
  | PDocHash of hashval
  | PDocParam of hashval * stp * pdoc
  | PDocParamHash of hashval * pdoc
  | PDocDef of stp * trm * pdoc
  | PDocDefHash of hashval * pdoc
  | PDocKnown of trm * pdoc
  | PDocConj of trm * pdoc
  | PDocPfOf of trm * pf * pdoc
  | PDocPfOfHash of hashval * pdoc

(** * serialization code ***)

val seo_tp : (int -> int -> 'a -> 'a) -> stp -> 'a -> 'a
val sei_tp : (int -> 'a -> int * 'a) -> 'a -> stp * 'a

val hashtp : stp -> hashval

val seo_tm : (int -> int -> 'a -> 'a) -> trm -> 'a -> 'a
val sei_tm : (int -> 'a -> int * 'a) -> 'a -> trm * 'a

val hashtm : trm -> hashval
val tm_hashroot : trm -> hashval

val seo_pf : (int -> int -> 'a -> 'a) -> pf -> 'a -> 'a
val sei_pf : (int -> 'a -> int * 'a) -> 'a -> pf * 'a

val hashpf : pf -> hashval
val pf_hashroot : pf -> hashval

val seo_doc : (int -> int -> 'a -> 'a) -> doc -> 'a -> 'a
val sei_doc : (int -> 'a -> int * 'a) -> 'a -> doc * 'a
val seo_pdoc : (int -> int -> 'a -> 'a) -> pdoc -> 'a -> 'a
val sei_pdoc : (int -> 'a -> int * 'a) -> 'a -> pdoc * 'a

val hashdoc : doc -> hashval
val doc_hashroot : doc -> hashval

val hashpdoc : pdoc -> hashval
val pdoc_hashroot : pdoc -> hashval

val doc_uses_objs : doc -> (hashval * hashval) list
val doc_uses_props : doc -> hashval list
val doc_creates_objs : doc -> (hashval * hashval) list
val doc_creates_props : doc -> hashval list
val doc_creates_neg_props : doc -> hashval list

exception CheckingFailure
exception NotKnown of hashval option * hashval
exception UnknownTerm of hashval option * hashval * stp
exception UnknownSigna of hashval
exception NonNormalTerm
exception BetaLimit
exception TermLimit

val print_trm : int -> stp list -> trm -> unit
val print_tp : int -> stp -> unit

val json_doc : hashval option -> doc -> jsonval

val stp_from_json : jsonval -> stp
val trm_from_json : jsonval -> trm
val doc_from_json : jsonval -> doc
