(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Logic
open Hash
open Mathdata

val check_theoryspec : theoryspec -> (theory * gsign) option

val check_signaspec :
  (hashval option -> hashval -> stp -> bool) -> (hashval option -> hashval ->
  bool) -> hashval option -> theory -> stree option -> signaspec ->
  (gsign * hashval list) option

val check_doc :
  (hashval option -> hashval -> stp -> bool) -> (hashval option -> hashval ->
  bool) -> hashval option -> theory -> stree option -> doc ->
  (gsign * hashval list) option
