(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Logic
open Hash
open Mathdata

val check_doc :
  (hashval -> stp -> bool) -> (hashval -> bool) -> doc ->
  gsign option
