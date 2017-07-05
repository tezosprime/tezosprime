(* Copyright (c) 2017 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

type jsonval =
  | JsonStr of string
  | JsonNum of string (*** do not support e ***)
  | JsonObj of (string * jsonval) list
  | JsonArr of jsonval list
  | JsonBool of bool
  | JsonNull

val print_jsonval : out_channel -> jsonval -> unit

val parse_jsonval_start : string * int -> jsonval * int
val parse_jsonval : string -> jsonval * int

