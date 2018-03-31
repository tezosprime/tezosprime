(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int

type jsonval =
  | JsonStr of string
  | JsonNum of string (*** do not support e ***)
  | JsonObj of (string * jsonval) list
  | JsonArr of jsonval list
  | JsonBool of bool
  | JsonNull

val print_jsonval : out_channel -> jsonval -> unit

exception JsonParseFail of int * string

val parse_jsonval_start : string * int -> jsonval * int
val parse_jsonval : string -> jsonval * int

val bool_from_json : jsonval -> bool
val int_from_json : jsonval -> int
val int32_from_json : jsonval -> int32
val int64_from_json : jsonval -> int64
val big_int_from_json : jsonval -> big_int

