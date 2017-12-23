(* Copyright (c) 2015-2017 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Setconfig;;

datadir_from_command_line();; (*** if -datadir=... is on the command line, then set Config.datadir so we can find the config file ***)
process_config_file();;
process_config_args();; (*** settings on the command line shadow those in the config file ***)

let a = Array.length Sys.argv;;
let c = Sys.argv.(a-1);;

let ia = Unix.inet_addr_of_string "127.0.0.1";;
let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0;;
Unix.connect s (Unix.ADDR_INET(ia,!Config.rpcport));;
let sin = Unix.in_channel_of_descr s;;
let sout = Unix.out_channel_of_descr s;;
Printf.fprintf sout "%s\n%s\n%s\n" !Config.rpcuser !Config.rpcpass c;;
flush sout;;
try
  while true do
    let l = input_line sin in Printf.printf "%s\n" l; flush stdout
  done 
with End_of_file -> Unix.close s;;
