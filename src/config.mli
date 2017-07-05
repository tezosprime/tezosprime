(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

val datadir : string ref
val daemon : bool ref
val testnet : bool ref
val staking : bool ref
val ip : string option ref
val ipv6 : bool ref
val port : int ref
val socks : int option ref
val socksport : int ref
val rpcport : int ref
val maxconns : int ref
val lastcheckpoint : string ref
val prompt : string ref
val genesistimestamp : int64 ref
val maxburn : int64 ref
val ltctxfee : int64 ref
val mintimebetweenburns : int64 ref
val burnifleq : int ref
val seed : string ref
val randomseed : string option ref
val checkpointskey : string option ref
