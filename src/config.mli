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
val rpcuser : string ref
val rpcpass : string ref
val rpcport : int ref
val ltcrpcport : int ref
val ltcrpcuser : string ref
val ltcrpcpass : string ref
val ltcnotifyport : int ref
val ltcaddresses : string list ref
val curl : string ref
val maxconns : int ref
val lastcheckpoint : string ref
val ltcblockcheckpoint : string ref
val prompt : string ref
val genesistimestamp : int64 ref
val maxburn : int64 ref
val maxburnrate : int64 ref
val ltctxfee : int64 ref
val mintimebetweenburns : int64 ref
val burnifleq : int ref
val seed : string ref
val randomseed : string option ref
val minconnstostake : int ref
val minrelayfee : int64 ref
val extraindex : bool ref
val offlinestakerewardsdest : string option ref
val offlinestakerewardslock : string option ref
val generatenewstakingaddresses : bool ref
val reward_lock_relative : int64 option ref
val reward_lock_absolute : int64 option ref
