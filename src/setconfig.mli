(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash

val datadir_from_command_line : unit -> unit
val process_config_file : unit -> unit
val process_config_args : unit -> unit

val createsnapshot : bool ref
val importsnapshot : bool ref
val snapshot_dir : string option ref
val snapshot_headers : hashval list ref
val snapshot_blocks : hashval list ref
val snapshot_ledgerroots : hashval list ref
val snapshot_full : bool ref
val snapshot_addresses : addr list ref
