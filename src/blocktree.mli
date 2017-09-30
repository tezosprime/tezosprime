(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Hash
open Net
open Signat
open Ltcrpc
open Tx
open Ctre
open Block

val checkpointsprivkeyk : big_int option ref
val checkpointspubkeyx : big_int
val checkpointspubkeyy : big_int

val stxpool : (hashval,stx) Hashtbl.t

type validationstatus = Waiting of float * (blockdelta * connstate) option | ValidBlock | InvalidBlock

type blocktree = BlocktreeNode of blocktree option * p2pkhaddr list ref * (hashval * hashval * poburn) option * hashval option * hashval option * hashval * stakemod * targetinfo * int64 * big_int * int64 * validationstatus ref * bool ref * (hashval * blocktree) list ref

val genesisblocktreenode : blocktree ref
val lastcheckpointnode : blocktree ref
val blkheadernode : (hashval option,blocktree) Hashtbl.t
val bestnode : blocktree ref
val update_bestnode : blocktree -> unit
val initblocktree : unit -> unit
val node_recent_stakers : blocktree -> p2pkhaddr list
val node_prevblockhash : blocktree -> (hashval * hashval * poburn) option
val node_theoryroot : blocktree -> hashval option
val node_signaroot : blocktree -> hashval option
val node_ledgerroot : blocktree -> hashval
val node_stakemod : blocktree -> stakemod
val node_targetinfo : blocktree -> targetinfo
val node_timestamp : blocktree -> int64
val node_cumulstk : blocktree -> big_int
val node_blockheight : blocktree -> int64
val node_validationstatus : blocktree -> validationstatus
val node_children_ref : blocktree -> (hashval * blocktree) list ref
val eq_node : blocktree -> blocktree -> bool
val find_best_validated_block_from : blocktree -> blocktree -> big_int -> blocktree * big_int
val find_best_validated_block : unit -> unit
val is_recent_staker : p2pkhaddr -> blocktree -> int -> bool
val record_recent_staker : p2pkhaddr -> blocktree -> int -> unit

val print_best_node : unit -> unit

val lookup_thytree : hashval option -> Mathdata.ttree option
val lookup_sigtree : hashval option -> Mathdata.stree option

val publish_stx : hashval -> stx -> unit
val publish_block : int64 -> hashval -> block -> big_int -> unit

val send_inv : int -> out_channel -> connstate -> unit

val dumpblocktreestate : out_channel -> unit

val create_new_node : hashval -> bool -> blocktree
val ltc_best_chaintips : unit -> hashval list list
val get_bestnode : bool -> blocktree
