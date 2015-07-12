(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash
open Big_int
open Mathdata
open Assets
open Signat
open Tx
open Ctre
open Ctregraft

type stakemod = int64 * int64 * int64 * int64

val set_genesis_stakemods : string -> unit
val genesiscurrentstakemod : stakemod ref
val genesisfuturestakemod : stakemod ref
val genesisledgerroot : hashval ref
val genesistarget : big_int ref

val stakemod_pushbit : bool -> stakemod -> stakemod
val stakemod_lastbit : stakemod -> bool
val stakemod_firstbit : stakemod -> bool

type targetinfo = stakemod * stakemod * big_int

val hitval : int32 -> hashval -> stakemod -> big_int

type postor =
  | PostorTrm of hashval option * tm * tp * hashval
  | PostorDoc of payaddr * hashval * hashval option * pdoc * hashval

type blockheaderdata = {
    prevblockhash : hashval option;
    newtheoryroot : hashval option;
    newsignaroot : hashval option;
    newledgerroot : hashval;
    stake : int64;
    stakeaddr : p2pkhaddr;
    stakeassetid : hashval;
    stored : postor option;
    deltatime : int32;
    tinfo : targetinfo;
    prevledger : ctree;
  }

type blockheadersig = {
    blocksignat : signat;
    blocksignatrecid : int;
    blocksignatfcomp : bool;
  }

type blockheader = blockheaderdata * blockheadersig

type blockdelta = {
    stakeoutput : addr_preasset list;
    totalfees : int64;
    prevledgergraft : cgraft;
    blockdelta_stxl : stx list
  }

type block = blockheader * blockdelta

val coinstake : block -> tx

val check_hit : int64 -> blockheaderdata -> bool

val hash_blockheaderdata : blockheaderdata -> hashval

val valid_blockheader : int64 -> blockheader -> bool

val ctree_of_block : block -> ctree

val tx_of_block : block -> tx

val retarget : big_int -> int32 -> big_int

val valid_block : ttree option -> stree option -> int64 -> block -> bool

val blockheader_succ : blockheader -> blockheader -> bool

type blockchain = block * block list
type blockheaderchain = blockheader * blockheader list

val blockchain_headers : blockchain -> blockheaderchain

val ledgerroot_of_blockchain : blockchain -> hashval

val valid_blockchain : int64 -> blockchain -> bool

val valid_blockheaderchain : int64 -> blockheaderchain -> bool
