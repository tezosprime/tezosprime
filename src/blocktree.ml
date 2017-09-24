(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Utils
open Ser
open Hashaux
open Sha256
open Hash
open Net
open Db
open Assets
open Signat
open Tx
open Ctre
open Block
open Ltcrpc

let checkpointsprivkeyk = ref None
let checkpointspubkeyx = big_int_of_string "63717203732691002966627493523478395510925637964138185199101530429614494608215"
let checkpointspubkeyy = big_int_of_string "14551538993109352432558649646564074001010059416911525032428212764782058901234"

let checkpoints : (hashval,int64 * signat) Hashtbl.t = Hashtbl.create 1000;;

let stxpool : (hashval,stx) Hashtbl.t = Hashtbl.create 1000;;
let published_stx : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;
let thytree : (hashval,Mathdata.ttree) Hashtbl.t = Hashtbl.create 1000;;
let sigtree : (hashval,Mathdata.stree) Hashtbl.t = Hashtbl.create 1000;;

type validationstatus = Waiting of float * (blockdelta * connstate) option | ValidBlock | InvalidBlock (*** It isn't clear if there are any circumstances when it is safe to say the header is not the header for a valid block. InvalidBlock may be unused. ***)

type blocktree = BlocktreeNode of blocktree option * p2pkhaddr list ref * (hashval * hashval) option * hashval option * hashval option * hashval * stakemod * targetinfo * int64 * big_int * int64 * validationstatus ref * bool ref * (hashval * blocktree) list ref

let genesisblocktreenode = ref (BlocktreeNode(None,ref [],None,None,None,!genesisledgerroot,!genesisstakemod,!genesistarget,!Config.genesistimestamp,zero_big_int,1L,ref ValidBlock,ref false,ref []));;

let lastcheckpointnode = ref !genesisblocktreenode;;

let bestnode = ref !genesisblocktreenode;;

let node_recent_stakers n =
  let BlocktreeNode(_,rs,_,_,_,_,_,_,_,_,_,_,_,_) = n in
  !rs

let node_prevblockhash n =
  let BlocktreeNode(_,_,pbh,_,_,_,_,_,_,_,_,_,_,_) = n in
  pbh

let node_theoryroot n =
  let BlocktreeNode(_,_,_,tr,_,_,_,_,_,_,_,_,_,_) = n in
  tr

let node_signaroot n =
  let BlocktreeNode(_,_,_,_,sr,_,_,_,_,_,_,_,_,_) = n in
  sr

let node_ledgerroot n =
  let BlocktreeNode(_,_,_,_,_,lr,_,_,_,_,_,_,_,_) = n in
  lr

let node_stakemod n =
  let BlocktreeNode(_,_,_,_,_,_,sm,_,_,_,_,_,_,_) = n in
  sm

let node_targetinfo n =
  let BlocktreeNode(_,_,_,_,_,_,_,ti,_,_,_,_,_,_) = n in
  ti

let node_timestamp n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,tm,_,_,_,_,_) = n in
  tm

let node_cumulstk n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,cs,_,_,_,_) = n in
  cs

let node_blockheight n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,blkh,_,_,_) = n in
  blkh

let node_validationstatus n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,vs,_,_) = n in
  !vs

let node_children_ref n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,_,_,chr) = n in
  chr

let print_best_node () =
  let BlocktreeNode(_,_,_,pbh,_,_,_,_,_,_,_,_,_,_) = !bestnode in
  match pbh with
  | Some(h) -> Printf.fprintf !log "bestnode pbh %s\n" (hashval_hexstring h); flush !log
  | None -> Printf.fprintf !log "bestnode pbh (genesis)\n"; flush !log

let eq_node n1 n2 = node_prevblockhash n1 = node_prevblockhash n2

let blkheaders : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;
let blkheadernode : (hashval option,blocktree) Hashtbl.t = Hashtbl.create 1000;;
let orphanblkheaders : (hashval option,hashval * blockheader) Hashtbl.t = Hashtbl.create 1000;;
let earlyblocktreenodes : (int64 * blocktree) list ref = ref [];;
let tovalidate : (hashval,unit) Hashtbl.t = Hashtbl.create 100;;

let known_thytree_p thyroot =
  match thyroot with
  | None -> true
  | Some(r) -> Hashtbl.mem thytree r

let known_sigtree_p sigroot =
  match sigroot with
  | None -> true
  | Some(r) -> Hashtbl.mem sigtree r

let lookup_thytree thyroot =
  match thyroot with
  | None -> None
  | Some(r) -> Some(Hashtbl.find thytree r)

let lookup_sigtree sigroot =
  match sigroot with
  | None -> None
  | Some(r) -> Some(Hashtbl.find sigtree r)

let add_thytree thyroot otht =
  match thyroot,otht with
  | Some(r),Some(tht) -> if not (Hashtbl.mem thytree r) then Hashtbl.add thytree r tht
  | _,_ -> ()

let add_sigtree sigroot osigt =
  match sigroot,osigt with
  | Some(r),Some(sigt) -> if not (Hashtbl.mem sigtree r) then Hashtbl.add sigtree r sigt
  | _,_ -> ()

let rec collect_inv m cnt tosend n txinv =
  if !cnt < m then
    if !cnt mod 5 = 0 && not (txinv = []) then
      begin
	match txinv with
	| (txid::txinvr) ->
	    tosend := (int_of_msgtype STx,0L,txid)::!tosend; incr cnt;
	    collect_inv m cnt tosend n txinvr
	| [] -> incr cnt; collect_inv m cnt tosend n [] (*** can never happen ***)
      end
    else
      let BlocktreeNode(par,_,pbh,_,_,_,_,_,_,_,blkh,_,_,_) = n in
      match pbh with
      | None -> ()
      | Some(pbh,_) ->
	  if DbBlockHeaderData.dbexists pbh && DbBlockHeaderSig.dbexists pbh then
	    begin
	      tosend := (int_of_msgtype Headers,Int64.sub blkh 1L,pbh)::!tosend; incr cnt;
	      if DbBlockDelta.dbexists pbh then (tosend := (int_of_msgtype Blockdelta,blkh,pbh)::!tosend; incr cnt)
	    end;
	  match par with
	  | None -> ()
	  | Some(p) -> collect_inv m cnt tosend p txinv

let send_inv m sout cs =
  let cnt = ref 0 in
  let tosend = ref [] in
  let txinv = ref [] in
  Hashtbl.iter (fun k _ -> txinv := k::!txinv) stxpool;
  collect_inv m cnt tosend !bestnode !txinv;
  let invmsg = Buffer.create 10000 in
  let c = ref (seo_int32 seosb (Int32.of_int !cnt) (invmsg,None)) in
  List.iter
    (fun (i,blkh,h) ->
      let cn = seo_prod3 seo_int8 seo_int64 seo_hashval seosb (i,blkh,h) !c in
      c := cn)
    !tosend;
  ignore (queue_msg cs Inv (Buffer.contents invmsg));;

send_inv_fn := send_inv;;

let rec insertnewdelayed (tm,n) btnl =
  match btnl with
  | [] -> [(tm,n)]
  | (tm2,n2)::btnr when tm < tm2 -> (tm,n)::btnl
  | (tm2,n2)::btnr -> (tm2,n2)::insertnewdelayed (tm,n) btnr

let process_new_tx h hh =
  try
    let tx1 = DbTx.dbget h in
    let txid = hashtx tx1 in
    if not (txid = h) then (*** wrong hash, remove it but don't blacklist the (wrong) hashval ***)
      begin
        Printf.fprintf !log "WARNING: Received tx with different hash as advertised, removing %s\nThis may be due to a bug or due to a misbehaving peer.\n" hh; flush !log;
	DbTx.dbdelete h;
	DbTxSignatures.dbdelete h;
      end
    else if tx_valid tx1 then
      begin (*** checking the validity of signatures and support depend on the current ledger; delay this here in favor of checking them before including them in a block we're actively trying to stake; note that the relevant properties are checked when they are included in a block as part of checking a block is valid ***)
	let txsigs1 = DbTxSignatures.dbget h in
        Hashtbl.add stxpool txid (tx1,txsigs1);
      end
    else
      (*** in this case, reject the tx since it's definitely not valid ***)
     begin
       DbBlacklist.dbput h true;
       DbTx.dbdelete h;
       DbTxSignatures.dbdelete h;
     end
  with (*** in some cases, failure should lead to blacklist and removal of the tx, but it's not clear which cases; if it's in a block we might need to distinguish between definitely incorrect vs. possibly incorrect ***)
  | Not_found ->
      Printf.fprintf !log "Problem with tx, deleting it\n"; flush !log;
      DbTx.dbdelete h;
      DbTxSignatures.dbdelete h;
  | e ->
      Printf.fprintf !log "exception %s\n" (Printexc.to_string e); flush !log;
      ()

let rec prev_nth_node i n =
  if i <= 0 then
    Some(n)
  else
    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,_,_,_) = n in
    match par with
    | None -> None
    | Some(p) -> prev_nth_node (i-1) p

let update_bestnode n =
  Printf.fprintf !log "New best blockheader %s\n" (match node_prevblockhash n with Some(h,_) -> hashval_hexstring h | None -> "(genesis)"); flush !log;
  bestnode := n;
  netblkh := node_blockheight n

let rec processdelayednodes tm btnl =
  match btnl with
  | (tm2,n2)::btnr when tm2 <= tm ->
    let BlocktreeNode(_,_,_,_,_,_,_,_,_,bestcumulstk,_,_,_,_) = !bestnode in
    let BlocktreeNode(_,_,pbh,_,_,_,_,_,_,newcumulstk,_,_,_,_) = n2 in
    if gt_big_int newcumulstk bestcumulstk then update_bestnode n2;
    processdelayednodes tm btnr
  | _ -> btnl

let rec processblockvalidation vl =
  match vl with
  | [] -> []
  | (v,f)::vr ->
      let vr2 = processblockvalidation vr in
      f();
      match !v with
      | Waiting(_,_) -> (v,f)::vr2
      | _ -> vr2

let rec is_recent_staker stkaddr n i =
  if i > 0 then
    begin
      let BlocktreeNode(par,stakers,_,_,_,_,_,_,_,_,_,_,_,_) = n in
      if List.mem stkaddr !stakers then
	true
      else
	match par with
	| Some(p) -> is_recent_staker stkaddr p (i-1)
	| _ -> false
    end
  else
    false

let rec record_recent_staker stkaddr n i =
  if i > 0 then
    begin
      let BlocktreeNode(par,stakers,_,_,_,_,_,_,_,_,_,_,_,_) = n in
      stakers := stkaddr::!stakers;
      match par with
      | Some(p) -> record_recent_staker stkaddr p (i-1)
      | None -> ()
    end

let equ_tinfo (x,(y3,y2,y1,y0),z) (u,(v3,v2,v1,v0),w) =
   x = u && y3 = v3 && y2 = v2 && y1 = v1 && Int64.logand y0 (Int64.lognot 1L) = Int64.logand v0 (Int64.lognot 1L) && eq_big_int z w

let rec validate_block_of_node newnode thyroot sigroot csm tinf blkhght h blkdel cs =
  let (blkhd,_) as blkh = (DbBlockHeaderData.dbget h,DbBlockHeaderSig.dbget h) in
  let blk = (blkh,blkdel) in
  if known_thytree_p thyroot && known_sigtree_p sigroot then (*** these should both be known if the parent block has been validated ***)
    let BlocktreeNode(_,_,_,tr2,sr2,_,csm2,tinf2,_,newcumulstake,blkhght2,vs,_,chlr) = newnode in
    Printf.fprintf !log "About to check if block %s at height %Ld is valid\n" (hashval_hexstring h) blkhght;
    begin
      match valid_block (lookup_thytree thyroot) (lookup_sigtree sigroot) blkhght csm tinf blk with
      | Some(tht2,sigt2) ->
	  vs := ValidBlock;
	  Hashtbl.remove tovalidate h;
	  DbBlockDelta.dbput h blkdel;
	  let BlocktreeNode(_,_,_,_,_,_,_,_,_,bestcumulstk,_,_,_,_) = !bestnode in
	  if gt_big_int newcumulstake bestcumulstk then update_bestnode newnode;
	  add_thytree blkhd.newtheoryroot tht2;
	  add_sigtree blkhd.newsignaroot sigt2;
	  broadcast_inv [(int_of_msgtype Blockdelta,blkhght,h)];
	  (*** construct a transformed tree consisting of elements ***)
	  begin
	    let prevc = load_expanded_ctree (ctree_of_block blk) in
	    let (cstk,txl) = txl_of_block blk in (*** the coinstake tx is performed last, i.e., after the txs in the block. ***)
	    match tx_octree_trans false false blkhght cstk (txl_octree_trans false false blkhght txl (Some(prevc))) with (*** "false false" disallows database lookups and remote requests ***)
	    | Some(newc) -> ignore (save_ctree_elements newc)
	    | None -> raise (Failure("transformed tree was empty, although block seemed to be valid"))
	  end;
	  List.iter
	    (fun (h,n) ->
	      let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,vs,_,_) = n in
	      match !vs with
	      | Waiting(_,Some(blkdel,cs)) -> validate_block_of_node n tr2 sr2 csm2 tinf2 blkhght2 h blkdel cs
	      | _ -> ())
	    !chlr
      | None -> (*** We can't mark it as invalid because the peer may be misbehaving and sending a blockdelta that does not correspond to the header. In this case, ban the peer, drop the connection, and request it from someone else. ***)
	  Printf.fprintf !log "Block delta for %s was not valid.\n" (hashval_hexstring h);
	  let tm = Unix.time() in
	  cs.banned <- true;
	  Hashtbl.add bannedpeers cs.addrfrom ();
	  vs := Waiting(tm,None)
    end
  else
    raise (Failure("parent was validated but thyroot and/or sigroot is not known"));;

let fstohash a =
  match a with
  | None -> None
  | Some(h,_) -> Some(h)

let rec process_new_header_a h hh blkhs1h blkh1 blkhd1 blkhs1 initialization knownvalid =
  try
    process_new_header_aa h hh blkhs1h blkh1 blkhd1 blkhs1 (blockheader_stakeasset blkhd1) initialization knownvalid
  with
  | HeaderStakedAssetNotMin ->
      Printf.fprintf !log "Header %s has extra information beyong supporting staked asset; should have been caught before process_new_header_a\n" hh;
      raise (Failure "header does not only support staked asset")
  | HeaderNoStakedAsset ->
      Printf.fprintf !log "Header %s does not support staked asset; should have been caught before process_new_header_a\n" hh;
      raise (Failure "header does not support staked asset")
and process_new_header_aa h hh blkhs1h blkh1 blkhd1 blkhs1 a initialization knownvalid =
  if valid_blockheader_signat blkh1 a then
    process_new_header_ab h hh blkhs1h blkh1 blkhd1 blkhs1 a initialization knownvalid
  else
    begin
      Printf.fprintf !log "Header %s has an invalid signature; should have been caught before process_new_header_aa\n" hh;
      raise (Failure "header has invalid signature")
    end
and process_new_header_ab h hh blkhs1h blkh1 blkhd1 blkhs1 a initialization knownvalid =
  let prevblkh = fstohash blkhd1.prevblockhash in
  begin
    try
      let prevnode = Hashtbl.find blkheadernode prevblkh in
      begin
	try
	  let BlocktreeNode(_,_,prevh,thyroot,sigroot,ledgerroot,csm,currtinfo,tmstamp,prevcumulstk,blkhght,validated,blacklisted,succl) = prevnode in
	  if !blacklisted then (*** child of a blacklisted node, drop and blacklist it ***)
            begin
	      Printf.fprintf !log "Header %s is child of blacklisted node; deleting and blacklisting it.\n" hh;
	      DbBlacklist.dbput h true;
	      DbBlockHeaderData.dbdelete h;
	      DbBlockHeaderSig.dbdelete h;
            end
	  else if
	    valid_blockheader blkhght csm currtinfo blkh1
              && 
	    blockheader_succ_a ledgerroot tmstamp currtinfo blkh1
	  then
	    begin
	      let newcumulstake = cumul_stake prevcumulstk currtinfo blkhd1.deltatime in
	      if not (DbBlockHeaderData.dbexists h) then DbBlockHeaderData.dbput h blkhd1;
	      if not (DbBlockHeaderSig.dbexists h) then DbBlockHeaderSig.dbput h blkhs1;
	      Hashtbl.add blkheaders h ();
	      broadcast_inv [(int_of_msgtype Headers,blkhght,h)];
	      let validated = ref (if knownvalid then ValidBlock else Waiting(Unix.time(),None)) in
	      let csm = poburn_stakemod blkhs1.announcedpoburn in
	      let newnode = BlocktreeNode(Some(prevnode),ref [blkhd1.stakeaddr],Some(h,blkhs1h),blkhd1.newtheoryroot,blkhd1.newsignaroot,blkhd1.newledgerroot,csm,blkhd1.tinfo,blkhd1.timestamp,newcumulstake,Int64.add blkhght 1L,validated,ref false,ref []) in
	      (*** add it as a leaf, indicate that we want the block delta to validate it, and check if it's the best ***)
	      Hashtbl.add blkheadernode (Some(h)) newnode;
	      succl := (h,newnode)::!succl;
	      record_recent_staker blkhd1.stakeaddr prevnode 6;
	      begin
		try
		  let blkdel = DbBlockDelta.dbget h in
		  let blk = (blkh1,blkdel) in
		  if known_thytree_p thyroot && known_sigtree_p sigroot then (*** these should both be known if the parent block has been validated ***)
		    begin
		      match valid_block (lookup_thytree thyroot) (lookup_sigtree sigroot) blkhght csm currtinfo blk with
		      | Some(_,_) ->
			  validated := ValidBlock
		      | None -> (*** should not have happened, delete it from the database and request it again. ***)
			  DbBlockDelta.dbdelete h;
			  Hashtbl.add tovalidate h ();
			  try
			    find_and_send_requestdata GetBlockdelta h
			  with Not_found ->
			    Printf.fprintf !log "No source for block delta of %s; must wait until it is explicitly requested\n" hh
		    end
		  else
		    raise (Failure "unknown thyroot or sigroot while trying to validate block")
		with Not_found ->
		  Hashtbl.add tovalidate h ();
		  try
		    find_and_send_requestdata GetBlockdelta h
		  with Not_found ->
		    Printf.fprintf !log "No source for block delta of %s\n" hh
	      end;
	      if Int64.of_float (Unix.time()) < tmstamp then (*** delay it ***)
		earlyblocktreenodes := insertnewdelayed (tmstamp,newnode) !earlyblocktreenodes
	      else
		let BlocktreeNode(_,_,_,_,_,_,_,_,_,bestcumulstk,_,_,_,_) = !bestnode in
		if gt_big_int newcumulstake bestcumulstk then update_bestnode newnode;
		List.iter
		  (fun (h,blkh1) -> let (blkhd1,blkhs1) = blkh1 in process_new_header_a h (hashval_hexstring h) (hash_blockheadersig blkhs1) blkh1 blkhd1 blkhs1 initialization knownvalid)
		  (Hashtbl.find_all orphanblkheaders (Some(h)));
		Hashtbl.remove orphanblkheaders (Some(h))
	    end
	  else
	    let csm = poburn_stakemod blkhs1.announcedpoburn in
	    begin (*** if it's wrong, delete it and blacklist it so it won't look new in the future [note: signature is assumed to have been checked to be valid by now] ***)
	      Printf.fprintf !log "Header %s was invalid, deleting and blacklisting it.\n" hh;
	      Printf.fprintf !log "vbh %Ld %s %b\n" blkhght (targetinfo_string currtinfo) (valid_blockheader blkhght csm currtinfo blkh1);
	      Printf.fprintf !log "bhsa %s %Ld %s %b\n" (hashval_hexstring ledgerroot) tmstamp (targetinfo_string currtinfo) (blockheader_succ_a ledgerroot tmstamp currtinfo blkh1);
              let newnode = BlocktreeNode(Some(prevnode),ref [],Some(h,blkhs1h),blkhd1.newtheoryroot,blkhd1.newsignaroot,blkhd1.newledgerroot,csm,blkhd1.tinfo,blkhd1.timestamp,zero_big_int,Int64.add blkhght 1L,ref InvalidBlock,ref true,ref []) in (*** dummy node just to remember it is blacklisted ***)
	      Hashtbl.add blkheadernode (Some(h)) newnode;
              DbBlacklist.dbput h true;
	      DbBlockHeaderData.dbdelete h;
	      DbBlockHeaderSig.dbdelete h;
            end
	with Not_found ->
	  Printf.fprintf !log "unexpected Not_found in process_new_header_a %s\n" hh;
	  raise (Failure "unexpected Not_found in process_new_header_a")
      end
    with Not_found -> (*** orphan block header, put it on the relevant hash table and request parent ***)
      Hashtbl.add orphanblkheaders prevblkh (h,blkh1);
      try
	find_and_send_requestdata GetHeader h
      with Not_found -> Printf.fprintf !log "no peer has parent header %s\n" hh
  end
and process_new_header_b h hh initialization knownvalid =
  Printf.fprintf !log "Processing new header %s\n" hh; flush !log;
  try
    let blkhd1 = DbBlockHeaderData.dbget h in
    let blkhs1 = DbBlockHeaderSig.dbget h in
    let blkh1 = (blkhd1,blkhs1) in
    if not (hash_blockheaderdata blkhd1 = h) then (*** wrong hash, remove it but don't blacklist the (wrong) hashval ***)
      begin
        Printf.fprintf !log "WARNING: Block header in database has different hash than key, removing %s\nThis must be due to a bug.\n" hh; flush !log;
	DbBlockHeaderData.dbdelete h;
	DbBlockHeaderSig.dbdelete h
      end
    else
      process_new_header_a h hh (hash_blockheadersig blkhs1) blkh1 blkhd1 blkhs1 initialization knownvalid
  with (*** in some cases, failure should lead to blacklist and removal of the header, but it's not clear which cases; if it's in a block we might need to distinguish between definitely incorrect vs. possibly incorrect ***)
  | Not_found ->
      Printf.fprintf !log "Problem with blockheader %s, deleting it\n" hh; flush !log;
      DbBlockHeaderData.dbdelete h;
      DbBlockHeaderSig.dbdelete h
  | e ->
      Printf.fprintf !log "exception %s\n" (Printexc.to_string e); flush !log;
      ()
and process_new_header h hh initialization knownvalid =
  if not (Hashtbl.mem blkheaders h) then
    process_new_header_b h hh initialization knownvalid

let rec init_headers_to h =
  if DbInvalidatedBlocks.dbexists h || DbBlacklist.dbexists h then
    Printf.fprintf !Utils.log "Skipping invalidated or blacklisted header %s" (hashval_hexstring h)
  else
    try
      let blkhd1 = DbBlockHeaderData.dbget h in
      let blkhs1 = DbBlockHeaderSig.dbget h in
      let blkh1 = (blkhd1,blkhs1) in
      begin
	match blkhd1.prevblockhash with
	| Some(ph,_) -> init_headers_to ph
	| None -> ()
      end;
      process_new_header_a h (hashval_hexstring h) (hash_blockheadersig blkhs1) blkh1 blkhd1 blkhs1 true false
    with Not_found ->
      Printf.printf "Could not find header %s\nStarting node without initializing headers.\n" (hashval_hexstring h)

let rec init_headers h =
  if DbBlockHeaderData.dbexists h then
    init_headers_to h
  else
    try
      let (ltx,prevbh) = DbHeaderLtcBurn.dbget h in
      match prevbh with
      | Some(prevh) -> init_headers prevh
      | None -> ()
    with Not_found ->
      Printf.printf "Could not find ltc burn for header %s\nStarting node without initializing headers.\n" (hashval_hexstring h)

let initblocktree () =
  genesisblocktreenode := BlocktreeNode(None,ref [],None,None,None,!genesisledgerroot,!genesisstakemod,!genesistarget,!Config.genesistimestamp,zero_big_int,1L,ref ValidBlock,ref false,ref []);
  lastcheckpointnode := !genesisblocktreenode;
  update_bestnode !genesisblocktreenode;
  Hashtbl.add blkheadernode None !genesisblocktreenode;
  let (lastchangekey,ctipsl) = ltcdacstatus_dbget !ltc_bestblock in
  List.iter (fun ctips -> List.iter (fun (h,_,_,_,_) -> init_headers h) ctips) ctipsl

let rec find_best_validated_block_from fromnode bestn bestcumulstk =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,cumulstk,_,validatedp,blklistp,succl) = fromnode in
  if not !blklistp && !validatedp = ValidBlock then
    begin
      let newbestn = ref (if gt_big_int cumulstk bestcumulstk then fromnode else bestn) in
      let newbestcumulstk = ref (node_cumulstk !newbestn) in
      List.iter
	(fun (_,childnode) ->
	  let (bn,bcs) = find_best_validated_block_from childnode !newbestn !newbestcumulstk in
	  newbestn := bn;
	  newbestcumulstk := bcs)
	!succl;
      (!newbestn,!newbestcumulstk)
    end
  else
    (bestn,bestcumulstk)

let find_best_validated_block () =
  let (bn,bcs) = find_best_validated_block_from !lastcheckpointnode !lastcheckpointnode (node_cumulstk !lastcheckpointnode) in
  update_bestnode bn

let rec blacklist_from h n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,_,blklstd,chl) = n in
  blklstd := true;
  DbBlacklist.dbput h true;
  DbBlockHeaderData.dbdelete h;
  DbBlockHeaderSig.dbdelete h;
  List.iter (fun (k,c) -> blacklist_from k c) !chl

let publish_stx txh stx1 =
  let (tx1,txsigs1) = stx1 in
  if not (Hashtbl.mem stxpool txh) then Hashtbl.add stxpool txh stx1;
  DbTx.dbput txh tx1;
  DbTxSignatures.dbput txh txsigs1;
  Hashtbl.add published_stx txh ();
  broadcast_inv [(int_of_msgtype STx,0L,txh)]

let publish_block blkh bhh (bh,bd) cs =
  Printf.fprintf !log "publishing block %s\n" (hashval_hexstring bhh); flush !log;
  broadcast_inv [(int_of_msgtype Headers,blkh,bhh);(int_of_msgtype Blockdelta,blkh,bhh)];;

Hashtbl.add msgtype_handler GetHeader
  (fun (sin,sout,cs,ms) ->
    let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
    let i = int_of_msgtype GetHeader in
    if not (List.mem (i,h) cs.sentinv) then (*** don't resend ***)
      try
	let bhd = DbBlockHeaderData.dbget h in
	let bhs = DbBlockHeaderSig.dbget h in
	let bh = (bhd,bhs) in
	let s = Buffer.create 1000 in
	seosbf (seo_blockheader seosb bh (seo_hashval seosb h (seo_int8 seosb 1 (s,None))));
	cs.sentinv <- (i,h)::cs.sentinv;
	let ss = Buffer.contents s in
	ignore (queue_msg cs Headers ss)
      with Not_found ->
	(*** don't have it to send, ignore ***)
	());;

Hashtbl.add msgtype_handler GetHeaders
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let m = ref 0 in
    let bhl = ref [] in
    let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
    c := cn;
    let i = int_of_msgtype GetHeader in
    for j = 1 to n do
      let (h,cn) = sei_hashval seis !c in
      c := cn;
      if not (List.mem (i,h) cs.sentinv) then (*** don't resend ***)
	try
	  let blkhd1 = DbBlockHeaderData.dbget h in
	  let blkhs1 = DbBlockHeaderSig.dbget h in
	  let bh = (blkhd1,blkhs1) in
	  incr m;
	  bhl := (h,bh)::!bhl;
	  cs.sentinv <- (i,h)::cs.sentinv
	with Not_found ->
	  (*** don't have it to send, ignore ***)
	    ()
      done;
    let s = Buffer.create 10000 in
    let co = ref (seo_int8 seosb !m (s,None)) in
    List.iter (fun (h,bh) -> co := seo_blockheader seosb bh (seo_hashval seosb h !co)) !bhl;
    seosbf !co;
    let ss = Buffer.contents s in
    ignore (queue_msg cs Headers ss)
  );;

Hashtbl.add msgtype_handler Headers
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
    c := cn;
    let i = int_of_msgtype GetHeader in
    for j = 1 to n do
      let (h,cn) = sei_hashval seis !c in
      let (bh,cn) = sei_blockheader seis cn in (*** deserialize if only to get to the next one ***)
      c := cn;
      if not (DbBlockHeaderData.dbexists h) && List.mem (i,h) cs.invreq then
	let (bhd,bhs) = bh in
	let bhsh = hash_blockheadersig bhs in
	if not (hash_blockheaderdata bhd = h) then
	  begin (*** this may be the result of a misbehaving peer ***)
	    Printf.fprintf !log "got a header with the wrong hash, dropping it and banning node\n";
	    flush !log;
	    cs.banned <- true
	  end
	else
	  begin
	    try
	      let a = blockheader_stakeasset bhd in
	      if not (valid_blockheader_signat (bhd,bhs) a) then
		begin (*** this may simply be the result of a misbehaving peer mangling the signature of an otherwise valid header ***)
		  Printf.fprintf !log "got a header with an invalid signature, dropping it and banning node\n";
		  flush !log;
		  cs.banned <- true
		end
	      else
		process_new_header_ab h (hashval_hexstring h) bhsh bh bhd bhs a false false
	    with
	    | HeaderStakedAssetNotMin -> (*** here it is safe to blacklist the header's hash since no valid header can have this hash ***)
		begin
		  Printf.fprintf !log "header does not only have the staked asset; blacklisting it and banning node\n";
		  flush !log;
		  cs.banned <- true
		end
	    | HeaderNoStakedAsset -> (*** here it is safe to blacklist the header's hash since no valid header can have this hash ***)
		begin
		  Printf.fprintf !log "header does not have the staked asset; blacklisting it and banning node\n";
		  flush !log;
		  cs.banned <- true
		end
	  end
    done);;

let req_headers sout cs m nw =
  if m > 0 then
    begin
      let s = Buffer.create 1000 in
      let co = ref (seo_int8 seosb m (s,None)) in
      List.iter (fun h -> co := seo_hashval seosb h !co) nw;
      seosbf !co;
      ignore (queue_msg cs GetHeaders (Buffer.contents s))
    end;;

let rec req_header_batches sout cs m hl nw =
  if m = 255 then
    (req_headers sout cs m nw; req_header_batches sout cs 0 hl [])
  else
    match hl with
    | (_,h)::hr ->
	let i = int_of_msgtype GetHeader in
	cs.invreq <- (i,h)::cs.invreq;
	req_header_batches sout cs (m+1) hr (h::nw)
    | [] -> req_headers sout cs m nw;;

Hashtbl.add msgtype_handler Inv
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let hl = ref [] in
    let (n,cn) = sei_int32 seis !c in
    c := cn;
    for j = 1 to Int32.to_int n do
      let ((i,blkh,h),cn) = sei_prod3 sei_int8 sei_int64 sei_hashval seis !c in
      c := cn;
      cs.rinv <- (i,h)::cs.rinv;
      if i = int_of_msgtype Headers then Printf.fprintf !log "Headers, dbexists %b, archived %b\n" (DbBlockHeaderData.dbexists h) (DbArchived.dbexists h);
      if i = int_of_msgtype Headers && not (DbArchived.dbexists h) then
	begin
	  try
	    let blkhd1 = DbBlockHeaderData.dbget h in
	    let blkhs1 = DbBlockHeaderSig.dbget h in
	    let bh = (blkhd1,blkhs1) in
	    if not (Hashtbl.mem blkheadernode (Some(h))) then
	      let (bhd,bhs) = bh in
	      process_new_header_a h (hashval_hexstring h) (hash_blockheadersig bhs) bh bhd bhs false false
	  with Not_found ->
	    hl := List.merge (fun (blkh1,_) (blkh2,_) -> compare blkh2 blkh1) !hl [(blkh,h)] (*** reverse order because they will be reversed again when requested ***)
	end
      else if i = int_of_msgtype Blockdelta && not (DbBlockDelta.dbexists h) && not (DbArchived.dbexists h) && Hashtbl.mem tovalidate h then
	begin
	  try
            cs.invreq <- (int_of_msgtype GetBlockdelta,h)::cs.invreq;
	    let s = Buffer.create 1000 in
	    seosbf (seo_hashval seosb h (s,None));
	    Printf.fprintf !log "Immediately requesting blockdelta %s\n" (hashval_hexstring h);
	    ignore (queue_msg cs GetBlockdelta (Buffer.contents s))
	  with exn -> Printf.fprintf !log "inv blockdelta %s\n" (Printexc.to_string exn)
	end
      else if i = int_of_msgtype STx && not (DbArchived.dbexists h) then
	begin
	  if DbTx.dbexists h then
	    if not (DbTxSignatures.dbexists h) then
	      begin
                cs.invreq <- (int_of_msgtype GetTxSignatures,h)::cs.invreq;
		let s = Buffer.create 1000 in
		seosbf (seo_hashval seosb h (s,None));
		ignore (queue_msg cs GetTxSignatures (Buffer.contents s))
	      end
            else ()
          else
 	    begin
              cs.invreq <- (int_of_msgtype GetTx,h)::cs.invreq;
              let s = Buffer.create 1000 in
	      seosbf (seo_hashval seosb h (s,None));
	      ignore (queue_msg cs GetTx (Buffer.contents s))
	    end
	end
      else if i = int_of_msgtype Checkpoint then
	begin (*** only request if we need it ***)
	  try
	    let lcp = !lastcheckpointnode in
	    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,lcpblkh,_,_,_) = lcp in
	    if lcpblkh < blkh then
	      raise Exit
	    else
	      let lcpp = prev_nth_node (Int64.to_int (Int64.sub lcpblkh blkh)) lcp in
	      match lcpp with
	      | None -> raise Exit
	      | Some(lcppn) ->
		match node_prevblockhash lcppn with
		| None -> raise Exit
		| Some(lcppnh,_) -> if not (lcppnh = h) then raise Exit
	  with Exit -> (*** if Exit was raised, then we need the checkpoint ***)
	    cs.invreq <- (int_of_msgtype GetCheckpoint,h)::cs.invreq;
            let s = Buffer.create 1000 in
	    seosbf (seo_hashval seosb h (s,None));
	    ignore (queue_msg cs GetCheckpoint (Buffer.contents s))
	end
    done;
    req_header_batches sout cs 0 !hl []);;

Hashtbl.add msgtype_handler GetBlockdelta
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetBlockdelta in
      if not (List.mem (i,h) cs.sentinv) then (*** don't resend ***)
	try
	  let blkdel = DbBlockDelta.dbget h in
	  let bdsb = Buffer.create 100 in
	  seosbf (seo_blockdelta seosb blkdel (seo_hashval seosb h (bdsb,None)));
	  let bdser = Buffer.contents bdsb in
	  ignore (queue_msg cs Blockdelta bdser);
	  cs.sentinv <- (i,h)::cs.sentinv
	with Not_found ->
	  Printf.fprintf !log "Unknown Block Delta %s (Bad Peer or Did I Advertize False Inventory?)\n" (hashval_hexstring h);
	  ());;

Hashtbl.add msgtype_handler Blockdelta
  (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetBlockdelta in
      if not (DbBlockDelta.dbexists h) then (*** if we already have it, abort ***)
	begin
	  try
	    cs.invreq <- List.filter (fun (j,k) -> not (i = j && h = k)) cs.invreq;
	    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,vs,_,chlr) as newnode = Hashtbl.find blkheadernode (Some(h)) in
	    match !vs with
	    | Waiting(tm,None) ->
		begin
		  match par with
		  | None -> (*** genesis node, parent implicitly valid ***)
		      let (blkdel,_) = sei_blockdelta seis r in
		      validate_block_of_node newnode None None !genesisstakemod !genesistarget 1L h blkdel cs
		  | Some(BlocktreeNode(_,_,_,thyroot,sigroot,_,csm,tinf,_,_,blkhght,vsp,_,_)) ->
		      match !vsp with
		      | InvalidBlock -> raise Not_found (*** questionable if this can happen ***)
		      | Waiting(_,_) ->
			  let (blkdel,_) = sei_blockdelta seis r in
			  vs := Waiting(tm,Some(blkdel,cs)) (*** wait for the parent to be validated; remember the connstate in case we decide to ban it for giving a bad block delta ***)
		      | ValidBlock -> (*** validate now, and if valid check if children nodes are waiting to be validated ***)
			  begin
			    let (blkdel,_) = sei_blockdelta seis r in
			    validate_block_of_node newnode thyroot sigroot csm tinf blkhght h blkdel cs
			  end
		end
	    | _ -> ()
	  with Not_found -> ()
	end);;

Hashtbl.add msgtype_handler GetTx
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetTx in
      if not (List.mem (i,h) cs.sentinv) then (*** don't resend ***)
	try
	  let (tau,_) = Hashtbl.find stxpool h in
	  let tausb = Buffer.create 100 in
	  seosbf (seo_tx seosb tau (seo_hashval seosb h (tausb,None)));
	  let tauser = Buffer.contents tausb in
	  Printf.fprintf !log "Sending Tx (from pool) %s\n" (hashval_hexstring h);
	  ignore (queue_msg cs Tx tauser);
	  cs.sentinv <- (i,h)::cs.sentinv
	with Not_found ->
	  try
	    let tau = DbTx.dbget h in
	    let tausb = Buffer.create 100 in
	    seosbf (seo_tx seosb tau (seo_hashval seosb h (tausb,None)));
	    let tauser = Buffer.contents tausb in
	    Printf.fprintf !log "Sending Tx (from db) %s\n" (hashval_hexstring h);
	    ignore (queue_msg cs Tx tauser);
	    cs.sentinv <- (i,h)::cs.sentinv
	  with Not_found ->
	    Printf.fprintf !log "Unknown Tx %s\n" (hashval_hexstring h);
	    ());;

Hashtbl.add msgtype_handler Tx
    (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetTx in
      if not (DbTx.dbexists h) then (*** if we already have it, abort ***)
	if List.mem (i,h) cs.invreq then (*** only continue if it was requested ***)
          let (tau,_) = sei_tx seis r in
	  if hashtx tau = h then
	    begin
  	      DbTx.dbput h tau;
	      cs.invreq <- List.filter (fun (j,k) -> not (i = j && h = k)) cs.invreq
	    end
          else (*** otherwise, it seems to be a misbehaving peer --  ignore for now ***)
	    (Printf.fprintf !Utils.log "misbehaving peer? [malformed Tx]\n"; flush !Utils.log)
	else (*** if something unrequested was sent, then seems to be a misbehaving peer ***)
	  (Printf.fprintf !Utils.log "misbehaving peer? [unrequested Tx]\n"; flush !Utils.log));;

Hashtbl.add msgtype_handler GetTxSignatures
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      Printf.fprintf !log "Processing GetTxSignatures %s\n" (hashval_hexstring h);
      let i = int_of_msgtype GetTxSignatures in
      if not (List.mem (i,h) cs.sentinv) then (*** don't resend ***)
	try
	  let (_,s) = Hashtbl.find stxpool h in
	  let ssb = Buffer.create 100 in
	  seosbf (seo_txsigs seosb s (seo_hashval seosb h (ssb,None)));
	  let sser = Buffer.contents ssb in
	  Printf.fprintf !log "Sending TxSignatures (from pool) %s\n" (hashval_hexstring h);
	  ignore (queue_msg cs TxSignatures sser);
	  cs.sentinv <- (i,h)::cs.sentinv
	with Not_found ->
	  try
	    let s = DbTxSignatures.dbget h in
	    let ssb = Buffer.create 100 in
	    seosbf (seo_txsigs seosb s (seo_hashval seosb h (ssb,None)));
	    let sser = Buffer.contents ssb in
	    Printf.fprintf !log "Sending TxSignatures (from db) %s\n" (hashval_hexstring h);
	    ignore (queue_msg cs TxSignatures sser);
	    cs.sentinv <- (i,h)::cs.sentinv
	with Not_found ->
	  Printf.fprintf !log "Unknown TxSignatures %s\n" (hashval_hexstring h);
	  ());;

Hashtbl.add msgtype_handler TxSignatures
    (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetTxSignatures in
      if not (DbTxSignatures.dbexists h) then (*** if we already have it, abort ***)
	try
	  let ((tauin,_) as tau) = DbTx.dbget h in
	  if List.mem (i,h) cs.invreq then (*** only continue if it was requested ***)
            let (s,_) = sei_txsigs seis r in
	    try
	      let al = List.map (fun (_,aid) -> DbAsset.dbget aid) tauin in
	      ignore (tx_signatures_valid_asof_blkh al (tau,s)); (*** signatures valid at some block height ***)
              Hashtbl.add stxpool h (tau,s);
  	      DbTxSignatures.dbput h s;
	      cs.invreq <- List.filter (fun (j,k) -> not (i = j && h = k)) cs.invreq
	    with
	    | BadOrMissingSignature -> (*** otherwise, it seems to be a misbehaving peer --  ignore for now ***)
		(Printf.fprintf !Utils.log "misbehaving peer? [malformed TxSignatures]\n"; flush !Utils.log)
	    | Not_found -> (*** in this case, we don't have all the spent assets in the database, which means we shouldn't have requested the signatures, ignore ***)
		()
	  else (*** if something unrequested was sent, then seems to be a misbehaving peer ***)
	    (Printf.fprintf !Utils.log "misbehaving peer? [unrequested TxSignatures]\n"; flush !Utils.log)
	with Not_found -> (*** do not know the tx, so drop the sig ***)
	  ());;

let dumpblocktreestate sa =
  Printf.fprintf sa "=========\nstxpool:\n";
  Hashtbl.iter
    (fun h ((tauin,tauout) as tau,tausg) ->
      Printf.fprintf sa "- tx %s\n" (hashval_hexstring (hashtx tau));
      Printf.fprintf sa "inputs %d\n" (List.length tauin);
      let c = ref 0 in
      List.iter
	(fun (alpha,aid) ->
	  Printf.fprintf sa "%d. %s %s\n" !c (Cryptocurr.addr_daliladdrstr alpha) (hashval_hexstring aid);
	  incr c)
	tauin;
      Printf.fprintf sa "outputs %d\n" (List.length tauin);
      c := 0;
      List.iter (fun (alpha,(obl,u)) ->
	Printf.fprintf sa "%d. %s %s %s\n" !c (Cryptocurr.addr_daliladdrstr alpha) (obligation_string obl) (preasset_string u);
	incr c)
	tauout;
      let sb = Buffer.create 100 in
      seosbf (seo_stx seosb (tau,tausg) (sb,None));
      Printf.fprintf sa "%s\n" (string_hexstring (Buffer.contents sb))
    )
    stxpool;
  Printf.fprintf sa "=========\npublished_stx:\n";
  Hashtbl.iter (fun h () ->
      Printf.fprintf sa "- tx %s\n" (hashval_hexstring h))
    published_stx;
  Printf.fprintf sa "=========\nthytree:\n";
  Hashtbl.iter (fun h _ ->
    Printf.fprintf sa "- thytree root %s\n" (hashval_hexstring h))
    thytree;
  Printf.fprintf sa "=========\nsigtree:\n";
  Hashtbl.iter (fun h _ ->
    Printf.fprintf sa "- sigtree root %s\n" (hashval_hexstring h))
    sigtree;
  Printf.fprintf sa "=========\nblkheaders:\n";
  Hashtbl.iter
    (fun h _ ->
      Printf.fprintf sa "- blk %s\n" (hashval_hexstring h))
    blkheaders;
  Printf.fprintf sa "=========\nblkheadernode:\n";
  Hashtbl.iter
    (fun h (BlocktreeNode(_,rs,pbh,tr,sr,lr,csm,tar,tm,cs,blkh,vs,bl,chr)) ->
      Printf.fprintf sa "- blk %s node:\n" (match h with Some(h) -> hashval_hexstring h | None -> "[genesis]");
      Printf.fprintf sa "recentstakers:\n";
      List.iter (fun k -> Printf.fprintf sa "%s\n" (Cryptocurr.addr_daliladdrstr (p2pkhaddr_addr k))) !rs;
      Printf.fprintf sa "prevblockhash: %s\n" (match pbh with Some(h,_) -> hashval_hexstring h | None -> "[genesis]");
      Printf.fprintf sa "theory tree root: %s\n" (match tr with Some(h) -> hashval_hexstring h | None -> "[empty]");
      Printf.fprintf sa "sig tree root: %s\n" (match sr with Some(h) -> hashval_hexstring h | None -> "[empty]");
      Printf.fprintf sa "ledger tree root: %s\n" (hashval_hexstring lr);
      Printf.fprintf sa "targetinfo:\ncsm %s\ntar %s\n" (hashval_hexstring csm) (string_of_big_int tar);
      Printf.fprintf sa "timestamp: %Ld\n" tm;
      Printf.fprintf sa "cumulative stake: %s\n" (string_of_big_int cs);
      Printf.fprintf sa "block height: %Ld\n" blkh;
      Printf.fprintf sa "validation status: %s\n"
	(match !vs with Waiting(tm,None) -> "Waiting " ^ string_of_float tm | Waiting(tm,Some(_)) -> "Waiting for parent " ^ string_of_float tm | ValidBlock -> "Valid" | InvalidBlock -> "Invalid");
      if !bl then Printf.fprintf sa "*blacklisted*\n";
      Printf.fprintf sa "children nodes: %d\n" (List.length !chr);
      List.iter (fun (h,_) -> Printf.fprintf sa "%s\n" (hashval_hexstring h)) !chr)
    blkheadernode;
  Printf.fprintf sa "=========\norphanblkheaders:\n";
  Hashtbl.iter
    (fun h (k,bh) ->
      Printf.fprintf sa "- orphan blk %s waiting for %s\n" (hashval_hexstring k) (match h with Some(h) -> hashval_hexstring h | None -> "[genesis?]");
      let sb = Buffer.create 100 in
      seosbf (seo_blockheader seosb bh (sb,None));
      Printf.fprintf sa "%s\n" (string_hexstring (Buffer.contents sb)))
    orphanblkheaders;
  Printf.fprintf sa "=========\nearlyblocktreenodes:\n";
  List.iter
    (fun (futuretm,BlocktreeNode(_,rs,pbh,tr,sr,lr,csm,tar,tm,cs,blkh,vs,bl,chr)) ->
      Printf.fprintf sa "future timestamp: %Ld\n" futuretm;
      Printf.fprintf sa "recentstakers:\n";
      List.iter (fun k -> Printf.fprintf sa "%s\n" (Cryptocurr.addr_daliladdrstr (p2pkhaddr_addr k))) !rs;
      Printf.fprintf sa "prevblockhash: %s\n" (match pbh with Some(h,_) -> hashval_hexstring h | None -> "[genesis]");
      Printf.fprintf sa "theory tree root: %s\n" (match tr with Some(h) -> hashval_hexstring h | None -> "[empty]");
      Printf.fprintf sa "sig tree root: %s\n" (match sr with Some(h) -> hashval_hexstring h | None -> "[empty]");
      Printf.fprintf sa "ledger tree root: %s\n" (hashval_hexstring lr);
      Printf.fprintf sa "targetinfo:\ncsm %s\ntar %s\n" (hashval_hexstring csm) (string_of_big_int tar);
      Printf.fprintf sa "timestamp: %Ld\n" tm;
      Printf.fprintf sa "cumulative stake: %s\n" (string_of_big_int cs);
      Printf.fprintf sa "block height: %Ld\n" blkh;
      Printf.fprintf sa "validation status: %s\n"
	(match !vs with Waiting(tm,None) -> "Waiting " ^ string_of_float tm | Waiting(tm,Some(_)) -> "Waiting for parent " ^ string_of_float tm | ValidBlock -> "Valid" | InvalidBlock -> "Invalid");
      if !bl then Printf.fprintf sa "*blacklisted*\n";
      Printf.fprintf sa "children nodes: %d\n" (List.length !chr);
      List.iter (fun (h,_) -> Printf.fprintf sa "%s\n" (hashval_hexstring h)) !chr)
    !earlyblocktreenodes;
  Printf.fprintf sa "=========\ntovalidate:\n";
  Hashtbl.iter
    (fun h () ->
      Printf.fprintf sa "%s\n" (hashval_hexstring h))
    tovalidate;;

Hashtbl.add msgtype_handler GetCheckpoint
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetCheckpoint in
      if not (List.mem (i,h) cs.sentinv) then (*** don't resend ***)
	try
	  let (chblkh,chsg) = Hashtbl.find checkpoints h in
	  let sb = Buffer.create 100 in
	  seosbf (seo_signat seosb chsg (seo_int64 seosb chblkh (seo_hashval seosb h (sb,None))));
	  let ser = Buffer.contents sb in
	  ignore (queue_msg cs Checkpoint ser);
	  cs.sentinv <- (i,h)::cs.sentinv
	with Not_found ->
	  Printf.fprintf !log "Unknown Checkpoint %s (Bad Peer or Did I Advertize False Inventory?)\n" (hashval_hexstring h);
	  ());;

Hashtbl.add msgtype_handler Checkpoint
    (fun (sin,sout,cs,ms) ->
      Printf.fprintf !log "Processing Checkpoint\n";
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let (blkh,r) = sei_int64 seis r in
      Printf.fprintf !log "Processing Checkpoint %Ld %s\n" blkh (hashval_hexstring h);
      let (sg,_) = sei_signat seis r in
      if Signat.verify_signed_hashval h (Some(checkpointspubkeyx,checkpointspubkeyy)) sg then
	begin
	  Hashtbl.add checkpoints h (blkh,sg);
	  if DbBlacklist.dbexists h then
	    begin
	      Printf.fprintf !log "Checkpoint %s was blacklisted; removing the blacklist.\n" (hashval_hexstring h);
	      DbBlacklist.dbdelete h
	    end;
	  try
	    let n = Hashtbl.find blkheadernode (Some(h)) in
	    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,vs,blklstd,_) = n in
	    begin
	      match par with
	      | None -> ()
	      | Some(p) ->
		  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,_,_,chl) = p in
		  List.iter
		    (fun (k,c) ->
		      if not (k = h) then blacklist_from k c
		      )
		    !chl
	    end;
	    if !blklstd then blklstd := false;
	    if not (!vs = ValidBlock) then vs := ValidBlock;
	    lastcheckpointnode := n;
	    find_best_validated_block();
	    broadcast_inv [(int_of_msgtype Checkpoint,node_blockheight n,h)]
	  with Not_found ->
	    try
	      let blkhd1 = DbBlockHeaderData.dbget h in
	      let blkhs1 = DbBlockHeaderSig.dbget h in
	      let bh = (blkhd1,blkhs1) in
	      let (bhd,bhs) = bh in
	      let bhsh = hash_blockheadersig bhs in
	      let csm = poburn_stakemod bhs.announcedpoburn in
	      let fnode = BlocktreeNode(None,ref [],Some(h,bhsh),bhd.newtheoryroot,bhd.newsignaroot,bhd.newledgerroot,csm,bhd.tinfo,bhd.timestamp,zero_big_int,blkh,ref ValidBlock,ref false,ref []) in (*** just drop the cumul stake back to zero because I don't know it here ***)
	      Hashtbl.add blkheadernode (Some(h)) fnode;
	      Hashtbl.remove orphanblkheaders (Some(h));
	      lastcheckpointnode := fnode;
	      update_bestnode fnode
	    with Not_found ->
	      Printf.fprintf !log "Unknown header %s, trying to request it.\n" (hashval_hexstring h);
	      try
		find_and_send_requestdata Headers h
	      with Not_found ->
		Printf.fprintf !log "No peer willing to send header was found %s\n" (hashval_hexstring h)
	end
      else
	Printf.fprintf !log "Bad signature on checkpoint\n%s\n" (string_hexstring ms));;

