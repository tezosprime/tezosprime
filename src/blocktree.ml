(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Utils
open Ser
open Hashaux
open Sha256
open Hash
open Htree
open Net
open Db
open Assets
open Signat
open Tx
open Ctre
open Block
open Ltcrpc

let stxpool : (hashval,stx) Hashtbl.t = Hashtbl.create 1000;;
let published_stx : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;
let unconfirmed_spent_assets : (hashval,hashval) Hashtbl.t = Hashtbl.create 100;;

let processing_deltas : hashval list ref = ref [];;

let save_processing_deltas () =
  match !processing_deltas with
  | [] -> ()
  | hl ->
      let pdf = Filename.concat (datadir()) "processingdeltas" in
      let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 pdf in
      List.iter (fun h -> seocf (seo_hashval seoc h (ch,None))) hl;
      close_out ch

let thytree : (hashval,Mathdata.ttree) Hashtbl.t = Hashtbl.create 1000;;
let sigtree : (hashval,Mathdata.stree) Hashtbl.t = Hashtbl.create 1000;;

type validationstatus = Waiting of float * (blockdelta * connstate) option | ValidBlock | InvalidBlock

type blocktree = BlocktreeNode of blocktree option * p2pkhaddr list ref * (hashval * poburn) option * hashval option * hashval option * hashval * stakemod * targetinfo * int64 * big_int * int64 * validationstatus ref * bool ref * (hashval * blocktree) list ref

let genesisblocktreenode = ref (BlocktreeNode(None,ref [],None,None,None,!genesisledgerroot,!genesisstakemod,!genesistarget,!Config.genesistimestamp,zero_big_int,1L,ref ValidBlock,ref false,ref []));;

let lastcheckpointnode = ref !genesisblocktreenode;;

let node_recent_stakers n =
  let BlocktreeNode(_,rs,_,_,_,_,_,_,_,_,_,_,_,_) = n in
  !rs

let node_prevblockhash n =
  let BlocktreeNode(_,_,pbh,_,_,_,_,_,_,_,_,_,_,_) = n in
  pbh

let fstohash a =
  match a with
  | None -> None
  | Some(h,_) -> Some(h)

let node_parprevblockhash n =
  let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,_,_,_) = n in
  match par with
  | None -> None
  | Some(p) -> fstohash (node_prevblockhash p)

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

let node_blockheight n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,blkh,_,_,_) = n in
  blkh

let node_validationstatus n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,vs,_,_) = n in
  !vs

let node_children_ref n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,_,_,chr) = n in
  chr

let eq_node n1 n2 = node_prevblockhash n1 = node_prevblockhash n2

let blkheaders : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;
let blkheadernode : (hashval option,blocktree) Hashtbl.t = Hashtbl.create 1000;;
let orphanblkheaders : (hashval option,hashval * blockheader) Hashtbl.t = Hashtbl.create 1000;;
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

let rec get_all_theories t =
  match t with
  | None -> []
  | Some(HBin(tl,tr)) -> get_all_theories tl @ get_all_theories tr
  | Some(HLeaf(x)) ->
      match Mathdata.hashtheory x with
      | Some(h) -> [(h,x)]
      | None -> raise (Failure "empty theory ended up in the theory tree somehow")

let rec get_all_signas t loc =
  match t with
  | None -> []
  | Some(HLeaf(x)) -> [(bitseq_hashval (List.rev loc),Mathdata.hashsigna x,x)]
  | Some(HBin(tl,tr)) -> get_all_signas tl (false::loc) @ get_all_signas tr (true::loc)

let rec get_added_theories t1 t2 =
  match (t1,t2) with
  | (None,t2) -> get_all_theories t2
  | (Some(HLeaf(_)),Some(HLeaf(_))) -> [] (*** assume equal, which should be an invariant ***)
  | (Some(HBin(t1l,t1r)),Some(HBin(t2l,t2r))) -> get_added_theories t1l t2l @ get_added_theories t1r t2r (*** inefficient, but new theories should be rare ***)
  | (_,_) -> raise (Failure("Impossible pair of old and new theory trees"))

let rec get_added_signas t1 t2 loc =
  match (t1,t2) with
  | (None,t2) -> get_all_signas t2 loc
  | (Some(HLeaf(_)),Some(HLeaf(_))) -> [] (*** assume equal, which should be an invariant ***)
  | (Some(HBin(t1l,t1r)),Some(HBin(t2l,t2r))) -> get_added_signas t1l t2l (false::loc) @ get_added_signas t1r t2r (true::loc) (*** inefficient, but new signatures should be rare ***)
  | (_,_) -> raise (Failure("Impossible pair of old and new signature trees"))

(*** save information indicating how to rebuild the theory and signature trees upon initialization ***)
let update_theories oldthyroot oldthytree newthytree =
  let newthyroot = Mathdata.ottree_hashroot newthytree in
  if not (oldthyroot = newthyroot) then
    begin
      match newthyroot with
      | None -> raise (Failure "cannot go from nonempty thy tree to empty thy tree")
      | Some(newthyrootreal) ->
	  let addedtheories = get_added_theories oldthytree newthytree in
	  List.iter
	    (fun (h,thy) -> Mathdata.DbTheory.dbput h thy)
	    addedtheories;
	  let ttf = Filename.concat (datadir()) "theorytreeinfo" in
	  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 ttf in
	  seocf (seo_prod3 (seo_option seo_hashval) seo_hashval (seo_list seo_hashval) seoc
		   (oldthyroot,newthyrootreal,List.map (fun (h,_) -> h) addedtheories)
		   (ch,None));
	  close_out ch;
	  add_thytree newthyroot newthytree
    end

let update_signatures oldsigroot oldsigtree newsigtree =
  let newsigroot = Mathdata.ostree_hashroot newsigtree in
  if not (oldsigroot = newsigroot) then
    begin
      match newsigroot with
      | None -> raise (Failure "cannot go from nonempty sig tree to empty sig tree")
      | Some(newsigrootreal) ->
	  let addedsignas = get_added_signas oldsigtree newsigtree [] in
	  List.iter
	    (fun (_,k,signa) -> Mathdata.DbSigna.dbput k signa)
	    addedsignas;
	  let stf = Filename.concat (datadir()) "signatreeinfo" in
	  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 stf in
	  seocf (seo_prod3 (seo_option seo_hashval) seo_hashval (seo_list (seo_prod seo_hashval seo_hashval)) seoc
		   (oldsigroot,newsigrootreal,List.map (fun (h,k,_) -> (h,k)) addedsignas)
		   (ch,None));
	  close_out ch;
	  add_sigtree newsigroot newsigtree
    end

let init_thytrees () =
  let ttf = Filename.concat (datadir()) "theorytreeinfo" in
  if Sys.file_exists ttf then
    let ch = open_in_bin ttf in
    try
      while true do
	let ((oldroot,newroot,added),_) = sei_prod3 (sei_option sei_hashval) sei_hashval (sei_list sei_hashval) seic (ch,None) in
	try
	  let oldthytree = lookup_thytree oldroot in
	  let newthytree = ref oldthytree in
	  List.iter
	    (fun h ->
	      try
		let th = Mathdata.DbTheory.dbget h in
		newthytree := Some(Mathdata.ottree_insert !newthytree (hashval_bitseq h) th)
	      with Not_found ->
		raise (Failure("fatal error trying to initialize theory trees; unknown theory " ^ (hashval_hexstring h))))
	    added;
	  let newroot2 = Mathdata.ottree_hashroot !newthytree in
	  if newroot2 = Some(newroot) then
	    begin
	      match !newthytree with
	      | Some(ntt) -> Hashtbl.add thytree newroot ntt
	      | None -> () (*** should not happen ***)
	    end
	  else
	    begin
	      close_in ch;
	      raise (Failure("fatal error trying to initialize theory trees; theory tree root mismatch expected " ^ (hashval_hexstring newroot) ^ " but got " ^ (match newroot2 with None -> "None" | Some(h) -> hashval_hexstring h)))
	    end
	with Not_found ->
	  close_in ch;
	  raise (Failure("fatal error trying to initialize theory trees; did not build tree with root " ^ (match oldroot with None -> "None" | Some(h) -> hashval_hexstring h)))
      done
    with End_of_file ->
      close_in ch

let init_sigtrees () =
  let stf = Filename.concat (datadir()) "signatreeinfo" in
  if Sys.file_exists stf then
    let ch = open_in_bin stf in
    try
      while true do
	let ((oldroot,newroot,added),_) = sei_prod3 (sei_option sei_hashval) sei_hashval (sei_list (sei_prod sei_hashval sei_hashval)) seic (ch,None) in
	try
	  let oldsigtree = lookup_sigtree oldroot in
	  let newsigtree = ref oldsigtree in
	  List.iter
	    (fun (h,k) ->
	      try
		let s = Mathdata.DbSigna.dbget k in
		newsigtree := Some(Mathdata.ostree_insert !newsigtree (hashval_bitseq h) s)
	      with Not_found ->
		raise (Failure("fatal error trying to initialize signature trees; unknown signa " ^ (hashval_hexstring h))))
	    added;
	  let newroot2 = Mathdata.ostree_hashroot !newsigtree in
	  if newroot2 = Some(newroot) then
	    begin
	      match !newsigtree with
	      | Some(nst) -> Hashtbl.add sigtree newroot nst
	      | None -> ()
	    end
	  else
	    begin
	      close_in ch;
	      raise (Failure("fatal error trying to initialize signature trees; signa tree root mismatch expected " ^ (hashval_hexstring newroot) ^ " but got " ^ (match newroot2 with None -> "None" | Some(h) -> hashval_hexstring h)))
	    end
	with Not_found ->
	  close_in ch;
	  raise (Failure("fatal error trying to initialize signa trees; did not build tree with root " ^ (match oldroot with None -> "None" | Some(h) -> hashval_hexstring h)))
      done
    with End_of_file ->
      close_in ch

let collect_inv m cnt tosend txinv =
  let (lastchangekey,ctips0l) = ltcdacstatus_dbget !ltc_bestblock in
  let inclh : (hashval,unit) Hashtbl.t = Hashtbl.create 5000 in
  let rec collect_inv_chain tosend n =
    List.iter
      (fun (bh,nch) ->
	collect_inv_chain tosend nch;
	if not (Hashtbl.mem inclh bh) then
	  begin
	    if DbBlockHeader.dbexists bh then
	      begin
		Hashtbl.add inclh bh ();
		tosend := (int_of_msgtype Headers,bh)::!tosend;
		if DbBlockDelta.dbexists bh then (tosend := (int_of_msgtype Blockdelta,bh)::!tosend)
	      end;
	  end)
      !(node_children_ref n)
  in
  let rec collect_inv_r3 m cnt tosend ctips ctipsr txinv =
    if !cnt < m then
      begin
	match ctips with
	| (bh,_,_,_,_)::ctipr ->
	    if not (Hashtbl.mem inclh bh) then
	      begin
		if DbBlockHeader.dbexists bh then
		  begin
		    Hashtbl.add inclh bh ();
		    tosend := (int_of_msgtype Headers,bh)::!tosend; incr cnt;
		    if DbBlockDelta.dbexists bh then (tosend := (int_of_msgtype Blockdelta,bh)::!tosend; incr cnt)
		  end;
	      end;
	    collect_inv_r3 m cnt tosend ctipr ctipsr txinv
	| [] -> collect_inv_r2 m cnt tosend ctipsr txinv
      end
  and collect_inv_r2 m cnt tosend ctipsl txinv =
    if !cnt < m then
      begin
	match ctipsl with
	| (ctips::ctipsr) -> collect_inv_r3 m cnt tosend ctips ctipsr txinv
	| [] ->
	    if not (txinv = []) then collect_inv_r1 m cnt tosend [] txinv
      end
  and collect_inv_r1 m cnt tosend ctipsl txinv =
    if !cnt < m then
      begin
	match txinv with
	| (txid::txinvr) ->
	    tosend := (int_of_msgtype STx,txid)::!tosend; incr cnt;
	    collect_inv_r1 m cnt tosend ctipsl txinvr
	| []  ->
	    if not (ctipsl = []) then
	      collect_inv_r2 m cnt tosend ctipsl txinv
      end
  in
  collect_inv_chain tosend !genesisblocktreenode;
  collect_inv_r1 m cnt tosend ctips0l txinv

let send_inv m sout cs =
  let cnt = ref 0 in
  let tosend = ref [] in
  let txinv = ref [] in
  Hashtbl.iter (fun k _ -> txinv := k::!txinv) stxpool;
  collect_inv m cnt tosend !txinv;
  let invmsg = Buffer.create 10000 in
  let c = ref (seo_int32 seosb (Int32.of_int (List.length !tosend)) (invmsg,None)) in
  List.iter
    (fun (i,h) ->
      let cn = seo_prod seo_int8 seo_hashval seosb (i,h) !c in
      c := cn)
    !tosend;
  ignore (queue_msg cs Inv (Buffer.contents invmsg));;

send_inv_fn := send_inv;;

let rec insertnewdelayed (tm,n) btnl =
  match btnl with
  | [] -> [(tm,n)]
  | (tm2,n2)::btnr when tm < tm2 -> (tm,n)::btnl
  | (tm2,n2)::btnr -> (tm2,n2)::insertnewdelayed (tm,n) btnr

let rec prev_nth_node i n =
  if i <= 0 then
    Some(n)
  else
    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,_,_,_) = n in
    match par with
    | None -> None
    | Some(p) -> prev_nth_node (i-1) p

let rec processblockvalidation vl =
  match vl with
  | [] -> []
  | (v,f)::vr ->
      let vr2 = processblockvalidation vr in
      f();
      match !v with
      | Waiting(_,_) -> (v,f)::vr2
      | _ -> vr2

let equ_tinfo (x,(y3,y2,y1,y0),z) (u,(v3,v2,v1,v0),w) =
   x = u && y3 = v3 && y2 = v2 && y1 = v1 && Int64.logand y0 (Int64.lognot 1L) = Int64.logand v0 (Int64.lognot 1L) && eq_big_int z w

type consensuswarning =
  | ConsensusWarningMissing of hashval * hashval option * int64 * bool * bool * string
  | ConsensusWarningWaiting of hashval * hashval option * int64 * float * bool * bool
  | ConsensusWarningBlacklist of hashval * hashval option * int64
  | ConsensusWarningInvalid of hashval * hashval option * int64
  | ConsensusWarningNoBurn of hashval
  | ConsensusWarningTerminal

exception NoReq

let rec get_bestnode req =
  let (lastchangekey,ctips0l) = ltcdacstatus_dbget !ltc_bestblock in
  let tm = ltc_medtime() in
  if ctips0l = [] && tm > Int64.add !Config.genesistimestamp 604800L then
    begin
      Printf.printf "No blocks were created in the past week. Dalilcoin has reached terminal status.\n"
    end;
  let rec get_bestnode_r2 ctips ctipsr cwl =
    match ctips with
    | [] -> get_bestnode_r ctipsr cwl
    | (dbh,lbh,ltxh,ltm,lhght)::ctipr ->
	begin
	  let handle_node n =
	    let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,blkh,vs,_,_) = n in
	    if DbInvalidatedBlocks.dbexists dbh then
	      get_bestnode_r2 ctipr ctipsr (ConsensusWarningInvalid(dbh,node_parprevblockhash n,Int64.sub blkh 1L)::cwl)
	    else if DbBlacklist.dbexists dbh then
	      get_bestnode_r2 ctipr ctipsr (ConsensusWarningBlacklist(dbh,node_parprevblockhash n,Int64.sub blkh 1L)::cwl)
	    else
	      match !vs with
	      | ValidBlock -> (n,cwl)
	      | InvalidBlock ->
		  get_bestnode_r2 ctipr ctipsr (ConsensusWarningInvalid(dbh,node_parprevblockhash n,Int64.sub blkh 1L)::cwl)
	      | Waiting(tm,_) ->
		  get_bestnode_r2 ctipr ctipsr (ConsensusWarningWaiting(dbh,node_parprevblockhash n,Int64.sub blkh 1L,tm,DbBlockHeader.dbexists dbh,DbBlockDelta.dbexists dbh)::cwl)
	  in
	  let handle_exc comm =
	    begin
	      if DbInvalidatedBlocks.dbexists dbh then
		get_bestnode_r2 ctipr ctipsr (ConsensusWarningInvalid(dbh,None,-1L)::cwl)
	      else
		try
		  let (_,oprev) = find_dalilcoin_header_ltc_burn dbh in
		  try
		    get_bestnode_r2 ctipr ctipsr (ConsensusWarningMissing(dbh,oprev,-1L,DbBlockHeader.dbexists dbh,DbBlockDelta.dbexists dbh,comm)::cwl)
		  with Not_found ->
		    Printf.fprintf !log "Not_found raised by get_bestnode_r2, probably indicating a bug.\n";
		    raise (Failure("Not_found raised by get_bestnode_r2, probably indicating a bug."));
		with Not_found ->
		  Printf.fprintf !log "WARNING: No burn for %s although seems to be a tip, this should not have happened so there must be a bug.\n" (hashval_hexstring dbh);
		  get_bestnode_r2 ctipr ctipsr (ConsensusWarningNoBurn(dbh)::cwl)
	    end
	  in
          try
	    handle_node (Hashtbl.find blkheadernode (Some(dbh)))
	  with
	  | Not_found -> (*** if we have the header, we may have simply not yet built the node of the block tree, try to build it here ***)
	      begin
		if DbBlockHeader.dbexists dbh then
		  begin
		    try
		      add_known_header_to_blocktree dbh;
		      handle_node (Hashtbl.find blkheadernode (Some(dbh)))
		    with e -> handle_exc (Printexc.to_string e)
		  end
		else
		  handle_exc "Recent burned header not found; May be out of sync" (*** assume we don't have the header yet if the node has not been created; add a consensus warning and continue ***)
	      end
	  | NoReq -> handle_exc "not allowed to request"
	  | GettingRemoteData -> handle_exc "requested from remote node"
	  | e -> handle_exc (Printexc.to_string e)
	end
  and get_bestnode_r ctipsl cwl =
    match ctipsl with
    | [] ->
	let tm = ltc_medtime() in
	if tm > Int64.add !Config.genesistimestamp 604800L then
	  begin
	    raise (Failure "cannot find best validated header; probably out of sync")
	  end
	else
	  (!genesisblocktreenode,cwl)
    | ctips::ctipsr ->
	get_bestnode_r2 ctips ctipsr cwl
  in
  let cwl =
    let tm = ltc_medtime() in
    if ctips0l = [] && tm > Int64.add !Config.genesistimestamp 604800L then
      [ConsensusWarningTerminal]
    else
      []
  in
  get_bestnode_r ctips0l cwl
and create_new_node h req =
  try
    create_new_node_a h req
  with
  | Not_found ->
      Printf.fprintf !log "create_new_node called with %s but no such entry is in HeaderLtcBurn\n" (hashval_hexstring h);
      flush !log;
      raise (Failure "problem in create_new_node")
  | NoReq ->
      Printf.fprintf !log "do not have block header or delta for %s and cannot currently request it\n" (hashval_hexstring h);
      flush !log;
      raise (Failure "delaying create_new_node")
  | GettingRemoteData ->
      Printf.fprintf !log "requesting block header or delta for %s\n" (hashval_hexstring h);
      flush !log;
      raise (Failure "delaying create_new_node")
and create_new_node_a h req =
  let (pob,_) = find_dalilcoin_header_ltc_burn h in
  create_new_node_b h pob req
and create_new_node_b h pob req =
  try
    let hh = hashval_hexstring h in
    if hh = !Config.lastcheckpoint && Sys.file_exists (Filename.concat (datadir()) ("checkpoint_" ^ hh)) then
      begin
	let checkpointfile = open_in_bin (Filename.concat (datadir()) ("checkpoint_" ^ hh)) in
	let fnode : blocktree = input_value checkpointfile in
	close_in checkpointfile;
	fnode
      end
    else
      begin
	let (bhd,bhs) = DbBlockHeader.dbget h in
	if DbBlockDelta.dbexists h then
	  let newcsm = poburn_stakemod pob in
	  let pbh = fstohash bhd.prevblockhash in
	  let (par,blkh,chlr) =
	    match pbh with
	    | Some(pbh) ->
		let par = get_or_create_node pbh req in
		(Some(par),node_blockheight par,node_children_ref par)
	    | None ->
		(Some(!genesisblocktreenode),1L,node_children_ref !genesisblocktreenode)
	  in
	  let par = if hh = !Config.lastcheckpoint then None else par in
	  let fnode = BlocktreeNode(par,ref [],Some(h,pob),bhd.newtheoryroot,bhd.newsignaroot,bhd.newledgerroot,newcsm,bhd.tinfo,bhd.timestamp,zero_big_int,Int64.add blkh 1L,ref ValidBlock,ref false,ref []) in
	  if hh = !Config.lastcheckpoint then
	    begin
	      let checkpointfile = open_out_bin (Filename.concat (datadir()) ("checkpoint_" ^ hh)) in
	      output_value checkpointfile fnode;
	      close_out checkpointfile;
	    end;
	  chlr := (h,fnode)::!chlr;
	  Hashtbl.add blkheadernode (Some(h)) fnode;
	  possibly_handle_orphan h fnode false false;
	  fnode
	else if not req then
	  raise NoReq
	else
	  begin
	    Printf.fprintf !log "trying to request delta %s\n" (hashval_hexstring h);
	    try
	      find_and_send_requestdata GetBlockdelta h;
	      raise GettingRemoteData
	    with Not_found ->
	      Printf.fprintf !log "not connected to a peer with delta %s\n" (hashval_hexstring h);
	      raise Exit
	  end
      end
  with Not_found ->
    if not req then
      raise NoReq
    else
      begin
        Printf.fprintf !log "trying to request header %s\n" (hashval_hexstring h);
        try
	  find_and_send_requestdata GetHeader h;
	  raise GettingRemoteData
        with Not_found ->
	  Printf.fprintf !log "not connected to a peer with header %s\n" (hashval_hexstring h);
          Printf.printf "not connected to a peer with delta %s\n" (hashval_hexstring h);
	  raise Exit
      end
and process_delta_real h blkhght blk =
  let (blkh,blkdel) = blk in
  List.iter
    (fun stau ->
      let txid = hashstx stau in
      DbSTx.dbput txid stau)
    blkdel.blockdelta_stxl;
  begin
    let prevc = load_expanded_ctree (ctree_of_block blk) in
    let (cstk,txl) = txl_of_block blk in (*** the coinstake tx is performed last, i.e., after the txs in the block. ***)
    try
      match tx_octree_trans false false blkhght cstk (txl_octree_trans false false blkhght txl (Some(prevc))) with (*** "false false" disallows database lookups and remote requests ***)
      | Some(newc) -> ignore (save_ctree_elements newc)
      | None -> raise (Failure("transformed tree was empty, although block seemed to be valid"))
    with MaxAssetsAtAddress -> raise (Failure("transformed tree would hold too many assets at an address"))
  end
and get_or_create_node h req =
  try
    Hashtbl.find blkheadernode (Some(h))
  with Not_found ->
    create_new_node h req
and validate_block_of_node newnode thyroot sigroot csm tinf blkhght h blkdel cs =
  let blkdelroot = blockdelta_hashroot blkdel in
  let (blkhd,_) as blkh = DbBlockHeader.dbget h in
  let BlocktreeNode(_,_,_,tr2,sr2,_,csm2,tinf2,_,newcumulstake,blkhght2,vs,_,chlr) = newnode in
  if not (blkdelroot = blkhd.blockdeltaroot) then (*** if even this fails, then the peer is misbehaving and sending a blockdelta that does not correspond to the header. In this case, ban the peer, drop the connection, and request it from someone else. ***)
    begin
      let b = Buffer.create 1000 in
      seosbf (seo_blockdelta seosb blkdel (b,None));
      Printf.fprintf !log "Bad block delta received for %s; full delta = %s\n" (hashval_hexstring h) (string_hexstring (Buffer.contents b));
      let tm = Unix.time() in
      cs.banned <- true;
      Hashtbl.add bannedpeers cs.addrfrom ();
      vs := Waiting(tm,None)
    end
  else
    let (Poburn(lblkh,ltxh,lmedtm,burned),_) = find_dalilcoin_header_ltc_burn h in
    let blk = (blkh,blkdel) in
    if known_thytree_p thyroot && known_sigtree_p sigroot then (*** these should both be known if the parent block has been validated ***)
      begin
	let thytree = lookup_thytree thyroot in
	let sigtree = lookup_sigtree sigroot in
	Printf.fprintf !log "About to check if block %s at height %Ld is valid\n" (hashval_hexstring h) blkhght;
	match valid_block thytree sigtree blkhght csm tinf blk lmedtm burned with
	| Some(tht2,sigt2) ->
	    vs := ValidBlock;
	    Hashtbl.remove tovalidate h;
	    processing_deltas := h::!processing_deltas;
	    DbBlockDelta.dbput h blkdel;
	    process_delta_real h blkhght blk;
	    let (bn,cwl) = get_bestnode true in
	    let BlocktreeNode(_,_,_,_,_,_,_,_,_,bestcumulstk,_,_,_,_) = bn in
	    update_theories thyroot thytree tht2;
	    update_signatures sigroot sigtree sigt2;
	    (*** construct a transformed tree consisting of elements ***)
	    processing_deltas := List.filter (fun k -> not (k = h)) !processing_deltas;
	    broadcast_inv [(int_of_msgtype Blockdelta,h)];
	    List.iter
	      (fun (h,n) ->
		let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,vs,_,_) = n in
		match !vs with
		| Waiting(_,Some(blkdel,cs)) -> validate_block_of_node n tr2 sr2 csm2 tinf2 blkhght2 h blkdel cs
		| _ -> ())
	      !chlr
	| None -> (*** We can mark it as invalid because we know this is the only delta that could support the header. ***)
	    let b = Buffer.create 1000 in
	    seosbf (seo_blockdelta seosb blkdel (b,None));
	    Printf.fprintf !log "Block delta for %s was invalid; full delta = %s\n" (hashval_hexstring h) (string_hexstring (Buffer.contents b));
	    let tm = Unix.time() in
	    cs.banned <- true;
	    Hashtbl.add bannedpeers cs.addrfrom ();
	    vs := InvalidBlock
      end
    else
      raise (Failure("parent was validated but thyroot and/or sigroot is not known"))
and process_new_header_a h hh blkh1 blkhd1 blkhs1 initialization knownvalid =
  try
    process_new_header_aa h hh blkh1 blkhd1 blkhs1 (blockheader_stakeasset blkhd1) initialization knownvalid
  with
  | HeaderStakedAssetNotMin ->
      Printf.fprintf !log "Header %s has extra information beyong supporting staked asset; should have been caught before process_new_header_a\n" hh;
      raise (Failure "header does not only support staked asset")
  | HeaderNoStakedAsset ->
      Printf.fprintf !log "Header %s does not support staked asset; should have been caught before process_new_header_a\n" hh;
      raise (Failure "header does not support staked asset")
and process_new_header_aa h hh blkh1 blkhd1 blkhs1 a initialization knownvalid =
  if valid_blockheader_signat blkh1 a then
    let (pob,prevbh) = find_dalilcoin_header_ltc_burn h in
    process_new_header_ab h hh blkh1 blkhd1 blkhs1 a initialization knownvalid pob
  else
    begin
      Printf.fprintf !log "Header %s has an invalid signature; should have been caught before process_new_header_aa\n" hh;
      raise (Failure "header has invalid signature")
    end
and process_new_header_ab h hh blkh1 blkhd1 blkhs1 a initialization knownvalid pob =
  let prevblkh = fstohash blkhd1.prevblockhash in
  begin
    try
      let prevnode = Hashtbl.find blkheadernode prevblkh in
      begin
	try
	  let Poburn(_,_,lmedtm,burned) = pob in
	  let BlocktreeNode(_,_,prevh,thyroot,sigroot,ledgerroot,csm,currtinfo,tmstamp,prevcumulstk,blkhght,validated,blacklisted,succl) = prevnode in
	  if !blacklisted then (*** child of a blacklisted node, drop and blacklist it ***)
            begin
	      let newcsm = poburn_stakemod pob in
	      Printf.fprintf !log "Header %s is child of blacklisted node; deleting and blacklisting it.\n" hh;
              let newnode = BlocktreeNode(Some(prevnode),ref [],Some(h,pob),blkhd1.newtheoryroot,blkhd1.newsignaroot,blkhd1.newledgerroot,newcsm,blkhd1.tinfo,blkhd1.timestamp,zero_big_int,Int64.add blkhght 1L,ref InvalidBlock,ref true,ref []) in (*** dummy node just to remember it is blacklisted ***)
	      Hashtbl.add blkheadernode (Some(h)) newnode;
	      possibly_handle_orphan h newnode initialization knownvalid;
              DbBlacklist.dbput h true;
(*	      DbBlockHeader.dbdelete h; *) (* do not delete header in case we want to inspect or reconsider it *)
            end
	  else if
	    valid_blockheader blkhght csm currtinfo blkh1 lmedtm burned
              && 
	    blockheader_succ_a ledgerroot tmstamp currtinfo blkh1
	  then
	    begin
	      if not (DbBlockHeader.dbexists h) then DbBlockHeader.dbput h (blkhd1,blkhs1);
	      Hashtbl.add blkheaders h ();
	      broadcast_inv [(int_of_msgtype Headers,h)];
	      let validated = ref (if knownvalid then ValidBlock else Waiting(Unix.time(),None)) in
	      let newcsm = poburn_stakemod pob in
	      let newnode = BlocktreeNode(Some(prevnode),ref [blkhd1.stakeaddr],Some(h,pob),blkhd1.newtheoryroot,blkhd1.newsignaroot,blkhd1.newledgerroot,newcsm,blkhd1.tinfo,blkhd1.timestamp,zero_big_int,Int64.add blkhght 1L,validated,ref false,ref []) in
	      (*** add it as a leaf, indicate that we want the block delta to validate it, and check if it's the best ***)
	      Hashtbl.add blkheadernode (Some(h)) newnode;
	      succl := (h,newnode)::!succl;
	      possibly_handle_orphan h newnode initialization knownvalid;
	      begin
		try
		  let blkdel = DbBlockDelta.dbget h in
		  let blk = (blkh1,blkdel) in
		  if known_thytree_p thyroot && known_sigtree_p sigroot then (*** these should both be known if the parent block has been validated ***)
		    begin
		      let thytree = lookup_thytree thyroot in
		      let sigtree = lookup_sigtree sigroot in
		      match valid_block thytree sigtree blkhght csm currtinfo blk lmedtm burned with
		      | Some(tht2,sigt2) ->
			  validated := ValidBlock;
			  update_theories thyroot thytree tht2;
			  update_signatures sigroot sigtree sigt2
		      | None -> (*** should not have happened, delete it from the database and request it again. ***)
			  DbBlockDelta.dbdelete h;
			  Hashtbl.add tovalidate h ();
                          if DbBlockHeader.dbexists h then
                            begin
                              try
                                find_and_send_requestdata GetBlockdelta h
			      with Not_found ->
                                Printf.fprintf !log "No source for block delta of %s; must wait until it is explicitly requested\n" hh
                            end
                          else
                            begin
                              try
                                Printf.fprintf !log "Do not have header for %s; trying to request it.\n" hh;
                                find_and_send_requestdata GetHeader h
                              with Not_found ->
                                Printf.fprintf !log "No source for block header of %s\n" hh
                            end
		    end
		  else
		    raise (Failure "unknown thyroot or sigroot while trying to validate block")
		with Not_found ->
		  Hashtbl.add tovalidate h ();
                  if DbBlockHeader.dbexists h then
                    begin
                      try
                        find_and_send_requestdata GetBlockdelta h
                      with Not_found ->
                        Printf.fprintf !log "No source for block delta of %s; must wait until it is explicitly requested\n" hh
                    end
                  else
                    begin
                      try
                        Printf.fprintf !log "Do not have header for %s; trying to request it.\n" hh;
                        find_and_send_requestdata GetHeader h
                      with Not_found ->
                        Printf.fprintf !log "No source for block header of %s\n" hh
                    end
	      end
	    end
	  else
	    let newcsm = poburn_stakemod pob in
	    begin (*** if it's wrong, delete it and blacklist it so it won't look new in the future [note: signature is assumed to have been checked to be valid by now] ***)
	      Printf.fprintf !log "Header %s was invalid, deleting and blacklisting it.\n" hh;
	      Printf.fprintf !log "vbh %Ld %s %b\n" blkhght (targetinfo_string currtinfo) (valid_blockheader blkhght csm currtinfo blkh1 lmedtm burned);
	      Printf.fprintf !log "bhsa %s %Ld %s %b\n" (hashval_hexstring ledgerroot) tmstamp (targetinfo_string currtinfo) (blockheader_succ_a ledgerroot tmstamp currtinfo blkh1);
	      verbose_blockcheck := Some(!log);
	      ignore (valid_blockheader blkhght csm currtinfo blkh1 lmedtm burned);
	      ignore (blockheader_succ_a ledgerroot tmstamp currtinfo blkh1);
	      verbose_blockcheck := None;
              let newnode = BlocktreeNode(Some(prevnode),ref [],Some(h,pob),blkhd1.newtheoryroot,blkhd1.newsignaroot,blkhd1.newledgerroot,newcsm,blkhd1.tinfo,blkhd1.timestamp,zero_big_int,Int64.add blkhght 1L,ref InvalidBlock,ref true,ref []) in (*** dummy node just to remember it is blacklisted ***)
	      Hashtbl.add blkheadernode (Some(h)) newnode;
	      possibly_handle_orphan h newnode initialization knownvalid;
              DbBlacklist.dbput h true;
(*	      DbBlockHeader.dbdelete h; *) (* do not delete header in case we want to inspect or reconsider it *)
            end
	with Not_found ->
	  Printf.fprintf !log "unexpected Not_found in process_new_header_a %s\n" hh;
	  raise (Failure "unexpected Not_found in process_new_header_a")
      end
    with Not_found -> (*** orphan block header, put it on the relevant hash table and possibly request parent ***)
      match prevblkh with
      | Some(parblkh) ->
	  begin
	    let parblkhh = hashval_hexstring parblkh in
	    Printf.fprintf !log "Got header %s before parent %s; keeping as orphan and possibly requesting parent\n" hh parblkhh;
	    Hashtbl.add orphanblkheaders prevblkh (h,blkh1);
	    try
	      if DbBlockHeader.dbexists parblkh then
		process_new_header_b parblkh parblkhh initialization knownvalid
	      else
		find_and_send_requestdata GetHeader parblkh
	    with Not_found -> Printf.fprintf !log "no peer has parent header %s\n" parblkhh
	  end
      | None ->
	  begin
	    Printf.fprintf !log "Bug: Block header %s is marked as a genesis block, but there is no root to the blocktree.\n" hh
	  end
  end
and process_new_header_b h hh initialization knownvalid =
  Printf.fprintf !log "Processing new header %s\n" hh; flush !log;
  try
    let (blkhd1,blkhs1) = DbBlockHeader.dbget h in
    let blkh1 = (blkhd1,blkhs1) in
    if not (blockheader_id blkh1 = h) then (*** wrong hash, remove it but don't blacklist the (wrong) id ***)
      begin
        Printf.fprintf !log "WARNING: Block header in database has different hash than key, removing %s\nThis must be due to a bug.\n" hh; flush !log;
	DbBlockHeader.dbdelete h;
      end
    else
      process_new_header_a h hh blkh1 blkhd1 blkhs1 initialization knownvalid
  with (*** in some cases, failure should lead to blacklist and removal of the header, but it's not clear which cases; if it's in a block we might need to distinguish between definitely incorrect vs. possibly incorrect ***)
  | Not_found ->
      Printf.fprintf !log "Problem with blockheader %s\n" hh; flush !log;
(*      DbBlockHeader.dbdelete h; *) (* do not delete header in case we want to inspect or reconsider it *)
  | e ->
      Printf.fprintf !log "exception %s\n" (Printexc.to_string e); flush !log;
      ()
and process_new_header h hh initialization knownvalid =
  if not (Hashtbl.mem blkheaders h) then
    process_new_header_b h hh initialization knownvalid
and possibly_handle_orphan h n initialization knownvalid =
  List.iter
    (fun (k,bh) ->
      let (bhd,bhs) = bh in
      process_new_header_a k (hashval_hexstring k) bh bhd bhs initialization knownvalid;
      try
	let kn = Hashtbl.find blkheadernode (Some(k)) in
	let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,_,blklstd,chl) = n in
	chl := (k,kn)::!chl;
	Hashtbl.remove orphanblkheaders (Some(h))
      with Not_found -> ())
    (Hashtbl.find_all orphanblkheaders (Some(h)))
and add_known_header_to_blocktree h =
  let bh = DbBlockHeader.dbget h in
  let (bhd,bhs) = bh in
  begin
    match bhd.prevblockhash with
    | None -> ()
    | Some(pbh,_) ->
	if not (hashval_hexstring pbh = !Config.lastcheckpoint) then
	  add_known_header_to_blocktree pbh
	else
	  ()
  end;
  process_new_header_a h (hashval_hexstring h) bh bhd bhs false true

(*** during initialization, go as far back as necessary in history ***)
let rec traverse_ltc_history lb =
  try
    let (prevh,tm,hght,burntxhs) = DbLtcBlock.dbget lb in
    let stopatcheckpoint = ref false in
    let makenodes = ref [] in
    List.iter
      (fun burntxh ->
	try
	  let (burned,lprevtx,dnxt) = DbLtcBurnTx.dbget burntxh in
	  if hashval_hexstring dnxt = !Config.lastcheckpoint then stopatcheckpoint := true;
	  if (DbBlockHeader.dbexists dnxt) then
	    makenodes := (dnxt,(Poburn(lb,burntxh,tm,burned)))::!makenodes
	  else if not (DbBlockHeader.dbexists dnxt || DbBlacklist.dbexists dnxt || DbArchived.dbexists dnxt) then
	    missingheaders := dnxt :: !missingheaders (*** earliest headers should be earlier on the missingheaders list ***)
	with _ -> ())
      burntxhs;
    if not !stopatcheckpoint && not (!Config.ltcblockcheckpoint = hashval_hexstring prevh) then traverse_ltc_history prevh; (*** allow for a checkpoint to avoid needing to go back too far ***)
    List.iter (fun (h,pob) -> try ignore (create_new_node_b h pob false) with _ -> ()) !makenodes
  with Not_found -> ()

let process_delta h =
  let bh = DbBlockHeader.dbget h in
  let bd = DbBlockDelta.dbget h in
  process_delta_real h (Int64.sub (node_blockheight (Hashtbl.find blkheadernode (Some(h)))) 1L) (bh,bd)

let rec init_headers_to h =
  if DbInvalidatedBlocks.dbexists h || DbBlacklist.dbexists h then
    Printf.fprintf !log "Skipping invalidated or blacklisted header %s" (hashval_hexstring h)
  else
    try
      let (blkhd1,blkhs1) as blkh1 = DbBlockHeader.dbget h in
      begin
	match blkhd1.prevblockhash with
	| Some(ph,_) -> init_headers_to ph
	| None -> ()
      end;
      process_new_header_a h (hashval_hexstring h) blkh1 blkhd1 blkhs1 true false
    with Not_found ->
      Printf.printf "Could not find header %s\nStarting node without initializing headers.\n" (hashval_hexstring h)

let rec init_headers_l ltxh =
  if not (ltxh = (0l,0l,0l,0l,0l,0l,0l,0l)) then
    begin
      try
	begin
	  try
	    let (burned,lprevtx,dnxt) = DbLtcBurnTx.dbget ltxh in
	    if DbBlockHeader.dbexists dnxt then
	      init_headers_to dnxt
	    else
	      init_headers_l lprevtx
	  with Not_found ->
	    let (burned,lprevtx,dnxt,lblkh,confs) = ltc_gettransactioninfo (hashval_hexstring ltxh) in
	    if DbBlockHeader.dbexists dnxt then
	      init_headers_to dnxt
	    else
	      init_headers_l lprevtx
	end
      with _ -> ()
    end

let init_headers h =
  if DbBlockHeader.dbexists h then
    init_headers_to h
  else
    try
      let (Poburn(lblkh,ltxh,lmedtm,burned),prevbh) = find_dalilcoin_header_ltc_burn h in
      init_headers_l ltxh
    with Not_found ->
      Printf.printf "Could not find ltc burn for header %s\nStarting node without initializing headers.\n" (hashval_hexstring h)

let rec blacklist_from h n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,_,blklstd,chl) = n in
  blklstd := true;
  DbBlacklist.dbput h true;
  DbBlockHeader.dbdelete h;
  List.iter (fun (k,c) -> blacklist_from k c) !chl

let publish_stx txh stx1 =
  let (tx1,txsigs1) = stx1 in
  if not (Hashtbl.mem stxpool txh) then Hashtbl.add stxpool txh stx1;
  DbSTx.dbput txh stx1;
  Hashtbl.add published_stx txh ();
  broadcast_inv [(int_of_msgtype STx,txh)]

let publish_block blkh bhh (bh,bd) =
  Printf.fprintf !log "publishing block %s\n" (hashval_hexstring bhh); flush !log;
  broadcast_inv [(int_of_msgtype Headers,bhh);(int_of_msgtype Blockdelta,bhh)];;

let initblocktree () =
  genesisblocktreenode := BlocktreeNode(None,ref [],None,None,None,!genesisledgerroot,!genesisstakemod,!genesistarget,!Config.genesistimestamp,zero_big_int,1L,ref ValidBlock,ref false,ref []);
  lastcheckpointnode := !genesisblocktreenode;
  Hashtbl.add blkheadernode None !genesisblocktreenode;
  try
    traverse_ltc_history !ltc_bestblock;
    if !missingheaders = [] then
      ignore (get_bestnode true)
    else
      begin
	Printf.fprintf !log "%d headers are missing.\n" (List.length !missingheaders);
	find_and_send_requestmissingheaders()
      end
  with
  | _ -> ();;

Hashtbl.add msgtype_handler GetHeader
  (fun (sin,sout,cs,ms) ->
    let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
    let i = int_of_msgtype GetHeader in
    let tm = Unix.time() in
    if recently_sent (i,h) tm cs.sentinv then (*** don't resend ***)
      begin
	Printf.fprintf !log "recently sent header %s to %s; not resending\n" (hashval_hexstring h) cs.addrfrom;
	flush !log
      end
    else
      try
	let (bhd,bhs) as bh = DbBlockHeader.dbget h in
	let s = Buffer.create 1000 in
	Printf.fprintf !log "sending header %s to %s upon request at time %f (GetHeader)\n" (hashval_hexstring h) cs.addrfrom (Unix.time());
	seosbf (seo_blockheader seosb bh (seo_hashval seosb h (seo_int8 seosb 1 (s,None))));
	cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv;
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
    let tm = Unix.time() in
    let i = int_of_msgtype GetHeader in
    for j = 1 to n do
      let (h,cn) = sei_hashval seis !c in
      c := cn;
      if recently_sent (i,h) tm cs.sentinv then (*** don't resend ***)
	begin
	  Printf.fprintf !log "recently sent header %s to %s; not resending\n" (hashval_hexstring h) cs.addrfrom;
	  flush !log
	end
      else
	try
	  let (blkhd1,blkhs1) as bh = DbBlockHeader.dbget h in
	  if not (blockheader_id bh = h) then
	    Printf.fprintf !log "Serious bug: not sending blockheader %s since it does not have correct id but instead %s\n" (hashval_hexstring h) (hashval_hexstring (blockheader_id bh))
	  else
	    begin
	      incr m;
	      bhl := (h,bh)::!bhl;
	      Printf.fprintf !log "sending header %s to %s upon request at time %f (GetHeaders)\n" (hashval_hexstring h) cs.addrfrom (Unix.time());
	      cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv
	    end;
	with
	| Not_found ->
	  (*** don't have it to send, ignore ***)
	    ()
	| e -> (** ignore any other exception ***)
	    Printf.fprintf !log "unexpected exception when handling GetHeaders: %s\n" (Printexc.to_string e)
      done;
    let s = Buffer.create 10000 in
    Printf.fprintf !log "sending %d headers\n" !m;
    let co = ref (seo_int8 seosb !m (s,None)) in
    List.iter (fun (h,bh) -> co := seo_blockheader seosb bh (seo_hashval seosb h !co)) !bhl;
    seosbf !co;
    let ss = Buffer.contents s in
    ignore (queue_msg cs Headers ss)
  );;

let deserialize_exc_protect cs f =
  try
    f()
  with e ->
    Printf.fprintf !log "Deserialization exception: %s\nDisconnecting and banning node %s\n" (Printexc.to_string e) cs.realaddr;
    flush !log;
    cs.banned <- true;
    raise e;;

Hashtbl.add msgtype_handler Headers
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
    Printf.fprintf !log "get %d Headers\n" n; flush !log;
    c := cn;
    let tm = Unix.time() in
    let i = int_of_msgtype GetHeader in
    for j = 1 to n do
      let (h,cn) = sei_hashval seis !c in
      let (bh,cn) = deserialize_exc_protect cs (fun () -> sei_blockheader seis cn) in (*** deserialize if only to get to the next one ***)
      c := cn;
      Printf.fprintf !log "Headers msg %d %s at time %f\n"j (hashval_hexstring h) tm;
      if not (DbBlockHeader.dbexists h) &&
	((try ignore (find_dalilcoin_header_ltc_burn h); true with Not_found -> false) || recently_requested (i,h) tm cs.invreq)
      then
	let (bhd,bhs) = bh in
	if not (blockheader_id bh = h) then
	  begin (*** this may be the result of a misbehaving peer ***)
	    Printf.fprintf !log "got a header with the wrong hash, dropping it and banning node\n";
	    let b = Buffer.create 1000 in
	    seosbf (seo_blockheader seosb (bhd,bhs) (b,None));
	    Printf.fprintf !log "given id h = %s header with wrong id = %s\n" (hashval_hexstring h) (string_hexstring (Buffer.contents b));
	    flush !log;
	    cs.banned <- true
	  end
	else
	  begin
	    missingheaders := List.filter (fun mh -> not (mh = h)) !missingheaders; (*** remove from missing list ***)
	    try
	      let a = blockheader_stakeasset bhd in
	      let validsofar = ref true in
	      let dontban = ref false in
	      begin
		match bhd.prevblockhash with
		| None -> ()
		| Some(prevh,Poburn(lblkh,ltxh,lmedtm,burned)) -> (* commitment to previous proof of burn *)
		    try
		      let (Poburn(lblkh2,ltxh2,lmedtm2,burned2),_) = find_dalilcoin_header_ltc_burn prevh in (* previous proof of burn *)
		      if not (lblkh = lblkh2) then
			begin
			  Printf.fprintf !log "Rejecting incoming header %s since ltc block hash mismatch in poburn: %s vs %s\n" (hashval_hexstring h) (hashval_hexstring lblkh) (hashval_hexstring lblkh2);
			  validsofar := false
			end;
		      if not (ltxh = ltxh2) then
			begin
			  Printf.fprintf !log "Rejecting incoming header %s since ltc tx hash mismatch in poburn: %s vs %s\n" (hashval_hexstring h) (hashval_hexstring ltxh) (hashval_hexstring ltxh2);
			  validsofar := false
			end;
		      if not (lmedtm = lmedtm2) then
			begin
			  Printf.fprintf !log "Rejecting incoming header %s since ltc median time mismatch in poburn: %Ld vs %Ld\n" (hashval_hexstring h) lmedtm lmedtm2;
			  validsofar := false
			end;
		      if not (burned = burned2) then
			begin
			  Printf.fprintf !log "Rejecting incoming header %s since ltc burn amount mismatch in poburn: %Ld vs %Ld\n" (hashval_hexstring h) burned burned2;
			  validsofar := false
			end;
		    with Not_found ->
		      Printf.fprintf !log "Rejecting incoming header %s (without banning or blacklisting) since no corresponding ltc burn of previous %s.\n" (hashval_hexstring h) (hashval_hexstring prevh);
		      validsofar := false;
		      dontban := true
	      end;
	      if not !validsofar then
		begin
		  Printf.fprintf !log "Got invalid header %s\n" (hashval_hexstring h);
		  let b = Buffer.create 1000 in
		  seosbf (seo_blockheader seosb (bhd,bhs) (b,None));
		  Printf.fprintf !log "full invalid header for %s = %s\n" (hashval_hexstring h) (string_hexstring (Buffer.contents b));
		  flush !log;
		  if not !dontban then cs.banned <- true
		end
	      else if not (valid_blockheader_signat (bhd,bhs) a) then
		begin (*** this may simply be the result of a misbehaving peer mangling the signature of an otherwise valid header ***)
		  Printf.printf "Got invalid header %s (signature invalid)\n" (hashval_hexstring h);
		  let b = Buffer.create 1000 in
		  seosbf (seo_blockheader seosb (bhd,bhs) (b,None));
		  Printf.fprintf !log "full invalid header for %s = %s\n" (hashval_hexstring h) (string_hexstring (Buffer.contents b));
		  flush !log;
		  if not !dontban then cs.banned <- true
		end
	      else
		try
		  let (pob,_) = find_dalilcoin_header_ltc_burn h in
		  process_new_header_ab h (hashval_hexstring h) bh bhd bhs a false false pob
		with Not_found -> (*** before the pob has been completed; should not have been requested yet ***)
		  Printf.fprintf !log "Header %s was requested and received before the proof-of-burn was confirmed; ignoring it and waiting\n" (hashval_hexstring h);
		  flush !log
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
    | h::hr ->
	let i = int_of_msgtype GetHeader in
	let tm = Unix.time() in
	cs.invreq <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.invreq;
	req_header_batches sout cs (m+1) hr (h::nw)
    | [] -> req_headers sout cs m nw;;

Hashtbl.add msgtype_handler Inv
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let hl = ref [] in
    let (n,cn) = sei_int32 seis !c in
    Printf.fprintf !log "Inv msg %ld entries\n" n;
    c := cn;
    for j = 1 to Int32.to_int n do
      let ((i,h),cn) = sei_prod sei_int8 sei_hashval seis !c in
      c := cn;
      cs.rinv <- (i,h)::cs.rinv;
      if i = int_of_msgtype Headers then Printf.fprintf !log "Headers, dbexists %b, archived %b\n" (DbBlockHeader.dbexists h) (DbArchived.dbexists h);
      Printf.fprintf !log "Inv %d %s\n" i (hashval_hexstring h);
      if i = int_of_msgtype Headers && not (DbArchived.dbexists h) then
	begin
	  try
	    let (blkhd1,blkhs1) as bh = DbBlockHeader.dbget h in
	    if not (Hashtbl.mem blkheadernode (Some(h))) then
	      let (bhd,bhs) = bh in
	      process_new_header_a h (hashval_hexstring h) bh blkhd1 blkhs1 false false
	  with Not_found ->
	    try
	      ignore (find_dalilcoin_header_ltc_burn h); (*** only request headers after a pob is completed ***)
	      hl := h::!hl;
	    with Not_found ->
	      ()
	end
      else if i = int_of_msgtype Blockdelta && (DbBlockHeader.dbexists h) && not (DbBlockDelta.dbexists h) && not (DbArchived.dbexists h) && not (DbBlacklist.dbexists h) && Hashtbl.mem tovalidate h then
	begin
	  try
	    let tm = Unix.time() in
            cs.invreq <- (int_of_msgtype GetBlockdelta,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.invreq;
	    let s = Buffer.create 1000 in
	    seosbf (seo_hashval seosb h (s,None));
	    Printf.fprintf !log "Immediately requesting blockdelta %s\n" (hashval_hexstring h);
	    ignore (queue_msg cs GetBlockdelta (Buffer.contents s))
	  with exn -> Printf.fprintf !log "inv blockdelta %s\n" (Printexc.to_string exn)
	end
      else if i = int_of_msgtype STx && not (DbArchived.dbexists h) then
	begin
	  if not (DbSTx.dbexists h) && not (Hashtbl.mem stxpool h) then
 	    begin
	      let tm = Unix.time() in
              cs.invreq <- (int_of_msgtype GetSTx,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.invreq;
              let s = Buffer.create 1000 in
	      seosbf (seo_hashval seosb h (s,None));
	      Printf.fprintf !log "Sending GetSTx %s to %s at %f\n" (hashval_hexstring h) cs.realaddr tm;
	      ignore (queue_msg cs GetSTx (Buffer.contents s))
	    end
	end
    done;
    req_header_batches sout cs 0 !hl []);;

Hashtbl.add msgtype_handler GetBlockdelta
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetBlockdelta in
      let tm = Unix.time() in
      if recently_sent (i,h) tm cs.sentinv then (*** don't resend ***)
	begin
	  Printf.fprintf !log "recently sent delta %s to %s; not resending\n" (hashval_hexstring h) cs.addrfrom;
	  flush !log
	end
      else
	try
	  let blkdel = DbBlockDelta.dbget h in
	  let bdsb = Buffer.create 100 in
	  seosbf (seo_blockdelta seosb blkdel (seo_hashval seosb h (bdsb,None)));
	  let bdser = Buffer.contents bdsb in
	  ignore (queue_msg cs Blockdelta bdser);
	  cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv
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
	    let tm = Unix.time() in
	    cs.invreq <- List.filter (fun (j,k,tm0) -> not (i = j && h = k) && tm -. tm0 < 3600.0) cs.invreq;
	    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,vs,_,chlr) as newnode =
	      try
		Hashtbl.find blkheadernode (Some(h))
	      with Not_found ->
		create_new_node h true
	    in
	    match !vs with
	    | Waiting(tm,None) ->
		begin
		  match par with
		  | None -> (*** genesis node, parent implicitly valid ***)
		      let (blkdel,_) = deserialize_exc_protect cs (fun () -> sei_blockdelta seis r) in
		      validate_block_of_node newnode None None !genesisstakemod !genesistarget 1L h blkdel cs
		  | Some(BlocktreeNode(_,_,_,thyroot,sigroot,_,csm,tinf,_,_,blkhght,vsp,_,_)) ->
		      match !vsp with
		      | InvalidBlock -> raise Not_found
		      | Waiting(_,_) ->
			  let (blkdel,_) = deserialize_exc_protect cs (fun () -> sei_blockdelta seis r) in
			  vs := Waiting(tm,Some(blkdel,cs)) (*** wait for the parent to be validated; remember the connstate in case we decide to ban it for giving a bad block delta ***)
		      | ValidBlock -> (*** validate now, and if valid check if children nodes are waiting to be validated ***)
			  begin
			    let (blkdel,_) = deserialize_exc_protect cs (fun () -> sei_blockdelta seis r) in
			    validate_block_of_node newnode thyroot sigroot csm tinf blkhght h blkdel cs
			  end
		end
	    | _ -> ()
	  with e ->
	    Printf.fprintf !log "Problem handling Blockdelta, presumably with getting corresponding blocktree node: %s\n" (Printexc.to_string e)
	end);;

Hashtbl.add msgtype_handler GetSTx
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetSTx in
      let tm = Unix.time() in
      if not (recently_sent (i,h) tm cs.sentinv) then (*** don't resend ***)
	try
	  let stau = Hashtbl.find stxpool h in
	  let stausb = Buffer.create 100 in
	  seosbf (seo_stx seosb stau (seo_hashval seosb h (stausb,None)));
	  let stauser = Buffer.contents stausb in
	  Printf.fprintf !log "Sending Signed Tx (from pool) %s\n" (hashval_hexstring h);
	  ignore (queue_msg cs STx stauser);
	  cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv
	with Not_found ->
	  try
	    let stau = DbSTx.dbget h in
	    let stausb = Buffer.create 100 in
	    seosbf (seo_stx seosb stau (seo_hashval seosb h (stausb,None)));
	    let stauser = Buffer.contents stausb in
	    Printf.fprintf !log "Sending Signed Tx (from db) %s\n" (hashval_hexstring h);
	    ignore (queue_msg cs STx stauser);
	    cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv
	  with Not_found ->
	    Printf.fprintf !log "Unknown Tx %s\n" (hashval_hexstring h);
	    ());;

let add_to_txpool txid stau =
  Hashtbl.add stxpool txid stau;
  let ((txin,_),_) = stau in
  List.iter (fun (_,h) -> Hashtbl.add unconfirmed_spent_assets h txid) txin

let remove_from_txpool txid =
  try
    let stau = Hashtbl.find stxpool txid in
    Hashtbl.remove stxpool txid;
    let ((txin,_),_) = stau in
    List.iter (fun (_,h) -> Hashtbl.remove unconfirmed_spent_assets h) txin
  with Not_found -> ()

let savetxtopool_real txid stau =
  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 (Filename.concat (datadir()) "txpool") in
  seocf (seo_prod seo_hashval seo_stx seoc (txid,stau) (ch,None));
  close_out ch;;

Hashtbl.add msgtype_handler STx
    (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetSTx in
      let tm = Unix.time() in
      Printf.fprintf !log "Got Signed Tx %s from %s at %f\n" (hashval_hexstring h) cs.realaddr tm;
      if not (DbSTx.dbexists h) && not (Hashtbl.mem stxpool h) then (*** if we already have it, abort ***)
	if recently_requested (i,h) tm cs.invreq then (*** only continue if it was requested ***)
          let (((tauin,tauout) as tau,_) as stau,_) = deserialize_exc_protect cs (fun () -> sei_stx seis r) in
	  if hashstx stau = h then
	    if tx_valid tau then
	      begin
		try
		  let (n,_) = get_bestnode false in (*** ignore consensus warnings here ***)
		  let BlocktreeNode(_,_,_,tr,sr,lr,_,_,_,_,blkh,_,_,_) = n in
		  let unsupportederror alpha k = Printf.fprintf !log "Could not find asset %s at address %s in ledger %s; throwing out tx %s\n" (hashval_hexstring k) (Cryptocurr.addr_daliladdrstr alpha) (hashval_hexstring lr) (hashval_hexstring h) in
		  let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr)) unsupportederror) in
		  if tx_signatures_valid blkh al stau then
		    begin
		      let nfee = ctree_supports_tx true false (lookup_thytree tr) (lookup_sigtree sr) blkh tau (CHash(lr)) in
		      let fee = Int64.sub 0L nfee in
		      if fee >= !Config.minrelayfee then
			begin
			  Hashtbl.add stxpool h stau;
			  Printf.fprintf !log "Accepting tx %s into pool\n" (hashval_hexstring h);
			  flush !log;
			  add_to_txpool h stau;
			  savetxtopool_real h stau
			end
		      else
			(Printf.fprintf !log "ignoring tx %s with low fee of %s fraenks (%Ld cants)\n" (hashval_hexstring h) (Cryptocurr.fraenks_of_cants fee) fee; flush !log)
		    end
		  else
		    (Printf.fprintf !log "ignoring tx %s since signatures are not valid at the current block height of %Ld\n" (hashval_hexstring h) blkh; flush !log)
		with _ ->
		  (Printf.fprintf !log "Tx %s is unsupported by the local ledger, dropping it.\n" (hashval_hexstring h); flush !log)
	      end
	    else
	      (Printf.fprintf !log "misbehaving peer? [invalid Tx %s]\n" (hashval_hexstring h); flush !log)
          else (*** otherwise, it seems to be a misbehaving peer --  ignore for now ***)
	    (Printf.fprintf !log "misbehaving peer? [malformed Tx]\n"; flush !log)
	else (*** if something unrequested was sent, then seems to be a misbehaving peer ***)
	  (Printf.fprintf !log "misbehaving peer? [unrequested Tx %s]\n" (hashval_hexstring h); flush !log));;

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
  Printf.fprintf sa "=========\ntovalidate:\n";
  Hashtbl.iter
    (fun h () ->
      Printf.fprintf sa "%s\n" (hashval_hexstring h))
    tovalidate;;

let print_best_node () =
  let (bn,cwl) = get_bestnode true in
  let BlocktreeNode(_,_,_,pbh,_,_,_,_,_,_,_,_,_,_) = bn in
  match pbh with
  | Some(h) -> Printf.fprintf !log "bestnode pbh %s\n" (hashval_hexstring h); flush !log
  | None -> Printf.fprintf !log "bestnode pbh (genesis)\n"; flush !log

let rec recursively_invalidate_children n =
  let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,vs,_,chlr) = n in
  vs := InvalidBlock;
  List.iter
    (fun (h,ch) ->
      if not (DbInvalidatedBlocks.dbexists h) then DbInvalidatedBlocks.dbput h true;
      recursively_invalidate_children ch)
    !chlr

let recursively_invalidate_blocks h =
  DbInvalidatedBlocks.dbput h true;
  try
    let BlocktreeNode(_,_,_,_,_,_,_,_,_,_,_,vs,_,chlr) = Hashtbl.find blkheadernode (Some(h)) in
    vs := InvalidBlock;
    List.iter
      (fun (h,ch) ->
	if not (DbInvalidatedBlocks.dbexists h) then DbInvalidatedBlocks.dbput h true;
	recursively_invalidate_children ch)
      !chlr
  with _ -> ()

let rec recursively_revalidate_parents n =
  let BlocktreeNode(par,_,pbh,_,_,_,_,_,_,_,_,vs,_,_) = n in
  vs := ValidBlock;
  begin
    match pbh with
    | Some(h,_) -> if DbInvalidatedBlocks.dbexists h then DbInvalidatedBlocks.dbdelete h
    | None -> ()
  end;
  match par with
  | Some(p) -> recursively_revalidate_parents p
  | None -> ()

let recursively_revalidate_blocks h =
  DbInvalidatedBlocks.dbdelete h;
  try
    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,vs,_,chlr) = Hashtbl.find blkheadernode (Some(h)) in
    vs := ValidBlock;
    match par with
    | Some(p) -> recursively_revalidate_parents p
    | None -> ()
  with _ -> ()

let reprocessblock oc h =
  try
    let bh = DbBlockHeader.dbget h in
    try
      let bd = DbBlockDelta.dbget h in
      let (bhd,bhs) = bh in
      let pbh = bhd.prevblockhash in
      try
	let n =
	  match pbh with
	  | Some(h,_) -> Hashtbl.find blkheadernode (Some(h))
	  | None -> Hashtbl.find blkheadernode None
	in
	let BlocktreeNode(_,_,_,thyroot,sigroot,_,csm,currtinfo,_,_,blkhght,vsp,_,_) = n in
	try
	  let thytree = lookup_thytree thyroot in
	  try
	    let sigtree = lookup_sigtree sigroot in
	    try
	      let (Poburn(lblkh,ltxh,lmedtm,burned),_) = find_dalilcoin_header_ltc_burn h in
	      match valid_block thytree sigtree blkhght csm currtinfo (bh,bd) lmedtm burned with
	      | Some(tht2,sigt2) ->
		  vsp := ValidBlock;
		  update_theories thyroot thytree tht2;
		  update_signatures sigroot sigtree sigt2
	      | None -> (*** should not have happened, delete it from the database and request it again. ***)
		  vsp := InvalidBlock;
		  Printf.fprintf oc "Invalid block %s\n" (hashval_hexstring h)
	    with _ ->
	      Printf.fprintf oc "Do not have proof of burn for block %s\n" (hashval_hexstring h);
	      flush oc
	  with Not_found ->
	    Printf.fprintf oc "Could not find signature tree for block\n";
	    flush oc
	with Not_found ->
	  Printf.fprintf oc "Could not find theory tree for block\n";
	  flush oc
      with Not_found ->
	Printf.fprintf oc "Could not find information for parent block %s\n"
	  (match pbh with Some(h,_) -> (hashval_hexstring h) | None -> "(genesis)");
	flush oc
    with Not_found ->
      Printf.fprintf oc "Do not have delta for block %s\n" (hashval_hexstring h);
      flush oc
  with Not_found ->
    Printf.fprintf oc "Do not have header for block %s\n" (hashval_hexstring h);
    flush oc
