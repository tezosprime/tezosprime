(* Copyright (c) 2015-2017 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json;;
open Big_int;;
open Utils;;
open Ser;;
open Sha256;;
open Ripemd160;;
open Hashaux;;
open Hash;;
open Net;;
open Db;;
open Secp256k1;;
open Signat;;
open Cryptocurr;;
open Mathdata;;
open Assets;;
open Tx;;
open Ctre;;
open Ctregraft;;
open Block;;
open Blocktree;;
open Ltcrpc;;
open Setconfig;;

let get_reward_locktime blkh =
  let rl =
    match !Config.reward_lock_relative with
    | None -> reward_locktime
    | Some(rl) -> if rl > reward_locktime then rl else reward_locktime
  in
  let m = Int64.add blkh rl in
  match !Config.reward_lock_absolute with
  | None -> m
  | Some(a) -> if a > m then a else m

let rec pblockchain s n c lr m =
  let BlocktreeNode(par,_,pbh,_,_,plr,csm,tar,tm,_,blkh,_,_,chl) = n in
  if m > 0 then
    begin
      match par with
      | Some(p) -> pblockchain s p pbh (Some(plr)) (m-1)
      | None -> ()
    end;
  Printf.fprintf s "Target: %s\n" (string_of_big_int tar);
  Printf.fprintf s "Difficulty: %s\n" (string_of_big_int (difficulty tar));
  Printf.fprintf s "Timestamp: %Ld\n" tm;
  match c with
  | Some(h,Poburn(lblkh,ltxh,lmedtm,burned)) ->
      Printf.fprintf s "Burned %Ld at median time %Ld with ltc tx %s in block %s\n" burned lmedtm (hashval_hexstring ltxh) (hashval_hexstring lblkh);
      List.iter (fun (k,_) -> if not (k = h) then Printf.fprintf s "[orphan %s]\n" (hashval_hexstring k)) !chl;
      begin
	match lr with
	| Some(lr) ->
	    Printf.fprintf s "block %Ld %s (post block ledger %s)\n" blkh (hashval_hexstring h) (hashval_hexstring lr)
	| None ->
	    Printf.fprintf s "block %Ld %s\n" blkh (hashval_hexstring h)
      end;
  | None ->
      List.iter (fun (k,_) -> Printf.fprintf s "[extra child, not yet considered best %s]\n" (hashval_hexstring k)) !chl

let print_consensus_warning s cw =
  match cw with
  | ConsensusWarningMissing(h,ph,blkh,hh,hd,comm) ->
      Printf.fprintf s "Missing Block %Ld %s%s%s%s %s\n"
	blkh (hashval_hexstring h)
	(match ph with None -> "" | Some(ph) -> Printf.sprintf " (succ of %s)" (hashval_hexstring ph))
	(if hh then " Have Header" else " Missing Header")
	(if hd then " Have Delta" else " Missing Delta")
	comm
  | ConsensusWarningWaiting(h,ph,blkh,tm,hh,hd) ->
      Printf.fprintf s "Waiting to Validate Block %Ld %s%s%s%s\n"
	blkh (hashval_hexstring h)
	(match ph with None -> "" | Some(ph) -> Printf.sprintf " (succ of %s)" (hashval_hexstring ph))
	(if hh then " Have Header" else " Missing Header")
	(if hd then " Have Delta" else " Missing Delta")
  | ConsensusWarningBlacklist(h,ph,blkh) ->
      Printf.fprintf s "Blacklisted Block %Ld %s%s\n"
	blkh (hashval_hexstring h)
	(match ph with None -> "" | Some(ph) -> Printf.sprintf " (succ of %s)" (hashval_hexstring ph))
  | ConsensusWarningInvalid(h,ph,blkh) ->
      Printf.fprintf s "Invalid Block %Ld %s%s\n"
	blkh (hashval_hexstring h)
	(match ph with None -> "" | Some(ph) -> Printf.sprintf " (succ of %s)" (hashval_hexstring ph))
  | ConsensusWarningNoBurn(h) ->
      Printf.fprintf s "BUG: Mystery unburned block %s\n" (hashval_hexstring h)
  | ConsensusWarningTerminal ->
      Printf.fprintf s "No blocks were created in the past week. Dalilcoin has reached terminal status.\nThe only recovery possible for the network is a hard fork.\n"
	
let get_bestnode_print_warnings s req =
  let (n,cwl) = get_bestnode req in
  if not (cwl = []) then
    begin
      let bh = ref (match node_prevblockhash n with Some(pbh,_) -> Some(pbh) | None -> None) in
      let cwlnew = ref [] in
      let cwlorph = ref [] in
      List.iter
	    (fun cw ->
	      match cw with
	      | ConsensusWarningMissing(h,ph,blkh,hh,hd,_) ->
		  if ph = !bh then (bh := Some(h); cwlnew := cw::!cwlnew) else cwlorph := cw::!cwlorph
	      | ConsensusWarningWaiting(h,ph,blkh,tm,hh,hd) ->
		  if ph = !bh then (bh := Some(h); cwlnew := cw::!cwlnew) else cwlorph := cw::!cwlorph
	      | ConsensusWarningBlacklist(h,ph,blkh) ->
		  if ph = !bh then (bh := Some(h); cwlnew := cw::!cwlnew) else cwlorph := cw::!cwlorph
	      | ConsensusWarningInvalid(h,ph,blkh) ->
		  if ph = !bh then (bh := Some(h); cwlnew := cw::!cwlnew) else cwlorph := cw::!cwlorph
	      | ConsensusWarningNoBurn(h) -> cwlorph := cw::!cwlorph
	      | ConsensusWarningTerminal ->
		  Printf.printf "No blocks were created in the past week. Dalilcoin has reached terminal status.\nThe only recovery possible for the network is a hard fork.\n")
	cwl;
      if not (!cwlnew = []) then
	begin
	  Printf.printf "WARNING: %d new blocks seem to have been mined on top of the current best block:\n" (List.length !cwlnew);
	  List.iter (print_consensus_warning s) (List.rev !cwlnew);
	end;
      if not (!cwlorph = []) then
	begin
	  Printf.printf "WARNING: %d blocks are possible orphans:\n" (List.length !cwlorph);
	  List.iter (print_consensus_warning s) (List.rev !cwlorph);
	end;
    end;
  n

let dumpstate fa =
  let sa = open_out fa in
  Printf.fprintf sa "=========\nNetwork connections: %d\n" (List.length !netconns);
  List.iter
    (fun (lth,sth,(fd,sin,sout,gcs)) ->
      match !gcs with
      | None -> Printf.fprintf sa "[Dead Connection]\n";
      | Some(cs) ->
	  Printf.fprintf sa "-----------\nConnection: %s %f\n" cs.realaddr cs.conntime;
	  Printf.fprintf sa "peertimeskew %d\nprotvers %ld\nuseragent %s\naddrfrom %s\nbanned %s\nlastmsgtm %f\nfirst_header_height %Ld\nfirst_full_height %Ld\nlast_height %Ld\n" cs.peertimeskew cs.protvers cs.useragent cs.addrfrom (if cs.banned then "true" else "false") cs.lastmsgtm cs.first_header_height cs.first_full_height cs.last_height;
	  Printf.fprintf sa "- pending %d:\n" (List.length cs.pending);
	  List.iter
	    (fun (h,(b,tm1,tm2,f)) ->
	      Printf.fprintf sa "%s %s %f %f\n" (hashval_hexstring h) (if b then "true" else "false") tm1 tm2
	    )
	    cs.pending;
	  Printf.fprintf sa "sentinv %d:\n" (List.length cs.sentinv);
	  List.iter
	    (fun (m,h,tm) ->
	      Printf.fprintf sa "%d %s %f\n" m (hashval_hexstring h) tm)
	    cs.sentinv;
	  Printf.fprintf sa "rinv %d:\n" (List.length cs.rinv);
	  List.iter
	    (fun (m,h) ->
	      Printf.fprintf sa "%d %s\n" m (hashval_hexstring h))
	    cs.rinv;
	  Printf.fprintf sa "invreq %d:\n" (List.length cs.invreq);
	  List.iter
	    (fun (m,h,tm) ->
	      Printf.fprintf sa "%d %s %f\n" m (hashval_hexstring h) tm)
	    cs.invreq;
    )
    !netconns;
  Printf.fprintf sa "=================\nBlock Chain:\n";
  (try pblockchain sa (get_bestnode_print_warnings sa true) None None 10000 with _ -> ());
  dumpblocktreestate sa;
  close_out sa

let exitfn : (int -> unit) ref = ref (fun n -> exit n);;

let lock datadir =
  let lf = Filename.concat datadir "lock" in
  let c = open_out lf in
  close_out c;
  exitfn := (fun n -> saveknownpeers(); save_processing_deltas(); Sys.remove lf; exit n);;

let stkth : Thread.t option ref = ref None;;

let initnetwork () =
  begin
    try
      match !Config.ip with
      | Some(ip) ->
	  let l = openlistener ip !Config.port 5 in
	  Printf.printf "Listening for incoming connections.\n";
	  flush stdout;
	  netlistenerth := Some(Thread.create netlistener l)
      | None ->
	  Printf.printf "Not listening for incoming connections.\nIf you want Dalilcoin to listen for incoming connections set ip to your ip address\nusing ip=... in dalilcoin.conf or -ip=... on the command line.\n";
	  flush stdout
    with _ -> ()
  end;
  netseeker ();
  (*** empty placeholder for now ***)
  ();;

let missingheaders_th : Thread.t option ref = ref None;;

let missingheadersthread () = (*** every five minutes check if there are headers known to be missing and if so try to request them ***)
  while true do
    try
      if not (!missingheaders = []) then find_and_send_requestmissingheaders();
      Thread.delay 300.0
    with _ ->
      Thread.delay 300.0
  done;;

let ltc_listener_th : Thread.t option ref = ref None;;

let ltc_init () =
  if !Config.testnet then ltctestnet();
  try
    Printf.fprintf !log "syncing with ltc\n";
    let lbh = ltc_getbestblockhash () in
    Printf.fprintf !log "ltc bestblock %s\n" lbh;
    ltc_process_block lbh;
    ltc_bestblock := hexstring_hashval lbh;
    Printf.fprintf !log "finished initial syncing with ltc, now checking for new blocks\n";
    let lbh = ltc_getbestblockhash () in
    Printf.fprintf !log "ltc bestblock %s\n" lbh;
    ltc_process_block lbh;
    ltc_bestblock := hexstring_hashval lbh;
    Printf.fprintf !log "finished syncing with ltc\n";
  with exc ->
    Printf.fprintf !log "problem syncing with ltc. %s quitting.\n" (Printexc.to_string exc);
    Printf.printf "problem syncing with ltc. quitting.\n";
    !exitfn 2

let ltc_listener () =
  while true do
    try
      let lbh = ltc_getbestblockhash () in
      ltc_process_block lbh;
      ltc_bestblock := hexstring_hashval lbh;
      Thread.delay 60.0
    with _ ->
      Thread.delay 120.0
  done;;

type nextstakeinfo = NextStake of (int64 * p2pkhaddr * hashval * int64 * obligation * int64 * int64 option * (hashval * hashval) option ref) | NoStakeUpTo of int64;;

let nextstakechances : (hashval option,nextstakeinfo) Hashtbl.t = Hashtbl.create 100;;
let nextstakechances_hypo : (hashval option,nextstakeinfo) Hashtbl.t = Hashtbl.create 100;;
let nextstakechances_checkedtotm : (hashval option,int64) Hashtbl.t = Hashtbl.create 100;;

let stakingassetsmutex = Mutex.create();;

let compute_recid (r,s) k =
  match smulp k _g with
  | Some(x,y) ->
      if eq_big_int x r then
	if evenp y then 0 else 1
      else
	if evenp y then 2 else 3
  | None -> raise (Failure "bad0");;

let rec hlist_stakingassets blkh alpha hl n =
  if n > 0 then
    match hl with
    | HCons((aid,bday,obl,Currency(v)),hr) ->
	let ca = coinage blkh bday obl v in
	Printf.fprintf !log "Checking asset %s %Ld %Ld %s %Ld %s\n" (hashval_hexstring aid) blkh bday (obligation_string obl) v (string_of_big_int ca);
	if gt_big_int ca zero_big_int && not (Hashtbl.mem unconfirmed_spent_assets aid) then
	  begin
	    Printf.fprintf !log "Staking asset: %s\n" (hashval_hexstring aid);
	    Mutex.lock stakingassetsmutex;
	    Commands.stakingassets := (alpha,aid,bday,obl,v)::!Commands.stakingassets;
	    Mutex.unlock stakingassetsmutex;
	  end;
	hlist_stakingassets blkh alpha hr (n-1)
    | HCons(_,hr) -> hlist_stakingassets blkh alpha hr (n-1)
    | HConsH(h,hr) ->
	begin
	  try
	    hlist_stakingassets blkh alpha (HCons(DbAsset.dbget h,hr)) n
	  with Not_found -> ()
	end
    | HHash(h,_) ->
	begin
	  try
	    let (h1,h2) = DbHConsElt.dbget h in
	    match h2 with
	    | Some(h2,l2) -> hlist_stakingassets blkh alpha (HConsH(h1,HHash(h2,l2))) n
	    | None -> hlist_stakingassets blkh alpha (HConsH(h1,HNil)) n
	  with Not_found -> ()
	end
    | _ -> ()
  else
    ();;

let extraburn : int64 ref = ref 0L;;

let lastburn : int64 ref = ref 0L;;

let allowedburn tm =
  if !extraburn > 0L then
    true
  else if !lastburn > 0L then
    let sinceburn = Int64.sub tm !lastburn in
    sinceburn >= !Config.mintimebetweenburns
  else
    true

let maxburnnow tm =
  let mbn =
    if !lastburn > 0L then
      let sinceburn = Int64.sub tm !lastburn in
      if sinceburn < !Config.mintimebetweenburns then
	0L
      else
	min !Config.maxburn (Int64.div (Int64.mul !Config.maxburnrate (Int64.sub tm !lastburn)) 86400L)
    else
      !Config.maxburn
  in
  max !extraburn (if mbn >= !Config.ltctxfee then Int64.sub mbn !Config.ltctxfee else 0L)

let fstohash a =
  match a with
  | None -> None
  | Some(h,_) -> Some(h)

let compute_staking_chances n fromtm totm =
  let BlocktreeNode(par,children,prevblk,thyroot,sigroot,currledgerroot,csm1,tar1,tmstamp,prevcumulstk,blkhght,validated,blacklisted,succl) = n in
  let prevblkh = fstohash prevblk in
  let fromtm =
    try
      Hashtbl.find nextstakechances_checkedtotm prevblkh
    with Not_found -> fromtm
  in
  let i = ref (max fromtm (Int64.add 1L tmstamp)) in
  if !Config.maxburn < 0L then (*** if must burn but not willing to burn, don't bother computing next staking chances ***)
    ()
  else
    let c = CHash(currledgerroot) in
    (*** collect assets allowed to stake now ***)
    Commands.stakingassets := [];
    let minburntostake = ref None in
    Printf.fprintf !log "Collecting staking assets in ledger %s (block height %Ld).\n" (hashval_hexstring currledgerroot) blkhght;
    List.iter
      (fun (k,b,(x,y),w,h,alpha) ->
	match try ctree_addr true true (p2pkhaddr_addr h) c None with _ -> (None,0) with
	| (Some(hl),_) ->
            hlist_stakingassets blkhght h (nehlist_hlist hl) 30
	| _ ->
	    ())
      !Commands.walletkeys;
    List.iter
      (fun (alpha,beta,_,_,_,_) ->
	let (p,x4,x3,x2,x1,x0) = alpha in
	let (q,_,_,_,_,_) = beta in
	if not p && not q then (*** only p2pkh can stake ***)
	  match try ctree_addr true true (payaddr_addr alpha) c None with _ -> (None,0) with
	  | (Some(hl),_) ->
	      hlist_stakingassets blkhght (x4,x3,x2,x1,x0) (nehlist_hlist hl) 50
	  | _ -> ())
      !Commands.walletendorsements;
    Printf.fprintf !log "%d staking assets\n" (List.length !Commands.stakingassets); flush !log;
    if not (!Commands.stakingassets = []) then
      let nextstake i stkaddr h bday obl v toburn =
	let deltm = Int64.to_int32 (Int64.sub i tmstamp) in
	Hashtbl.add nextstakechances prevblkh (NextStake(i,stkaddr,h,bday,obl,v,toburn,ref None));
	raise Exit
      in
      try
	while !i < totm do
	  i := Int64.add 1L !i;
	  (*** go through assets and check for staking at time !i ***)
	  List.iter
	    (fun (stkaddr,h,bday,obl,v) ->
	      let v2 = Int64.add v (Int64.mul (maxburnnow !i) 1000L) in
              (** Printf.fprintf !log "Checking for staking of %s at time %Ld\n" (hashval_hexstring h) !i; flush !log; **)
	      let caf = coinagefactor blkhght bday obl in
	      if gt_big_int (mult_big_int caf (big_int_of_int64 v2)) zero_big_int then
		begin
		  let hv = hitval !i h csm1 in
		  let mtar = mult_big_int tar1 caf in
		  let minv = succ_big_int (div_big_int hv mtar) in
		  let toburn = succ_big_int (succ_big_int (div_big_int (sub_big_int minv (big_int_of_int64 v)) (big_int_of_int 1000))) in
		  if lt_big_int zero_big_int toburn && lt_big_int toburn (big_int_of_string "1000000000") then (*** 10 ltc limit for reporting staking chances ***)
		    begin
		      match !minburntostake with
		      | None ->
			  Hashtbl.add nextstakechances_hypo prevblkh (NextStake(!i,stkaddr,h,bday,obl,v,Some(int64_of_big_int toburn),ref None));
			  minburntostake := Some(toburn,!i,stkaddr,h)
		      | Some(mburn,_,_,_) ->
			  if lt_big_int toburn mburn then
			    begin
			      Hashtbl.add nextstakechances_hypo prevblkh (NextStake(!i,stkaddr,h,bday,obl,v,Some(int64_of_big_int toburn),ref None));
			      minburntostake := Some(toburn,!i,stkaddr,h)
			    end
		    end;
		  if lt_big_int minv (big_int_of_int64 v) then (*** hit without burn ***)
		    (if allowedburn !i then
		      nextstake !i stkaddr h bday obl v (Some(0L))) (*** burn nothing, but announce in the pow chain (ltc) ***)
		  else if allowedburn !i then
		    if lt_big_int toburn (big_int_of_int64 (maxburnnow !i)) then (*** hit with burn ***)
		      nextstake !i stkaddr h bday obl v (Some(int64_of_big_int toburn))
		end
	    )
	    !Commands.stakingassets
	done;
	Printf.fprintf !log "No staking chances up to time %Ld\n" totm; flush !log;
	Hashtbl.add nextstakechances (fstohash prevblk) (NoStakeUpTo(totm));
      with
      | Exit -> ()
      | exn ->
	  Printf.fprintf !log "Unexpected Exception in Staking Loop: %s\n" (Printexc.to_string exn); flush stdout
	    
exception StakingPause of float
exception StakingProblemPause

let get_bestnode_cw_exception req e =
  let (best,cwl) = get_bestnode req in
  begin
    try
      let cw =
	List.find
	  (fun cw ->
	    match cw with
	    | ConsensusWarningMissing(h,ph,blkh,hh,hd,_) -> true
	    | ConsensusWarningWaiting(h,ph,blkh,tm,hh,hd) -> true
	    | _ -> false)
	  cwl
      in
      print_consensus_warning !log cw;
      Printf.fprintf !log "possibly not synced; delaying staking\n";
      flush !log;
      raise Exit
    with
    | Not_found -> ()
    | Exit -> raise e
  end;
  best

let pendingltctxs = ref [];;

let stakingthread () =
  let sleepuntil = ref (ltc_medtime()) in
  while true do
    try
      let sleeplen = Int64.to_float (Int64.sub !sleepuntil (ltc_medtime())) in
      Printf.fprintf !log "Staking sleeplen %f seconds\n" sleeplen;
      if sleeplen > 1.0 then Thread.delay sleeplen;
      Printf.fprintf !log "Staking after sleeplen %f seconds\n" sleeplen;
      if not (ltc_synced()) then (Printf.fprintf !log "ltc not synced yet; delaying staking\n"; flush !log; raise (StakingPause(60.0)));
      pendingltctxs := List.filter (fun h -> not (ltc_tx_confirmed h)) !pendingltctxs;
      if not (!pendingltctxs = []) then (Printf.fprintf !log "there are pending ltc txs; delaying staking\n"; flush !log; raise (StakingPause(60.0)));
      let best = get_bestnode_cw_exception false (StakingPause(300.0)) in
      try
	let pbhh = node_prevblockhash best in
	let pbhh1 = fstohash pbhh in
	let blkh = node_blockheight best in
        match Hashtbl.find nextstakechances pbhh1 with
	| NextStake(tm,alpha,aid,bday,obl,v,toburn,already) ->
	    begin
	      match !already with
	      | Some(_,_) -> raise (StakingPause(60.0))
	      | None ->
		  begin
		    let nw = ltc_medtime() in
		    let pbhtm = node_timestamp best in
		    Printf.fprintf !log "NextStake tm = %Ld nw = %Ld\n" tm nw; flush !log;
		    if tm >= Int64.add nw 60L || tm <= pbhtm then
		      begin (*** wait for a minute and then reevaluate; would be better to sleep until time to publish or until a new best block is found **)
			let tmtopub = Int64.sub tm nw in
			output_string !log ((Int64.to_string tmtopub) ^ " seconds until time to publish staked block\n");
			flush !log;
			if tmtopub >= 60L then
			  sleepuntil := Int64.add nw 60L
			else
			  begin
			    sleepuntil := Int64.add nw tmtopub;
			  end
		      end
		    else
		      begin (** go ahead and form the block; then publish it at the right time **)
			let prevledgerroot = node_ledgerroot best in
			let csm0 = node_stakemod best in
			let tar0 = node_targetinfo best in
			let deltm = Int64.to_int32 (Int64.sub tm pbhtm) in
			let tar = retarget tar0 deltm in
			let alpha2 = p2pkhaddr_addr alpha in
			Printf.fprintf !log "Forming new block at height %Ld with prevledgerroot %s, prev block %s and new stake addr %s stake aid %s (bday %Ld).\n" blkh (hashval_hexstring prevledgerroot) (match pbhh1 with Some(h) -> hashval_hexstring h | None -> "[none]") (addr_daliladdrstr alpha2) (hashval_hexstring aid) bday;
			let obl2 =
			  match obl with
			  | None ->  (* if the staked asset had the default obligation it can be left as the default obligation or locked for some number of blocks to commit to staking; there should be a configurable policy for the node *)
			      None
(**
   Some(p2pkhaddr_payaddr alpha,Int64.add blkh (Int64.logand 2048L (rand_int64())),false) (* it is not marked as a reward *)
 **)
			  | _ -> obl (* unless it's the default obligation, then the obligation cannot change when staking it *)
			in
			let prevc = Some(CHash(prevledgerroot)) in
			let octree_ctree c =
			  match c with
			  | Some(c) -> c
			  | None -> raise (Failure "tree should not be empty")
			in
			let dync = ref (octree_ctree prevc) in
			let dyntht = ref (lookup_thytree (node_theoryroot best)) in
			let dynsigt = ref (lookup_sigtree (node_signaroot best)) in
			let fees = ref 0L in
			let otherstxs = ref [] in
			let rembytesestimate = ref (maxblockdeltasize blkh - (2048 * 2)) in (*** estimate the remaining room in the block delta if the tx is added ***)
			Hashtbl.iter
			  (fun h ((tauin,tauout),sg) ->
(*		    Printf.fprintf !log "Trying to include tx %s\n" (hashval_hexstring h); flush stdout; *)
			    try
			      ignore (List.find (fun (_,h) -> h = aid) tauin);
(*		      Printf.fprintf !log "tx spends the staked asset; removing tx from pool\n"; flush !log; *)
			      remove_from_txpool h
			    with Not_found ->
			      try
				ignore (List.find (fun (alpha,_) -> alpha = alpha2) tauout) (*** Do not include txs that spend to the staking address, to avoid the possibility of ending up with too many assets at the stakign address ***)
			      with Not_found ->
				if tx_valid (tauin,tauout) then
				  try
				    let unsupportederror alpha h = Printf.fprintf !log "Could not find asset %s at address %s\n" (hashval_hexstring h) (addr_daliladdrstr alpha) in
				    let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin !dync unsupportederror) in
				    if tx_signatures_valid blkh al ((tauin,tauout),sg) then
				      begin
					let nfee = ctree_supports_tx true false !dyntht !dynsigt blkh (tauin,tauout) !dync in
					if nfee > 0L then
					  begin
(*				  Printf.fprintf !log "tx %s has negative fees %Ld; removing from pool\n" (hashval_hexstring h) nfee;
  flush !log; *)
					    remove_from_txpool h;
					  end
					else
					  let bytesestimate = 2048 * List.length tauin + 2048 * List.length tauout in (*** simple 2K per input and output (since must include relevant parts of ctree) ***)
					  if bytesestimate < !rembytesestimate then
					    begin
					      try
						let c = octree_ctree (tx_octree_trans true false blkh (tauin,tauout) (Some(!dync))) in
						otherstxs := (h,((tauin,tauout),sg))::!otherstxs;
						fees := Int64.sub !fees nfee;
						dync := c;
						dyntht := txout_update_ottree tauout !dyntht;
						dynsigt := txout_update_ostree tauout !dynsigt;
						rembytesestimate := !rembytesestimate - bytesestimate
					      with MaxAssetsAtAddress -> ()
					    end
					  else
					    begin
(*				    Printf.fprintf !log "tx %s not being included because estimated block size would be too big (rembytesestimate %d, bytesestimate %d)\n" (hashval_hexstring h) !rembytesestimate bytesestimate;
  flush !log *)
					    end
				      end
				    else
				      begin
(*			      Printf.fprintf !log "tx %s has an invalid signature; removing from pool\n" (hashval_hexstring h);
  flush !log; *)
					remove_from_txpool h;
				      end
				  with exn ->
				    begin
(*			    Printf.fprintf !log "Exception %s raised while trying to validate tx %s; this may mean the tx is not yet supported so leaving it in the pool\n" (Printexc.to_string exn) (hashval_hexstring h);
  flush !log; *)
				    end
				else
				  begin
(*			  Printf.fprintf !log "tx %s is invalid; removing from pool\n" (hashval_hexstring h);
  flush !log; *)
				    remove_from_txpool h;
				  end)
			  stxpool;
			let ostxs = !otherstxs in
			let otherstxs = ref [] in
			List.iter
			  (fun (h,stau) ->
			    remove_from_txpool h;
			    otherstxs := stau::!otherstxs)
			  ostxs;
			let othertxs = List.map (fun (tau,_) -> tau) !otherstxs in
			let alpha3 =
			  if !Config.generatenewstakingaddresses then
			    (let (_,alpha3) = Commands.generate_newkeyandaddress prevledgerroot in alpha3) (*** prevent staking address from ending up holding too many assets; max 32 are allowed ***)
			  else
			    let (i,x0,x1,x2,x3,x4) = alpha2 in
			    if i = 0 then
			      (x0,x1,x2,x3,x4)
			    else
			      begin
				Printf.fprintf !Utils.log "Apparent attempt to stake from non-p2pkh address %s\n" (addr_daliladdrstr alpha2);
				flush !Utils.log;
				raise StakingProblemPause
			      end
			in
			let alpha4 =
			  match !Config.offlinestakerewardskey with
			  | None -> p2pkhaddr_payaddr alpha3
			  | Some(x) ->
			      try
				let (i,x0,x1,x2,x3,x4) = daliladdrstr_addr x in
				if i = 0 then
				  (false,x0,x1,x2,x3,x4) (*** p2pkh ***)
				else if i = 1 then
				  (true,x0,x1,x2,x3,x4) (*** p2sh ***)
				else
				  p2pkhaddr_payaddr alpha3
			      with _ -> p2pkhaddr_payaddr alpha3
			in
			let stkoutl = [(alpha2,(obl2,Currency(v)));(p2pkhaddr_addr alpha3,(Some(alpha4,get_reward_locktime blkh,true),Currency(Int64.add !fees (rewfn blkh))))] in
			let coinstk : tx = ([(alpha2,aid)],stkoutl) in
			try
			  dync := octree_ctree (tx_octree_trans true false blkh coinstk (Some(!dync)));
			  let prevcforblock =
			    match
			      get_txl_supporting_octree (coinstk::othertxs) prevc
			    with
			    | Some(c) -> c
			    | None -> raise (Failure "ctree should not have become empty")
			  in
			  if not (ctree_hashroot prevcforblock = prevledgerroot) then
			    begin
			      Printf.fprintf !log "prevcforblock has the wrong hash root. This should never happen.\n";
			      let s = Buffer.create 10000 in
			      seosbf (seo_option seo_ctree seosb prevc (s,None));
			      Printf.fprintf !log "prevc: %s\n" (Hashaux.string_hexstring (Buffer.contents s));
			      let s = Buffer.create 10000 in
			      seosbf (seo_ctree seosb prevcforblock (s,None));
			      Printf.fprintf !log "prevcforblock: %s\nprevledgerroot: %s\n" (Hashaux.string_hexstring (Buffer.contents s)) (hashval_hexstring prevledgerroot);
			      let s = Buffer.create 10000 in
			      seosbf (seo_list seo_tx seosb (coinstk::othertxs) (s,None));
			      Printf.fprintf !log "txs: %s\n" (Hashaux.string_hexstring (Buffer.contents s));
			      flush !log;
			      Hashtbl.remove nextstakechances pbhh1;
			      raise StakingProblemPause;
			    end;
			  let (prevcforheader,cgr) = factor_inputs_ctree_cgraft [(alpha2,aid)] prevcforblock in
			  let newcr = save_ctree_elements !dync in
(*		Printf.fprintf !log "finished saving ctree elements of dync\n"; flush !log; *)
(*		    Hashtbl.add recentledgerroots newcr (blkh,newcr); *)
			  let newthtroot = ottree_hashroot !dyntht in
			  let newsigtroot = ostree_hashroot !dynsigt in
(*		Printf.fprintf !log "Including %d txs in block\n" (List.length !otherstxs); *)
			  let bdnew : blockdelta =
			    { stakeoutput = stkoutl;
			      prevledgergraft = cgr;
			      blockdelta_stxl = !otherstxs
			    }
			  in
			  let bdnewroot = blockdelta_hashroot bdnew in
			  let bhdnew : blockheaderdata
			      = { prevblockhash = pbhh;
				  newtheoryroot = newthtroot;
				  newsignaroot = newsigtroot;
				  newledgerroot = newcr;
				  stakeaddr = alpha;
				  stakeassetid = aid;
				  timestamp = tm;
				  deltatime = deltm;
				  tinfo = tar;
				  prevledger = prevcforheader;
				  blockdeltaroot = bdnewroot;
				}
			  in
			  let bhdnewh = hash_blockheaderdata bhdnew in
			  let bhsnew =
			    try
			      let (prvk,b,_,_,_,_) = List.find (fun (_,_,_,_,beta,_) -> beta = alpha) !Commands.walletkeys in
			      let r = rand_256() in
			      let sg : signat = signat_hashval bhdnewh prvk r in
			      { blocksignat = sg;
				blocksignatrecid = compute_recid sg r;
				blocksignatfcomp = b;
				blocksignatendorsement = None
			      }
			    with Not_found ->
			      try
				let (_,beta,(w,z),recid,fcomp,esg) =
				  List.find
				    (fun (alpha2,beta,(w,z),recid,fcomp,esg) ->
				      let (p,x0,x1,x2,x3,x4) = alpha2 in
				      let (q,_,_,_,_,_) = beta in
				      not p && (x0,x1,x2,x3,x4) = alpha && not q)
				    !Commands.walletendorsements
				in
				let (_,x0,x1,x2,x3,x4) = beta in
				let betah = (x0,x1,x2,x3,x4) in
				let (prvk,b,_,_,_,_) =
				  List.find
				    (fun (_,_,_,_,beta2,_) -> beta2 = betah)
				    !Commands.walletkeys in
				let r = rand_256() in
				let sg : signat = signat_hashval bhdnewh prvk r in
				{ blocksignat = sg;
				  blocksignatrecid = compute_recid sg r;
				  blocksignatfcomp = b;
				  blocksignatendorsement = Some(betah,recid,fcomp,esg)
				}
			      with Not_found ->
				raise (Failure("Was staking for " ^ Cryptocurr.addr_daliladdrstr (p2pkhaddr_addr alpha) ^ " but have neither the private key nor an appropriate endorsement for it."))
			  in
			  let bhnew = (bhdnew,bhsnew) in
			  let newblkid = blockheader_id bhnew in
			  DbBlockHeader.dbput newblkid bhnew;
			  DbBlockDelta.dbput newblkid bdnew;
			  List.iter
			    (fun stau -> DbSTx.dbput (hashstx stau) stau)
			    bdnew.blockdelta_stxl;
			  begin
			    let s = Buffer.create 10000 in
			    seosbf (seo_blockdelta seosb bdnew (s,None));
			    let bds = Buffer.length s in
			    if bds > maxblockdeltasize blkh then
			      (Printf.fprintf !log "New block is too big (%d bytes)\n" bds; flush !log; raise Not_found); (** in this case, probably the best option would be to switch back to an empty block **)
			    let prevledgerroot = node_ledgerroot best in
			    let csm0 = node_stakemod best in
			    let tar0 = node_targetinfo best in
			    if valid_blockheader blkh csm0 tar0 bhnew tm (match toburn with Some(burn) -> burn | _ -> 0L) then
			      () (* (Printf.fprintf !log "New block header is valid\n"; flush !log) *)
			    else
			      begin
				let b = Buffer.create 1000 in
				seosbf (seo_blockheader seosb bhnew (b,None));
				Printf.fprintf !log "New block header is not valid\nbhnew = %s\nfull header = %s\n" (hashval_hexstring newblkid) (string_hexstring (Buffer.contents b));
				flush !log;
				verbose_blockcheck := Some(!Utils.log);
				ignore (valid_blockheader blkh csm0 tar0 bhnew tm (match toburn with Some(burn) -> burn | _ -> 0L));
				verbose_blockcheck := None;
				let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in
				dumpstate (Filename.concat datadir "stakedinvalidblockheaderstate");
				Hashtbl.remove nextstakechances pbhh1;
				raise StakingProblemPause
			      end;
			    if not ((valid_block None None blkh csm0 tar0 (bhnew,bdnew) tm (match toburn with Some(burn) -> burn | _ -> 0L)) = None) then
			      () (* (Printf.fprintf !log "New block is valid\n"; flush stdout) *)
			    else
			      begin
				Printf.fprintf !log "New block is not valid\n";
				flush !log;
				verbose_blockcheck := Some(!Utils.log);
				ignore (valid_block None None blkh csm0 tar0 (bhnew,bdnew) tm (match toburn with Some(burn) -> burn | _ -> 0L));
				valid_blockheader blkh csm0 tar0 bhnew tm (match toburn with Some(burn) -> burn | _ -> 0L);
				verbose_blockcheck := None;
				let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in dumpstate (Filename.concat datadir "stakedinvalidblockstate");
				Hashtbl.remove nextstakechances pbhh1;
				raise StakingProblemPause
			      end;
			    match pbhh1 with
			    | None -> if blkh > 1L then (Printf.fprintf !log "No previous block but block height not 1\n"; flush !log; Hashtbl.remove nextstakechances None; raise StakingProblemPause)
			    | Some(pbhh1) ->
				if blkh = 1L then (Printf.fprintf !log "Previous block indicated but block height is 1\n"; flush !log; Hashtbl.remove nextstakechances (Some(pbhh1)); raise StakingProblemPause);
				let (pbhd,pbhs) = get_blockheader pbhh1 in
				let tmpsucctest bhd1 bhs1 bhd2 =
				  match bhd2.prevblockhash with
				  | Some(pbh,Poburn(lblkh,ltxh,lmedtm,burned)) ->
				      bhd2.timestamp = Int64.add bhd1.timestamp (Int64.of_int32 bhd2.deltatime)
					&&
				      pbh = blockheader_id (bhd1,bhs1) (*** the next block must also commit to the previous signature ***)
					&&
				      let tar1 = bhd1.tinfo in
				      let tar2 = bhd2.tinfo in
				      eq_big_int tar2 (retarget tar1 bhd2.deltatime)
				  | None -> false
				in
				if tmpsucctest pbhd pbhs bhdnew then
				  () (* (Printf.fprintf !log "Valid successor block\n"; flush !log) *)
				else
				  (Printf.fprintf !log "Not a valid successor block\n"; flush !log; let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in dumpstate (Filename.concat datadir "stakedinvalidsuccblockstate"); Hashtbl.remove nextstakechances (Some(pbhh1)); raise StakingProblemPause)
			  end;
			  begin
			    try
			      while true do
				let nw = ltc_medtime() in
				let tmtopub = Int64.sub tm nw in
				Printf.fprintf !log "tmtopub %Ld\n" tmtopub;
				if tmtopub > 0L then Thread.delay (Int64.to_float tmtopub) else raise Exit
			      done
			    with Exit -> ()
			  end;
			  let publish_new_block () =
			    Printf.fprintf !log "called publish_new_block\n";
			    if List.length !netconns < !Config.minconnstostake then
			      begin
				Printf.fprintf !log "Refusing to publish new block since node is insufficiently connected (only %d connections).\n" (List.length !netconns);
				Thread.delay 600.0 (*** delay for 10 minutes before continuing trying to stake to see if more connections arrive by then ***)
			      end
			    else
			      begin
				let ftm = Int64.add (ltc_medtime()) 3600L in
				if tm <= ftm then
				  begin
				    if node_validationstatus best = ValidBlock then (*** Don't publish a successor unless the previous block has been fully validated ***)
				      let currbestnode = get_bestnode_cw_exception false (StakingPause(300.0)) in
				      if pbhh = node_prevblockhash currbestnode then (*** if the bestnode has changed, don't publish it ***)
					begin
					  match toburn with
					  | Some(u) ->
					      begin (*** actually burn u litoshis and wait for a confirmation to know the block hash ***)
						try
						  let btx = ltc_createburntx (match pbhh with Some(_,Poburn(_,ltxh,_,_)) -> ltxh | None -> (0l,0l,0l,0l,0l,0l,0l,0l)) newblkid u in
						  let btxhex = Hashaux.string_hexstring btx in
						  let btxs = ltc_signrawtransaction btxhex in
						  let h = ltc_sendrawtransaction btxs in
						  pendingltctxs := h::!pendingltctxs;
						  Printf.fprintf !log "Sending ltc burn %s for header %s\n" h (hashval_hexstring newblkid);
						  publish_block blkh newblkid ((bhdnew,bhsnew),bdnew);
						  extraburn := 0L;
						  already := Some(newblkid,hexstring_hashval h);
						  output_string !log ("Burning " ^ (Int64.to_string u) ^ " litoshis in tx " ^ h ^ "\n")
						with
						| InsufficientLtcFunds ->
						    output_string !log ("insufficient ltc to burn " ^ (Int64.to_string u) ^ " litoshis" ^ "\n");
						    raise (StakingPause(300.0))
						| Not_found ->
						    output_string !log ("problem trying to burn " ^ (Int64.to_string u) ^ " litoshis" ^ "\n");
						    raise (StakingPause(300.0))
					      end
					  | None -> raise (Failure("must burn, should have known"))
					end
				  end			      
			      end
			  in
			  if node_validationstatus best = ValidBlock then (*** Don't publish a successor unless the previous block has been fully validated ***)
 			    let currbestnode = get_bestnode_cw_exception false (StakingPause(300.0)) in
			    if pbhh = node_prevblockhash currbestnode then (*** if the bestnode has changed, don't publish it unless the cumulative stake is higher ***)
			      publish_new_block()
			with MaxAssetsAtAddress ->
			  Printf.fprintf !log "Refusing to stake since the coinstake tx would put too many assets in an address.\n"
		      end
		  end
	    end
	| NoStakeUpTo(tm) ->
	    begin (*** before checking for future chances to stake, make sure we are clearly at one of the best chaintips ***)
	      match ltc_best_chaintips () with
	      | [] ->
		  begin
		    match pbhh1 with
		    | None -> ()
		    | Some(h) ->
			(*** this should not have happened, since the header should not have been completely formed until the burn was complete ***)
			Printf.fprintf !log "Refusing to stake on top of apparently unburned %s\nWaiting a few minutes to recheck for burn." (hashval_hexstring h);
			flush !log;
			raise (StakingPause(300.0))
		  end
	      | (bestctips::othctipsl) ->
		  begin
		    match pbhh1 with
		    | None ->
			Printf.fprintf !log "Refusing to stake genesis block when there are chaintips. Invalidate them by hand to force staking.\n";
			flush !log;
			raise (StakingPause(3600.0))
		    | Some(h) ->
			if List.mem h bestctips then
			  (if List.length bestctips > 1 then (Printf.fprintf !log "Staking on top of %s, orphaning other equally good tips.\n" (hashval_hexstring h); flush !log))
			else
			  begin
			    Printf.fprintf !log "Refusing to stake on top of %s when there are better chaintips. Invalidate them by hand to force staking.\n" (hashval_hexstring h);
			    flush !log;
			    raise (StakingPause(3600.0))
			  end
		  end
	    end;
	    let ltm = ltc_medtime() in
	    let stm = Int64.sub ltm 1200L in
	    let ftm = Int64.add ltm 86400L in
	    if tm < ftm && Int64.of_float (Unix.time()) < ftm then
	      compute_staking_chances best (if tm > stm then tm else stm) ftm
	    else
	      Thread.delay 60.0
      with
      | Not_found ->
	  Printf.fprintf !log "no nextstakechances\n"; flush !log;
	  Thread.delay 10.0;
	  Printf.fprintf !log "calling compute_staking_chances nextstakechances\n"; flush !log;
	  let ltm = ltc_medtime() in
	  let stm = Int64.sub ltm 1200L in
	  let ftm = Int64.add ltm 86400L in
	  compute_staking_chances best stm ftm
      | StakingProblemPause -> (*** there was some serious staking bug, try to recover by stopping staking for an hour and trying again ***)
	  Printf.fprintf !log "Pausing due to a staking bug; will retry staking in about an hour.\n";
	  flush !log;
	  Thread.delay 3600.0;
	  Printf.fprintf !log "Continuing staking.\n";
	  let ltm = ltc_medtime() in
	  let stm = Int64.sub ltm 1200L in
	  let ftm = Int64.add ltm 86400L in
	  compute_staking_chances best stm ftm
    with
    | StakingPause(del) ->
	Printf.fprintf !log "Staking pause of %f seconds\n" del; flush !log;
	Thread.delay del;
	Printf.fprintf !log "After staking pause of %f seconds\n" del; flush !log;
	sleepuntil := ltc_medtime()
  done;;

(*** if only one ledger root is in the snapshot, assets, hconselts and ctreeelts will not be revisited, so no need to waste memory by saving them in fin ***)
let snapshot_fin_mem fin h = 
  (List.length !snapshot_ledgerroots > 1) && Hashtbl.mem fin h

let snapshot_fin_add fin h =
  if List.length !snapshot_ledgerroots > 1 then
    Hashtbl.add fin h ()

let dbledgersnapshot_asset assetfile fin h =
  if not (snapshot_fin_mem fin h) then
    begin
      snapshot_fin_add fin h;
      try
        let a = DbAsset.dbget h in
        seocf (seo_asset seoc a (assetfile,None))
      with Not_found ->
        Printf.printf "Could not find %s asset in database\n" (hashval_hexstring h)
    end

let rec dbledgersnapshot_hcons (hconseltfile,assetfile) fin h l =
  if not (snapshot_fin_mem fin h) then
    begin
      snapshot_fin_add fin h;
      try
	let (ah,hr) = DbHConsElt.dbget h in
        seocf (seo_prod seo_hashval (seo_option (seo_prod seo_hashval seo_int8)) seoc (ah,hr) (hconseltfile,None));
	dbledgersnapshot_asset assetfile fin ah;
	match hr with
	| Some(hr,l2) ->
	    if not (l = l2+1) then Printf.printf "Length mismatch in hconselt %s: expected length %d after cons but rest has claimed length %d.\n" (hashval_hexstring h) l l2;
	    dbledgersnapshot_hcons (hconseltfile,assetfile) fin hr l2
	| None ->
	    if not (l = 1) then Printf.printf "Length mismatch in hconselt %s: expected length %d after cons but claimed to have no extra elements.\n" (hashval_hexstring h) l;
	    ()
      with Not_found ->
	Printf.printf "Could not find %s hcons element in database\n" (hashval_hexstring h)
    end

let rec dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h =
  if not (snapshot_fin_mem fin h) && (!snapshot_full || not (supp = [])) then
    begin
      snapshot_fin_add fin h;
      try
	let c = DbCTreeElt.dbget h in
	seocf (seo_ctree seoc c (ctreeeltfile,None));
	dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin supp c
      with Not_found ->
	Printf.printf "Could not find %s ctree element in database\n" (hashval_hexstring h)
    end
and dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin supp c =
  match c with
  | CLeaf(bl,NehHash(h,l)) ->
      dbledgersnapshot_hcons (hconseltfile,assetfile) fin h l
  | CLeaf(bl,_) ->
      Printf.printf "non element ctree found in database\n"
  | CHash(h) -> dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h
  | CLeft(c0) -> dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0
  | CRight(c1) -> dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1
  | CBin(c0,c1) ->
      dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0;
      dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1

let rec dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin supp c sl =
  if not (sl = []) then
    match c with
    | CLeaf(bl,NehHash(h,l)) ->
	dbledgersnapshot_hcons (hconseltfile,assetfile) fin h l
    | CLeaf(bl,_) ->
	Printf.printf "non element ctree found in database\n"
    | CHash(h) -> dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h
    | CLeft(c0) -> dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0 (strip_bitseq_false0 sl)
    | CRight(c1) -> dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1 (strip_bitseq_true0 sl)
    | CBin(c0,c1) ->
	dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0 (strip_bitseq_false0 sl);
	dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1 (strip_bitseq_true0 sl)

let dbledgersnapshot_shards (ctreeeltfile,hconseltfile,assetfile) fin supp h sl =
  if not (snapshot_fin_mem fin h) && (!snapshot_full || not (supp = [])) then
    begin
      snapshot_fin_add fin h;
      try
	let c = DbCTreeElt.dbget h in
	seocf (seo_ctree seoc c (ctreeeltfile,None));
	dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin supp c sl
      with Not_found ->
	Printf.printf "Could not find %s ctree element in database\n" (hashval_hexstring h)
    end

let dbledgersnapshot_ctree_top (ctreeeltfile,hconseltfile,assetfile) fin supp h s =
  match s with
  | None -> dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h
  | Some(sl) ->
      let bitseq j =
	let r = ref [] in
	for i = 0 to 8 do
	  if ((j lsr i) land 1) = 1 then
	    r := true::!r
	  else
	    r := false::!r
	done;
	!r
      in
      dbledgersnapshot_shards (ctreeeltfile,hconseltfile,assetfile) fin supp h (List.map bitseq sl)

let sinceltctime f =
  let snc = Int64.sub (ltc_medtime()) f in
  if snc >= 172800L then
    (Int64.to_string (Int64.div snc 86400L)) ^ " days"
  else if snc >= 7200L then
    (Int64.to_string (Int64.div snc 7200L)) ^ " hours"
  else if snc >= 120L then
    (Int64.to_string (Int64.div snc 60L)) ^ " minutes"
  else if snc = 1L then
    "1 second"
  else
    (Int64.to_string snc) ^ " seconds";;

let sincetime f =
  let snc = Int64.sub (Int64.of_float (Unix.time())) f in
  if snc >= 172800L then
    (Int64.to_string (Int64.div snc 86400L)) ^ " days"
  else if snc >= 7200L then
    (Int64.to_string (Int64.div snc 7200L)) ^ " hours"
  else if snc >= 120L then
    (Int64.to_string (Int64.div snc 60L)) ^ " minutes"
  else if snc = 1L then
    "1 second"
  else
    (Int64.to_string snc) ^ " seconds";;

let rec parse_command_r l i n =
  if i < n then
    let j = ref i in
    while !j < n && l.[!j] = ' ' do
      incr j
    done;
    let b = Buffer.create 20 in
    while !j < n && not (List.mem l.[!j] [' ';'"';'\'']) do
      Buffer.add_char b l.[!j];
      incr j
    done;
    let a = Buffer.contents b in
    let c d = if a = "" then d else a::d in
    if !j < n && l.[!j] = '"' then
      c (parse_command_r_q l (!j+1) n)
    else if !j < n && l.[!j] = '\'' then
      c (parse_command_r_sq l (!j+1) n)
    else
      c (parse_command_r l (!j+1) n)
  else
    []
and parse_command_r_q l i n =
  let b = Buffer.create 20 in
  let j = ref i in
  while !j < n && not (l.[!j] = '"') do
    Buffer.add_char b l.[!j];
    incr j
  done;
  if !j < n then
    Buffer.contents b::parse_command_r l (!j+1) n
  else
    raise (Failure("missing \""))
and parse_command_r_sq l i n =
  let b = Buffer.create 20 in
  let j = ref i in
  while !j < n && not (l.[!j] = '\'') do
    Buffer.add_char b l.[!j];
    incr j
  done;
  if !j < n then
    Buffer.contents b::parse_command_r l (!j+1) n
  else
    raise (Failure("missing '"))

let parse_command l =
  let ll = parse_command_r l 0 (String.length l) in
  match ll with
  | [] -> raise Exit (*** empty command, silently ignore ***)
  | (c::al) -> (c,al)

let do_command oc l =
  let (c,al) = parse_command l in
  match c with
  | "query" ->
      begin
	match al with
	| [h] ->
	    begin
	      try
		let blkh = Int64.of_string h in
		let j = Commands.query_blockheight blkh in
		print_jsonval oc j;
		Printf.fprintf oc "\n"
	      with Failure(_) ->
		let j = Commands.query h in
		print_jsonval oc j;
		Printf.fprintf oc "\n"
	    end
	| [h;kh] ->
	    let k = hexstring_hashval kh in
	    begin
	      try
		let BlocktreeNode(_,_,pbh,_,_,ledgerroot,_,_,_,_,blkh,_,_,_) = Hashtbl.find blkheadernode (Some(k)) in
		let j = Commands.query_at_block h pbh ledgerroot blkh in
		print_jsonval oc j;
		Printf.fprintf oc "\n"
	      with Not_found ->
		if DbCTreeElt.dbexists k then
		  begin
		    let j = Commands.query_at_block h None k (-1L) in
		    print_jsonval oc j;
		    Printf.fprintf oc "\n"
		  end
		else
		  raise (Failure ("could not interpret " ^ kh ^ " as a block or ledger root"))
	    end
	| _ ->
	    raise (Failure("expected query <hashval or address or int[block height]> [<blockid or ledgerroot>]"))
      end
  | "ltcstatusdump" ->
      begin
	let (fn,blkh,howfarback) =
	  match al with
	  | [] -> ("ltcstatusdumpfile",hexstring_hashval (Ltcrpc.ltc_getbestblockhash ()),1000)
	  | [fn] -> (fn,hexstring_hashval (Ltcrpc.ltc_getbestblockhash ()),1000)
	  | [fn;hh] -> (fn,hexstring_hashval hh,1000)
	  | [fn;hh;b] -> (fn,hexstring_hashval hh,int_of_string b)
	  | _ -> raise (Failure "expected ltcstatusdump [<filename> [<ltcblockhash> [<how many ltc blocks back>]]]")
	in
	let cblkh = ref blkh in
	let f = open_out fn in
	begin
	  try
	    for i = 1 to howfarback do
	      Printf.fprintf f "%d. ltc block %s DacStatus\n" i (hashval_hexstring !cblkh);
	      begin
		try
		  match DbLtcDacStatus.dbget !cblkh with
		  | LtcDacStatusPrev(h) ->
		      Printf.fprintf f "  DacStatus unchanged since ltc block %s\n" (hashval_hexstring h)
		  | LtcDacStatusNew(l) ->
		      Printf.fprintf f "  New DacStatus:\n";
		      let cnt = ref 0 in
		      List.iter
			(fun li ->
			  let i = !cnt in
			  incr cnt;
			  match li with
			  | [] -> Printf.fprintf f "   %d. Empty tip? Should not be possible.\n" i;
			  | ((bh,lbh,ltx,ltm,lhght)::r) ->
			      Printf.fprintf f "   (%d) - Dalilcoin Block: %s\n        Litecoin Block: %s\n        Litecoin Burn Tx: %s\n        Litecoin Time: %Ld\n        Litecoin Height: %Ld\n" i (hashval_hexstring bh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght;
			      List.iter (fun (bh,lbh,ltx,ltm,lhght) ->
				Printf.fprintf f "       - Dalilcoin Block: %s\n        Litecoin Block: %s\n        Litecoin Burn Tx: %s\n        Litecoin Time: %Ld\n        Litecoin Height: %Ld\n" (hashval_hexstring bh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght)
				r)
			l
		with Not_found ->
		  Printf.fprintf f "  DacStatus not found\n"
	      end;
	      begin
		try
		  let (prevh,tm,hght,burntxhs) = DbLtcBlock.dbget !cblkh in
		  Printf.fprintf f "%d. ltc block %s info\n" i (hashval_hexstring !cblkh);
		  Printf.fprintf f "   Previous %s\n   Block Time %Ld\n    Height %Ld\n" (hashval_hexstring prevh) tm hght;
		  cblkh := prevh;
		  match burntxhs with
		  | [] -> ()
		  | [x] -> Printf.fprintf f "    Burn Tx: %s\n" (hashval_hexstring x)
		  | _ ->
		      Printf.fprintf f "    %d Burn Txs:\n" (List.length burntxhs);
		      List.iter (fun x -> Printf.fprintf f "         %s\n" (hashval_hexstring x)) burntxhs
		with Not_found ->
		  Printf.fprintf f "  LtcBlock not found\n"
	      end
	    done
	  with e -> Printf.fprintf f "Exception: %s\n" (Printexc.to_string e)
	end;
	close_out f
      end
  | "ltcstatus" ->
      begin
	let h =
	  match al with
	  | [hh] -> hexstring_hashval hh
	  | _ ->
	      Printf.fprintf oc "ltcbest %s\n" (hashval_hexstring !ltc_bestblock);
	      !ltc_bestblock
	in
	let (lastchangekey,zll) = ltcdacstatus_dbget h in
	let tm = ltc_medtime() in
	if zll = [] && tm > Int64.add !Config.genesistimestamp 604800L then
	  begin
	    Printf.printf "No blocks were created in the past week. Dalilcoin has reached terminal status.\nThe only recovery possible for the network is a hard fork.\n"
	  end;
	let i = ref 0 in
	List.iter
	  (fun zl ->
	    incr i;
	    Printf.fprintf oc "%d.\n" !i;
	    List.iter
	      (fun (dbh,lbh,ltx,ltm,lhght) ->
		if DbBlacklist.dbexists dbh then
		  Printf.fprintf oc "- %s (blacklisted, presumably invalid) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		else if DbInvalidatedBlocks.dbexists dbh then
		  Printf.fprintf oc "- %s (marked invalid) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		else if DbBlockHeader.dbexists dbh then
		  if DbBlockDelta.dbexists dbh then
		    Printf.fprintf oc "+ %s %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		  else
		    begin
		      possibly_request_dalilcoin_block dbh;
		      Printf.fprintf oc "* %s (missing delta) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		    end
		else
		  begin
		    possibly_request_dalilcoin_block dbh;
		    Printf.fprintf oc "* %s (missing header) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		  end)
	      zl)
	  zll
      end
  | "ltcgettxinfo" ->
      begin
	match al with
	| [h] ->
	    begin
	      try
		let (burned,prev,nxt,lblkh,confs) = Ltcrpc.ltc_gettransactioninfo h in
		match lblkh,confs with
		| Some(lh),Some(confs) ->
		    Printf.fprintf oc "burned %Ld prev %s next %s in ltc block %s, %d confirmations\n" burned (hashval_hexstring prev) (hashval_hexstring nxt) lh confs
		| _,_ ->
		    Printf.fprintf oc "burned %Ld prev %s next %s\n" burned (hashval_hexstring prev) (hashval_hexstring nxt)
	      with Not_found -> raise (Failure("problem"))
	    end
	| _ -> raise (Failure("expected ltcgettxinfo <txid>"))
      end
  | "ltcgetbestblockhash" ->
      begin
	try
	  let x = Ltcrpc.ltc_getbestblockhash () in
	  Printf.fprintf oc "best ltc block hash %s\n" x
	with Not_found ->
	  Printf.fprintf oc "could not find best ltc block hash\n"
      end
  | "ltcgetblock" ->
      begin
	match al with
	| [h] ->
	    begin
	      try
		let (pbh,tm,hght,txl) = Ltcrpc.ltc_getblock h in
		Printf.fprintf oc "ltc block %s time %Ld height %Ld prev %s; %d dalilcoin candidate txs:\n" h tm hght pbh (List.length txl);
		List.iter (fun tx -> Printf.fprintf oc "%s\n" tx) txl
	      with Not_found ->
		Printf.fprintf oc "could not find ltc block %s\n" h
	    end
	| _ -> Printf.fprintf oc "expected ltcgetblock <blockid>\n"
      end
  | "ltclistunspent" ->
      begin
	try
	  let utxol = Ltcrpc.ltc_listunspent () in
	  Printf.fprintf oc "%d ltc utxos\n" (List.length utxol);
	  List.iter (fun (txid,vout,_,_,amt) -> Printf.fprintf oc "%s:%d %Ld\n" txid vout amt) utxol
	with Not_found ->
	  Printf.fprintf oc "could not get unspent ltc list\n"
      end
  | "hash" ->
      begin
	match al with
	| [h] -> Printf.fprintf oc "%s\n" (hashval_hexstring (Sha256.sha256dstr (Hashaux.hexstring_string h)))
	| _ -> raise Not_found
      end
  | "ltcsigntx" ->
      begin
	match al with
	| [tx] -> Printf.fprintf oc "%s\n" (Ltcrpc.ltc_signrawtransaction tx)
	| _ -> raise Not_found
      end
  | "ltcsendtx" ->
      begin
	match al with
	| [tx] -> Printf.fprintf oc "%s\n" (Ltcrpc.ltc_sendrawtransaction tx)
	| _ -> raise Not_found
      end
  | "ltccreateburn" ->
      begin
	match al with
	| [h1;h2;toburn] ->
	    begin
	      try
		let txs = Ltcrpc.ltc_createburntx (hexstring_hashval h1) (hexstring_hashval h2) (Int64.of_string toburn) in
		Printf.fprintf oc "burntx: %s\n" (Hashaux.string_hexstring txs)
	      with
	      | Ltcrpc.InsufficientLtcFunds ->
		  Printf.fprintf oc "no ltc utxo has %s litoshis\n" toburn
	      | Not_found ->
		  Printf.fprintf oc "trouble creating burn tx\n"
	    end
	| _ -> Printf.fprintf oc "expected ltccreateburn <hash1> <hash2> <litoshis to burn>\n"
      end
  | "exit" ->
      (*** Could call Thread.kill on netth and stkth, but Thread.kill is not always implemented. ***)
      closelog();
      Printf.fprintf oc "Shutting down threads. Please be patient.\n"; flush oc;
      !exitfn 0
  | "stop" ->
      (*** Could call Thread.kill on netth and stkth, but Thread.kill is not always implemented. ***)
      closelog();
      Printf.fprintf oc "Shutting down threads. Please be patient.\n"; flush oc;
      !exitfn 0
  | "dumpstate" -> (*** dump state to a file for debugging ***)
      begin
	match al with
	| [fa] -> dumpstate fa
	| _ -> raise (Failure "dumpstate <textfile>")
      end
  | "addnode" ->
      begin
	let addnode_add n =
	  match tryconnectpeer n with
	  | None -> raise (Failure "Failed to add node")
	  | Some(lth,sth,(fd,sin,sout,gcs)) ->
	      match !gcs with
	      | None -> raise (Failure "Problem adding node")
	      | Some(cs) ->
		  if cs.addrfrom = "" then Thread.delay 1.0;
		  addknownpeer (Int64.of_float cs.conntime) cs.addrfrom
	in
	match al with
	| [n] -> addnode_add n
	| [n;"add"] -> addnode_add n
        | [n;"remove"] ->
          removeknownpeer n;
          List.iter
	      (fun (lth,sth,(fd,sin,sout,gcs)) -> if peeraddr !gcs = n then (Unix.close fd; gcs := None))
	      !netconns
	| [n;"onetry"] ->
	    ignore (tryconnectpeer n)
	| _ ->
	    raise (Failure "addnode <ip:port> [add|remove|onetry]")
      end
  | "clearbanned" -> clearbanned()
  | "listbanned" -> Hashtbl.iter (fun n () -> Printf.printf "%s\n" n) bannedpeers
  | "bannode" -> List.iter (fun n -> banpeer n) al
  | "getinfo" ->
      remove_dead_conns();
      let ll = List.length !netconns in
      Printf.fprintf oc "%d connection%s\n" ll (if ll = 1 then "" else "s");
      begin
	try
	  let BlocktreeNode(_,_,pbh,_,_,ledgerroot,csm,tar,tmstmp,_,blkh,_,_,_) = get_bestnode_print_warnings oc true in
	  let gtm = Unix.gmtime (Int64.to_float tmstmp) in
	  begin
	    match pbh with
	    | Some(h,_) -> Printf.fprintf oc "Best block %s at height %Ld\n" (hashval_hexstring h) (Int64.sub blkh 1L) (*** blkh is the height the next block will have ***)
	    | None -> Printf.fprintf oc "No blocks yet\n"
	  end;
	  Printf.fprintf oc "Time: %Ld (UTC %02d %02d %04d %02d:%02d:%02d)\n" tmstmp gtm.Unix.tm_mday (1+gtm.Unix.tm_mon) (1900+gtm.Unix.tm_year) gtm.Unix.tm_hour gtm.Unix.tm_min gtm.Unix.tm_sec;
	  Printf.fprintf oc "Target: %s\n" (string_of_big_int tar);
	  Printf.fprintf oc "Difficulty: %s\n" (string_of_big_int (difficulty tar));
	  let (bal1,bal2,bal3,bal4) = Commands.get_cants_balances_in_ledger oc ledgerroot in
	  Printf.fprintf oc "Total p2pkh: %s fraenks\n" (fraenks_of_cants bal1);
	  Printf.fprintf oc "Total p2sh: %s fraenks\n" (fraenks_of_cants bal2);
	  Printf.fprintf oc "Total via endorsement: %s fraenks\n" (fraenks_of_cants bal3);
	  Printf.fprintf oc "Total watched: %s fraenks\n" (fraenks_of_cants bal4);
	  Printf.fprintf oc "Sum of all: %s fraenks\n" (fraenks_of_cants (Int64.add bal1 (Int64.add bal2 (Int64.add bal3 bal4))))
	with e ->
	  Printf.fprintf oc "Exception: %s\n" (Printexc.to_string e)
      end
  | "getpeerinfo" ->
      remove_dead_conns();
      let ll = List.length !netconns in
      Printf.fprintf oc "%d connection%s\n" ll (if ll = 1 then "" else "s");
      List.iter
	(fun (_,_,(_,_,_,gcs)) ->
	  match !gcs with
	  | Some(cs) ->
	      Printf.fprintf oc "%s (%s): %s\n" cs.realaddr cs.addrfrom cs.useragent;
	      let snc = Int64.sub (Int64.of_float (Unix.time())) (Int64.of_float cs.conntime) in
	      let snc1 = sincetime (Int64.of_float cs.conntime) in
	      let snc2 = sincetime (Int64.of_float cs.lastmsgtm) in
	      Printf.fprintf oc "Connected for %s; last message %s ago.\n" snc1 snc2;
	      if cs.handshakestep < 5 then Printf.fprintf oc "(Still in handshake phase)\n";
	  | None -> (*** This could happen if a connection died after remove_dead_conns above. ***)
	      Printf.fprintf oc "[Dead Connection]\n";
	  )
	!netconns;
      flush oc
  | "nettime" ->
      let (tm,skew) = network_time() in
      Printf.fprintf oc "network time %Ld (median skew of %d)\n" tm skew;
      flush oc;
  | "invalidateblock" ->
      begin
	match al with
	| [h] ->
	    let hh = hexstring_hashval h in
	    recursively_invalidate_blocks hh
	| _ -> raise (Failure "invalidateblock <blockhash>")
      end
  | "revalidateblock" ->
      begin
	match al with
	| [h] ->
	    let hh = hexstring_hashval h in
	    recursively_revalidate_blocks hh
	| _ -> raise (Failure "revalidateblock <blockhash>")
      end
  | "getblock" ->
      begin
	match al with
	| [hh] ->
	    begin
	      let h = hexstring_hashval hh in
	      try
		let (bhd,_) = DbBlockHeader.dbget h in
		Printf.printf "Time: %Ld\n" bhd.timestamp;
		begin
		  try
		    let bd = DbBlockDelta.dbget h in
		    Printf.printf "%d txs\n" (List.length (bd.blockdelta_stxl));
		    List.iter (fun (tx,txs) -> Printf.printf "%s\n" (hashval_hexstring (hashtx tx))) (bd.blockdelta_stxl);
		  with Not_found ->
		    find_and_send_requestdata GetBlockdelta h;
		    Printf.printf "Missing block delta\n"
		end
	      with Not_found ->
		find_and_send_requestdata GetHeader h
	    end
	| _ -> raise (Failure "getblock <blockhash>")
      end
  | "nextstakingchances" ->
      begin
	let (scnds,maxburn,n) =
	  match al with
	  | [] ->
	      let n = get_bestnode_print_warnings oc true in
	      (3600 * 24,100000000L,n)
	  | [hrs] ->
	      let n = get_bestnode_print_warnings oc true in
	      (3600 * (int_of_string hrs),100000000L,n)
	  | [hrs;maxburn] ->
	      let n = get_bestnode_print_warnings oc true in
	      (3600 * (int_of_string hrs),litoshis_of_ltc maxburn,n)
	  | [hrs;maxburn;blockid] ->
	      begin
		try
		  let n = Hashtbl.find blkheadernode (Some(hexstring_hashval blockid)) in
		  (3600 * (int_of_string hrs),litoshis_of_ltc maxburn,n)
		with Not_found ->
		  raise (Failure ("unknown block " ^ blockid))
	      end
	  | _ -> raise (Failure "nextstakingchances [<hours> [<max ltc to burn> [<blockid>]]")
	in
	let BlocktreeNode(_,_,prevblk,_,_,_,_,_,tmstmp,_,_,_,_,_) = n in
	let prevblkh = fstohash prevblk in
	let nw = ltc_medtime() in (*** for staking purposes, ltc is the clock to follow ***)
	let fromnow_string i nw =
	  if i <= nw then
	    "now"
	  else
	    let del = Int64.to_int (Int64.sub i nw) in
	    if del < 60 then
	      Printf.sprintf "%d seconds from now" del
	    else if del < 3600 then
	      Printf.sprintf "%d minutes %d seconds from now" (del / 60) (del mod 60)
	    else
	      Printf.sprintf "%d hours %d minutes %d seconds from now" (del / 3600) ((del mod 3600) / 60) (del mod 60)
	in
	compute_staking_chances n tmstmp (min (Int64.add tmstmp 604800L) (Int64.add nw (Int64.of_int scnds)));
	begin
	  try
	    match Hashtbl.find nextstakechances prevblkh with
	    | NextStake(i,stkaddr,h,bday,obl,v,Some(toburn),_) ->
		Printf.fprintf oc "Can stake at time %Ld (%s) with asset %s at address %s burning %Ld litoshis (%s ltc).\n" i (fromnow_string i nw) (hashval_hexstring h) (addr_daliladdrstr (p2pkhaddr_addr stkaddr)) toburn (ltc_of_litoshis toburn);
	    | NextStake(i,stkaddr,h,bday,obl,v,None,_) -> () (*** should not happen; ignore ***)
	    | NoStakeUpTo(_) -> Printf.fprintf oc "Found no chance to stake with current wallet and ltc burn limits.\n"
	  with Not_found -> ()
	end;
	List.iter
	  (fun z ->
	    let il = ref [] in
	    match z with
	    | NextStake(i,stkaddr,h,bday,obl,v,Some(toburn),_) ->
		if not (List.mem i !il) then
		  begin
		    il := i::!il; (** while the info should not be on the hash table more than once, sometimes it is, so only report it once **)
		    Printf.fprintf oc "With extraburn %Ld litoshis (%s ltc), could stake at time %Ld (%s) with asset %s at address %s.\n" toburn (ltc_of_litoshis toburn) i (fromnow_string i nw) (hashval_hexstring h) (addr_daliladdrstr (p2pkhaddr_addr stkaddr))
		  end
	    | _ -> ())
	  (List.sort
	     (fun y z ->
	       match (y,z) with
	       | (NextStake(i,_,_,_,_,_,Some(_),_),NextStake(j,_,_,_,_,_,Some(_),_)) -> compare i j
	       | _ -> 0)
	     (List.filter
		(fun z ->
		  match z with
		  | NextStake(i,stkaddr,h,bday,obl,v,Some(toburn),_) -> true
		  | _ -> false)
		(Hashtbl.find_all nextstakechances_hypo prevblkh)))
      end
  | "extraburn" ->
      begin
	match al with
	| [a] -> extraburn := litoshis_of_ltc a
	| [a;b] when b = "litoshis" -> extraburn := Int64.of_string a
	| _ -> raise (Failure "extraburn <ltc> or extraburn <litoshis> litoshis")
      end
  | "printassets" when al = [] -> Commands.printassets oc
  | "printassets" -> List.iter (fun h -> Commands.printassets_in_ledger oc (hexstring_hashval h)) al
  | "printtx" -> List.iter (fun h -> Commands.printtx (hexstring_hashval h)) al
  | "importprivkey" -> List.iter Commands.importprivkey al
  | "importbtcprivkey" -> List.iter Commands.importbtcprivkey al
  | "importwatchaddr" -> List.iter Commands.importwatchaddr al
  | "importwatchbtcaddr" -> List.iter Commands.importwatchbtcaddr al
  | "importendorsement" ->
      begin
	match al with
	| [a;b;s] -> Commands.importendorsement a b s
	| _ -> raise (Failure "importendorsement should be given three arguments: a b s where s is a signature made with the private key for address a endorsing to address b")
      end
  | "btctodaliladdr" -> List.iter Commands.btctodaliladdr al
  | "printasset" ->
      begin
	match al with
	| [h] -> Commands.printasset (hexstring_hashval h)
	| _ -> raise (Failure "printasset <assethash>")
      end
  | "printhconselt" ->
      begin
	match al with
	| [h] -> Commands.printhconselt (hexstring_hashval h)
	| _ -> raise (Failure "printhconselt <hashval>")
      end
  | "printctreeelt" ->
      begin
	match al with
	| [h] -> Commands.printctreeelt (hexstring_hashval h)
	| _ -> raise (Failure "printctreeelt <hashval>")
      end
  | "printctreeinfo" ->
      begin
	match al with
	| [] ->
	    let best = get_bestnode_print_warnings oc true in
	    let BlocktreeNode(_,_,_,_,_,currledgerroot,_,_,_,_,_,_,_,_) = best in
	    Commands.printctreeinfo currledgerroot
	| [h] -> Commands.printctreeinfo (hexstring_hashval h)
	| _ -> raise (Failure "printctreeinfo [ledgerroot]")
      end
  | "newaddress" ->
      begin
	match al with
	| [] ->
	    let best = get_bestnode_print_warnings oc true in
	    let BlocktreeNode(_,_,_,_,_,currledgerroot,_,_,_,_,_,_,_,_) = best in
	    let (k,h) = Commands.generate_newkeyandaddress currledgerroot in
	    let alpha = p2pkhaddr_addr h in
	    let a = addr_daliladdrstr alpha in
	    Printf.fprintf oc "%s\n" a
	| [clr] ->
	    let (k,h) = Commands.generate_newkeyandaddress (hexstring_hashval clr) in
	    let alpha = p2pkhaddr_addr h in
	    let a = addr_daliladdrstr alpha in
	    Printf.fprintf oc "%s\n" a
	| _ -> raise (Failure "newaddress [ledgerroot]")
      end
  | "createtx" ->
      begin
	try
	  match al with
	  | [inp;outp] ->
	      let (inpj,_) = parse_jsonval inp in
	      let (outpj,_) = parse_jsonval outp in
	      let tau = Commands.createtx inpj outpj in
	      let s = Buffer.create 100 in
	      seosbf (seo_stx seosb (tau,([],[])) (s,None));
	      let hs = Hashaux.string_hexstring (Buffer.contents s) in
	      Printf.fprintf oc "%s\n" hs
	  | _ -> raise Exit
	with Exit ->
	  Printf.fprintf oc "createtx <inputs as json array> <outputs as json array>\neach input: {\"<addr>\":\"<assetid>\"}\neach output: {\"addr\":\"<addr>\",\"val\":<fraenks>,\"lock\":<height>,\"obligationaddress\":\"<addr>\"}\nwhere lock is optional (default null, unlocked output)\nand obligationaddress is optional (default null, meaning the holder address is implicitly the obligationaddress)\n"
      end
  | "creategeneraltx" ->
      begin
	try
	  match al with
	  | [jtxstr] ->
	      let (jtx,_) = parse_jsonval jtxstr in
	      let tau = tx_from_json jtx in
	      let s = Buffer.create 100 in
	      seosbf (seo_stx seosb (tau,([],[])) (s,None));
	      let hs = Hashaux.string_hexstring (Buffer.contents s) in
	      Printf.fprintf oc "%s\n" hs
	  | _ ->
	      raise Exit
	with Exit ->
	  Printf.fprintf oc "creategeneraltx <tx as json object>\n"
      end
  | "createsplitlocktx" ->
      begin
	match al with
	| (alp::aid::n::lkh::fee::r) ->
	    begin
	      let alpha2 = daliladdrstr_addr alp in
	      if not (payaddr_p alpha2) then raise (Failure (alp ^ " is not a pay address"));
	      let (p,a4,a3,a2,a1,a0) = alpha2 in
	      let alpha = (p=1,a4,a3,a2,a1,a0) in
	      let aid = hexstring_hashval aid in
	      let n = int_of_string n in
	      if n <= 0 then raise (Failure ("Cannot split into " ^ (string_of_int n) ^ " assets"));
	      let lkh = Int64.of_string lkh in
	      let fee = cants_of_fraenks fee in
	      if fee < 0L then raise (Failure ("Cannot have a negative free"));
	      match r with
	      | [] ->
		  let gamma = alpha2 in
		  let beta = alpha in
		  let lr = node_ledgerroot (get_bestnode_print_warnings oc true) in
		  Commands.createsplitlocktx lr alpha beta gamma aid n lkh fee
	      | (gam::r) ->
		  let gamma = daliladdrstr_addr gam in
		  if not (payaddr_p gamma) then raise (Failure (gam ^ " is not a pay address"));
		  match r with
		  | [] ->
		      let beta = alpha in
		      let lr = node_ledgerroot (get_bestnode_print_warnings oc true) in
		      Commands.createsplitlocktx lr alpha beta gamma aid n lkh fee
		  | (bet::r) ->
		      let beta2 = daliladdrstr_addr bet in
		      if not (payaddr_p beta2) then raise (Failure (bet ^ " is not a pay address"));
		      let (p,b4,b3,b2,b1,b0) = beta2 in
		      let beta = (p=1,b4,b3,b2,b1,b0) in
		      match r with
		      | [] ->
			  let lr = node_ledgerroot (get_bestnode_print_warnings oc true) in
			  Commands.createsplitlocktx lr alpha beta gamma aid n lkh fee
		      | [lr] ->
			  let lr = hexstring_hashval lr in
			  Commands.createsplitlocktx lr alpha beta gamma aid n lkh fee
		      | _ ->
			  Printf.fprintf oc "createsplitlocktx <current address> <assetid> <number of outputs> <lockheight> <fee> [<new holding address> [<new obligation address> [<ledger root>]]]\n";
			  flush oc
	    end
	| _ ->
	    Printf.fprintf oc "createsplitlocktx <current address> <assetid> <number of outputs> <lockheight> <fee> [<new holding address> [<new obligation address> [<ledger root>]]]\n";
	    flush oc
      end
  | "signtx" ->
      begin
	match al with
	| [s] -> Commands.signtx oc (node_ledgerroot (get_bestnode_print_warnings oc true)) s
	| _ ->
	    Printf.fprintf oc "signtx <tx in hex>\n";
	    flush oc
      end
  | "savetxtopool" ->
      begin
	match al with
	| [s] -> Commands.savetxtopool (node_blockheight (get_bestnode_print_warnings oc true)) (node_ledgerroot (get_bestnode_print_warnings oc true)) s
	| _ ->
	    Printf.fprintf oc "savetxtopool <tx in hex>\n";
	    flush oc
      end
  | "sendtx" ->
      begin
	match al with
	| [s] -> Commands.sendtx oc (node_blockheight (get_bestnode_print_warnings oc true)) (node_ledgerroot (get_bestnode_print_warnings oc true)) s
	| _ ->
	    Printf.fprintf oc "sendtx <tx in hex>\n";
	    flush oc
      end
  | "querybestblock" ->
      let node = get_bestnode_print_warnings oc true in
      let h = node_prevblockhash node in
      let blkh = node_blockheight node in
      let lr = node_ledgerroot node in
      begin
	match h with
	| Some(h,_) ->
	    print_jsonval oc (JsonObj([("height",JsonNum(Int64.to_string (Int64.sub blkh 1L)));("block",JsonStr(hashval_hexstring h));("ledgerroot",JsonStr(hashval_hexstring lr))]));
	    flush oc
	| None ->
	    print_jsonval oc (JsonObj([("height",JsonNum(Int64.to_string (Int64.sub blkh 1L)));("ledgerroot",JsonStr(hashval_hexstring lr))]));
	    flush oc
      end
  | "bestblock" ->
      let node = get_bestnode_print_warnings oc true in
      let h = node_prevblockhash node in
      let blkh = node_blockheight node in
      let lr = node_ledgerroot node in
      begin
	match h with
	| Some(h,_) ->
	    Printf.fprintf oc "Height: %Ld\nBlock hash: %s\nLedger root: %s\n" (Int64.sub blkh 1L) (hashval_hexstring h) (hashval_hexstring lr);
	    flush oc
	| None ->
	    Printf.fprintf oc "Height: %Ld\nNo blocks yet.\nLedger root: %s\n" (Int64.sub blkh 1L) (hashval_hexstring lr);
	    flush oc
      end
  | "difficulty" ->
      let node = get_bestnode_print_warnings oc true in
      let tar = node_targetinfo node in
      let blkh = node_blockheight node in
      Printf.fprintf oc "Current target (for block at height %Ld): %s\n" blkh (string_of_big_int tar);
      flush oc
  | "blockchain" -> pblockchain oc (get_bestnode_print_warnings oc true) None None 1000
  | _ ->
      (Printf.fprintf oc "Ignoring unknown command: %s\n" c; List.iter (fun a -> Printf.fprintf oc "%s\n" a) al; flush oc);;

let initialize () =
  begin
    datadir_from_command_line(); (*** if -datadir=... is on the command line, then set Config.datadir so we can find the config file ***)
    process_config_file();
    process_config_args(); (*** settings on the command line shadow those in the config file ***)
    if not !Config.testnet then (Printf.printf "Dalilcoin can only be run on testnet for now. Please give the -testnet command line argument.\n"; exit 1);
    let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in
    if !Config.testnet then
      begin
	max_target := shift_left_big_int unit_big_int 210;
	genesistarget := shift_left_big_int unit_big_int 205; (* easy initial target for testnet *)
	if !Config.ltcrpcport = 9332 then Config.ltcrpcport := 19332;
      end;
    if Sys.file_exists (Filename.concat datadir "lock") then
      begin
	Printf.printf "Cannot start Dalilcoin. Do you already have Dalilcoin running? If not, remove: %s\n" (Filename.concat datadir "lock");
	flush stdout;
	exit 1;
      end;
    lock datadir;
    Printf.printf "Initializing the database..."; flush stdout;
    let dbdir = Filename.concat datadir "db" in
    dbconfig dbdir; (*** configure the database ***)
    DbAsset.dbinit();
    DbAssetIdAt.dbinit();
    DbSTx.dbinit();
    DbHConsElt.dbinit();
    DbHConsEltAt.dbinit();
    DbCTreeElt.dbinit();
    DbCTreeEltAt.dbinit();
    DbBlockHeader.dbinit();
    DbBlockDelta.dbinit();
    DbInvalidatedBlocks.dbinit();
    DbLtcDacStatus.dbinit();
    DbLtcBurnTx.dbinit();
    DbLtcBlock.dbinit();
    Printf.printf "Initialized.\n"; flush stdout;
    openlog(); (*** Don't open the log until the config vars are set, so if we know whether or not it's testnet. ***)
    if !createsnapshot then
      begin
	match !snapshot_dir with
	| None ->
	    Printf.printf "No snapshot directory given.\n";
	    !exitfn 1
	| Some(dir) -> (*** then creating a snapshot ***)
	    Printf.printf "Creating snapshot.\n"; flush stdout;
	    let fin : (hashval,unit) Hashtbl.t = Hashtbl.create 10000 in
	    begin
	      if Sys.file_exists dir then
		if Sys.is_directory dir then
		  ()
		else
		  raise (Failure (dir ^ " is a file not a directory"))
	      else
		begin
		  Unix.mkdir dir 0b111111000
		end
	    end;
	    let headerfile = open_out_bin (Filename.concat dir "headers") in
	    let blockfile = open_out_bin (Filename.concat dir "blocks") in
	    let ctreeeltfile = open_out_bin (Filename.concat dir "ctreeelts") in
	    let hconseltfile = open_out_bin (Filename.concat dir "hconselts") in
	    let assetfile = open_out_bin (Filename.concat dir "assets") in
	    List.iter
	      (fun h ->
		if not (Hashtbl.mem fin h) then
		  begin
		    Hashtbl.add fin h ();
                    try
		      let bh = DbBlockHeader.dbget h in
		      let bd = DbBlockDelta.dbget h in
		      seocf (seo_block seoc (bh,bd) (blockfile,None))
		    with e ->
		      Printf.printf "WARNING: Exception called when trying to save block %s: %s\n" (hashval_hexstring h) (Printexc.to_string e)
		  end)
	      !snapshot_blocks;
	    List.iter
	      (fun h ->
		if not (Hashtbl.mem fin h) then
		  begin
		    Hashtbl.add fin h ();
                    try
		      let bh = DbBlockHeader.dbget h in
		      seocf (seo_blockheader seoc bh (headerfile,None));
		    with e ->
		      Printf.printf "WARNING: Exception called when trying to save header %s: %s\n" (hashval_hexstring h) (Printexc.to_string e)
		  end)
	      !snapshot_headers;
	    let supp = List.map addr_bitseq !snapshot_addresses in
	    List.iter
	      (fun h -> dbledgersnapshot_ctree_top (ctreeeltfile,hconseltfile,assetfile) fin supp h !snapshot_shards)
	      !snapshot_ledgerroots;
	    close_out headerfile;
	    close_out blockfile;
	    close_out ctreeeltfile;
	    close_out hconseltfile;
	    close_out assetfile;
	    closelog();
	    !exitfn 0;
      end;
    if !importsnapshot then
      begin
	match !snapshot_dir with
	| None ->
	    Printf.printf "No snapshot directory given.\n";
	    !exitfn 1
	| Some(dir) -> (*** then creating a snapshot ***)
	    Printf.printf "Importing snapshot.\n"; flush stdout;
	    let headerfile = open_in_bin (Filename.concat dir "headers") in
	    let blockfile = open_in_bin (Filename.concat dir "blocks") in
	    let ctreeeltfile = open_in_bin (Filename.concat dir "ctreeelts") in
	    let hconseltfile = open_in_bin (Filename.concat dir "hconselts") in
	    let assetfile = open_in_bin (Filename.concat dir "assets") in
	    begin
	      try
		while true do
		  let ((bh,bd),_) = sei_block seic (blockfile,None) in
		  let h = blockheader_id bh in
		  DbBlockHeader.dbput h bh;
		  DbBlockDelta.dbput h bd;
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let (bh,_) = sei_blockheader seic (headerfile,None) in
		  let h = blockheader_id bh in
		  DbBlockHeader.dbput h bh;
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let (c,_) = sei_ctree seic (ctreeeltfile,None) in
		  let h = ctree_hashroot c in
		  DbCTreeElt.dbput h c;
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let ((ah,hr),_) = sei_prod sei_hashval (sei_option (sei_prod sei_hashval sei_int8)) seic (hconseltfile,None) in
		  let (h,_) = nehlist_hashroot (NehConsH(ah,match hr with None -> HNil | Some(hr,l) -> HHash(hr,l))) in
		  DbHConsElt.dbput h (ah,hr)
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let (a,_) = sei_asset seic (assetfile,None) in
		  let aid = assetid a in
		  let h = hashasset a in
		  DbAsset.dbput h a
		done
	      with _ -> ()
	    end;
	    close_in headerfile;
	    close_in blockfile;
	    close_in ctreeeltfile;
	    close_in hconseltfile;
	    close_in assetfile;
	    closelog();
	    !exitfn 0;
      end;
    begin
      match !check_ledger with
      | None -> ()
      | Some(lr) ->
	  let totcants = ref 0L in
	  let totbounties = ref 0L in
	  let rec check_asset h =
	    try
	      let a = DbAsset.dbget h in
	      match a with
	      | (_,_,_,Currency(v)) -> totcants := Int64.add v !totcants
	      | (_,_,_,Bounty(v)) -> totbounties := Int64.add v !totbounties
	      | _ -> ()
	    with Not_found ->
	      Printf.printf "WARNING: asset %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec check_hconselt h =
	    try
	      let (ah,hr) = DbHConsElt.dbget h in
	      check_asset ah;
	      match hr with
	      | Some(h,_) -> check_hconselt h
	      | None -> ()
	    with Not_found ->
	      Printf.printf "WARNING: hconselt %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec check_ledger_rec h =
	    try
	      let c = DbCTreeElt.dbget h in
	      check_ctree_rec c 9
	    with Not_found ->
	      Printf.printf "WARNING: ctreeelt %s is not in database\n" (hashval_hexstring h)
	  and check_ctree_rec c i =
	    match c with
	    | CHash(h) -> check_ledger_rec h
	    | CLeaf(_,NehHash(h,_)) -> check_hconselt h
	    | CLeft(c0) -> check_ctree_rec c0 (i-1)
	    | CRight(c1) -> check_ctree_rec c1 (i-1)
	    | CBin(c0,c1) ->
		check_ctree_rec c0 (i-1);
		check_ctree_rec c1 (i-1)
	    | _ ->
		Printf.printf "WARNING: unexpected non-element ctree at level %d:\n" i;
		print_ctree c
	  in
	  check_ledger_rec lr;
	  Printf.printf "Total Currency Assets: %Ld cants (%s fraenks)\n" !totcants (fraenks_of_cants !totcants);
	  Printf.printf "Total Bounties: %Ld cants (%s fraenks)\n" !totbounties (fraenks_of_cants !totbounties);
	  !exitfn 0
    end;
    begin
      match !build_extraindex with
      | None -> ()
      | Some(lr) ->
	  let rec extraindex_asset h alpha =
	    try
	      let a = DbAsset.dbget h in
	      DbAssetIdAt.dbput (assetid a) alpha
	    with Not_found ->
	      Printf.printf "WARNING: asset %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec extraindex_hconselt h alpha =
	    try
	      let (ah,hr) = DbHConsElt.dbget h in
	      DbHConsEltAt.dbput ah alpha;
	      extraindex_asset ah alpha;
	      match hr with
	      | Some(h,_) -> extraindex_hconselt h alpha
	      | None -> ()
	    with Not_found ->
	      Printf.printf "WARNING: hconselt %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec extraindex_ledger_rec h pl =
	    try
	      let c = DbCTreeElt.dbget h in
	      DbCTreeEltAt.dbput h (List.rev pl);
	      extraindex_ctree_rec c 9 pl
	    with Not_found ->
	      Printf.printf "WARNING: ctreeelt %s is not in database\n" (hashval_hexstring h)
	  and extraindex_ctree_rec c i pl =
	    match c with
	    | CHash(h) -> extraindex_ledger_rec h pl
	    | CLeaf(bl,NehHash(h,_)) -> extraindex_hconselt h (bitseq_addr ((List.rev pl) @ bl))
	    | CLeft(c0) -> extraindex_ctree_rec c0 (i-1) (false::pl)
	    | CRight(c1) -> extraindex_ctree_rec c1 (i-1) (true::pl)
	    | CBin(c0,c1) ->
		extraindex_ctree_rec c0 (i-1) (false::pl);
		extraindex_ctree_rec c1 (i-1) (true::pl)
	    | _ ->
		Printf.printf "WARNING: unexpected non-element ctree at level %d:\n" i;
		print_ctree c
	  in
	  extraindex_ledger_rec lr [];
	  !exitfn 0
    end;
    begin
      match !netlogreport with
      | None -> ()
      | Some([]) ->
	  Printf.printf "Expected -netlogreport <sentlogfile> [<reclogfile>*]\n";
	  !exitfn 1
      | Some(sentf::recfl) ->
	  let extra_log_info mt ms = (*** for certain types of messages, give more information ***)
	    match mt with
	    | Inv ->
		begin
		  let c = ref (ms,String.length ms,None,0,0) in
		  let (n,cn) = sei_int32 seis !c in
		  Printf.printf "Inv msg %ld entries\n" n;
		  c := cn;
		  for j = 1 to Int32.to_int n do
		    let ((i,h),cn) = sei_prod sei_int8 sei_hashval seis !c in
		    c := cn;
		    Printf.printf "Inv %d %s\n" i (hashval_hexstring h);
		  done
		end
	    | GetHeader ->
		begin
		  let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
		  Printf.printf "GetHeader %s\n" (hashval_hexstring h)
		end
	    | GetHeaders ->
		begin
		  let c = ref (ms,String.length ms,None,0,0) in
		  let m = ref 0 in
		  let bhl = ref [] in
		  let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
		  c := cn;
		  Printf.printf "GetHeaders requesting these %d headers:\n" n;
		  for j = 1 to n do
		    let (h,cn) = sei_hashval seis !c in
		    c := cn;
		    Printf.printf "%d. %s\n" j (hashval_hexstring h);
		  done
		end
	    | Headers ->
		begin
		  let c = ref (ms,String.length ms,None,0,0) in
		  let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
		  Printf.printf "Got %d Headers\n" n;
		  c := cn;
		  for j = 1 to n do
		    let (h,cn) = sei_hashval seis !c in
		    let (bh,cn) = sei_blockheader seis cn in
		    c := cn;
		    Printf.printf "%d. %s\n" j (hashval_hexstring h)
		  done
		end
	    | _ -> ()
	  in
	  Printf.printf "++++++++++++++++++++++++\nReport of all sent messages:\n";
	  let f = open_in_bin sentf in
	  begin
	    try
	      while true do
		let (tmstmp,_) = sei_int64 seic (f,None) in
		let gtm = Unix.gmtime (Int64.to_float tmstmp) in
		Printf.printf "Sending At Time: %Ld (UTC %02d %02d %04d %02d:%02d:%02d)\n" tmstmp gtm.Unix.tm_mday (1+gtm.Unix.tm_mon) (1900+gtm.Unix.tm_year) gtm.Unix.tm_hour gtm.Unix.tm_min gtm.Unix.tm_sec;
		let (magic,_) = sei_int32 seic (f,None) in
		if magic = 0x44616c54l then Printf.printf "Testnet message\n" else if magic = 0x44616c4dl then Printf.printf "Mainnet message\n" else Printf.printf "Bad Magic Number %08lx\n" magic;
		let rby = input_byte f in
		if rby = 0 then
		  Printf.printf "Not a reply\n"
		else if rby = 1 then
		  begin
		    let (h,_) = sei_hashval seic (f,None) in
		    Printf.printf "Reply to %s\n" (hashval_hexstring h)
		  end
		else
		  Printf.printf "Bad Reply Byte %d\n" rby;
		let mti = input_byte f in
		Printf.printf "Message type %d: %s\n" mti (try string_of_msgtype (msgtype_of_int mti) with Not_found -> "no such message type");
		let (msl,_) = sei_int32 seic (f,None) in
		Printf.printf "Message contents length %ld bytes\n" msl;
		let (mh,_) = sei_hashval seic (f,None) in
		Printf.printf "Message contents hash %s\n" (hashval_hexstring mh);
		let sb = Buffer.create 100 in
		for i = 1 to (Int32.to_int msl) do
		  let x = input_byte f in
		  Buffer.add_char sb (Char.chr x)
		done;
		let s = Buffer.contents sb in
		Printf.printf "Message contents: %s\n" (string_hexstring s);
		try let mt = msgtype_of_int mti in extra_log_info mt s with Not_found -> ()
	      done
	    with
	    | End_of_file -> ()
	    | e -> Printf.printf "Exception: %s\n" (Printexc.to_string e)
	  end;
	  close_in f;
	  List.iter
	    (fun fn ->
	      Printf.printf "++++++++++++++++++++++++\nReport of all messages received via %s:\n" fn;
	      let f = open_in_bin fn in
	      begin
		try
		  while true do
		    let tmstmp : float = input_value f in
		    let gtm = Unix.gmtime tmstmp in
		    Printf.printf "Received At Time: %f (UTC %02d %02d %04d %02d:%02d:%02d)\n" tmstmp gtm.Unix.tm_mday (1+gtm.Unix.tm_mon) (1900+gtm.Unix.tm_year) gtm.Unix.tm_hour gtm.Unix.tm_min gtm.Unix.tm_sec;
		    let rmmm : hashval option * hashval * msgtype * string = input_value f in
		    let (replyto,mh,mt,m) = rmmm in
		    begin
		      match replyto with
		      | None -> Printf.printf "Not a reply\n"
		      | Some(h) -> Printf.printf "Reply to %s\n" (hashval_hexstring h)
		    end;
		    Printf.printf "Message type %d: %s\n" (int_of_msgtype mt) (string_of_msgtype mt);
		    Printf.printf "Message contents hash %s\n" (hashval_hexstring mh);
		    Printf.printf "Message contents: %s\n" (string_hexstring m);
		    extra_log_info mt m
		  done
		with
		| End_of_file -> ()
		| e -> Printf.printf "Exception: %s\n" (Printexc.to_string e)
	      end;
	      close_in f)
	    recfl;
	  !exitfn 0
    end;
    if !Config.seed = "" && !Config.lastcheckpoint = "" then
      begin
	raise (Failure "Need either a seed (to validate the genesis block) or a lastcheckpoint (to start later in the blockchain); have neither")
      end;
    if not (!Config.seed = "") then
      begin
	if not (String.length !Config.seed = 64) then raise (Failure "Bad seed");
	try
	  genesisstakemod := hexstring_hashval !Config.seed
	with
	| Invalid_argument(_) ->
	    raise (Failure "Bad seed")
      end;
    Printf.printf "Syncing with ltc\n"; flush stdout;
    ltc_init();
    Printf.printf "Initializing blocktree\n"; flush stdout;
    initblocktree();
    missingheaders_th := Some(Thread.create missingheadersthread ());
    begin
      let pdf = Filename.concat datadir "processingdeltas" in
      if Sys.file_exists pdf then
	begin
	  let ch = open_in_bin pdf in
	  try
	    while true do
	      let (h,_) = sei_hashval seic (ch,None) in
	      try
		process_delta h
	      with
	      | End_of_file -> raise End_of_file
	      | e ->
		  Printf.printf "Unexpected exception %s when processing block delta %s. Deleting delta to request it again.\n" (Printexc.to_string e) (hashval_hexstring h);
		  DbBlockDelta.dbdelete h
	    done
	  with End_of_file -> close_in ch; Sys.remove pdf
	end;
    end;
    Printf.printf "Loading wallet\n"; flush stdout;
    Commands.load_wallet();
    Printf.printf "Loading txpool\n"; flush stdout;
    Commands.load_txpool();
    (*** We next compute a nonce for the node to prevent self conns; it doesn't need to be cryptographically secure ***)
    if not !random_initialized then initialize_random_seed();
    let n = rand_int64() in
    this_nodes_nonce := n;
    Printf.fprintf !log "Nonce: %Ld\n" n; flush !log
  end;;

initialize();;
initnetwork();;
if !Config.staking then stkth := Some(Thread.create stakingthread ());;

ltc_listener_th := Some(Thread.create ltc_listener ());;

let last_failure = ref None;;
let failure_count = ref 0;;
let failure_delay() =
  let tm = ltc_medtime() in
  match !last_failure with
  | Some(tm0) ->
      let d = Int64.sub tm tm0 in
      if d > 21600L then (** first failure in 6 hours, reset failure count to 1 and only delay 1 second **)
	begin
	  failure_count := 1;
	  last_failure := Some(tm);
	  Thread.delay 1.0
	end
      else if !failure_count > 100 then (** after 100 regular failures, just exit **)
	begin
	  closelog();
	  !exitfn 1
	end
      else
	begin
	  incr failure_count;
	  last_failure := Some(tm);
	  Thread.delay (float_of_int !failure_count) (** with each new failure, delay for longer **)
	end
  | None ->
      incr failure_count;
      last_failure := Some(tm);
      Thread.delay 1.0

let readevalloop () =
  while true do
    try
      Printf.printf "%s" !Config.prompt; flush stdout;
      let l = read_line() in
      do_command stdout l
    with
    | GettingRemoteData -> Printf.printf "Requested some remote data; try again.\n"
    | Exit -> () (*** silently ignore ***)
    | End_of_file ->
	closelog();
	Printf.printf "Shutting down threads. Please be patient.\n"; flush stdout;
	!exitfn 0
    | Failure(x) ->
	Printf.fprintf stdout "Ignoring Uncaught Failure: %s\n" x; flush stdout;
	failure_delay()
    | exn -> (*** unexpected ***)
	Printf.fprintf stdout "Ignoring Uncaught Exception: %s\n" (Printexc.to_string exn); flush stdout;
	failure_delay()
  done;;

exception Timeout

let run_with_timeout timeout f x =
  let old_handler = Sys.signal Sys.sigalrm
    (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    ignore (f x);
    finish ()
  with Timeout -> finish ()
  | exn -> finish (); raise exn

exception Timeout

let daemon_readevalloop () =
  let lst = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let ia = Unix.inet_addr_of_string "127.0.0.1" in
  begin
    try
      Unix.bind lst (Unix.ADDR_INET(ia,!Config.rpcport));
    with _ ->
      Printf.printf "Cannot bind to rpcport. Quitting.\n";
      !exitfn 1
  end;
  let efn = !exitfn in
  exitfn := (fun n -> Unix.close lst; efn n);
  Unix.listen lst 1;
  while true do
    try
      let (s,a) = Unix.accept lst in
      let sin = Unix.in_channel_of_descr s in
      let sout = Unix.out_channel_of_descr s in
      let alrmh = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
      try
	ignore (Unix.alarm 2);
	let l = input_line sin in
	if not (l = !Config.rpcuser) then raise (Failure "bad rpcuser");
	let l = input_line sin in
	if not (l = !Config.rpcpass) then raise (Failure "bad rpcpass");
	let l = input_line sin in
	ignore (Unix.alarm 60);
	do_command sout l;
	flush sout;
	ignore (Unix.alarm 0);
	ignore (Sys.signal Sys.sigalrm alrmh);
	Unix.close s
      with
      | Timeout -> 
	  flush sout;
	  ignore (Sys.signal Sys.sigalrm alrmh);
	  Unix.close s
      | exn ->
	  flush sout;
	  ignore (Unix.alarm 0);
	  ignore (Sys.signal Sys.sigalrm alrmh);
	  Unix.close s;
	  raise exn
    with
    | Exit -> () (*** silently ignore ***)
    | End_of_file ->
	closelog();
	!exitfn 0
    | Failure(x) ->
	Printf.fprintf !log "Ignoring Uncaught Failure: %s\n" x; flush !log;
	failure_delay()
    | exn -> (*** unexpected ***)
	Printf.fprintf !log "Ignoring Uncaught Exception: %s\n" (Printexc.to_string exn); flush !log;
	failure_delay()
  done;;

if !Config.daemon then
  daemon_readevalloop ()
else
  readevalloop();;
