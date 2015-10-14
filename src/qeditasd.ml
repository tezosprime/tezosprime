(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int;;
open Ser;;
open Hash;;
open Secp256k1;;
open Signat;;
open Assets;;
open Tx;;
open Ctre;;
open Ctregraft;;
open Block;;
open Net;;
open Setconfig;;

let lastcheckpoint : (big_int * int64 * hashval) option ref = ref None;; (*** cumulative stake, block height, blockheaderdata hash ***)
let currstaking : (int64 * big_int * hashval * blockheaderdata option * (stakemod * stakemod * big_int)) option ref = ref None;;

let compute_recid (r,s) k =
  match smulp k _g with
  | Some(x,y) ->
      if eq_big_int x r then
	if evenp y then 0 else 1
      else
	if evenp y then 2 else 3
  | None -> raise (Failure "bad0");;

let lookup_ctree_root_abbrev cr =
  if cr = hexstring_hashval "7b47514ebb7fb6ab06389940224d09df2951e97e" then
    hexstring_hashval "df418292e7c54837ebdd3962cbfee9d4bc8ca981" (*** this is only correct locally; rethink ***)
  else
    let (blkh,ca) = Hashtbl.find recentledgerroots cr in
    ca

let rec fetch_blockheader blkh bhh =
  try
    let (cs,blkh2,bh) = List.assoc bhh !recentblockheaders in
    if blkh = blkh2 then
      bh
    else
      raise (Failure "Found recent header with the hash but it had the wrong height")
  with Not_found ->
    (*** next should look in the appropriate file, but for now say Not_found ***)
    raise Not_found;;

let rec ancestor_of blkh2 bhh2 blkh bhh =
  if blkh > blkh2 then
    try
      let (bhd,bhs) = fetch_blockheader blkh bhh in
      match bhd.prevblockhash with
      | Some(bhh1) ->
	  ancestor_of blkh2 bhh2 (Int64.sub blkh 1L) bhh1
      | None -> false
    with Not_found -> false
  else
    blkh = blkh2 && bhh = bhh2;;

let rec recent_common_ancestor_level blkh bhh1 bhh2 r =
  if bhh1 = bhh2 then
    bhh1
  else if r > 0 then
    let (bhd1,bhs1) = fetch_blockheader blkh bhh1 in
    let (bhd2,bhs2) = fetch_blockheader blkh bhh2 in
    match (bhd1.prevblockhash,bhd2.prevblockhash) with
    | (Some(bhh1p),Some(bhh2p)) ->
	recent_common_ancestor_level (Int64.sub blkh 1L) bhh1p bhh2p (r-1)
    | _ -> raise Not_found
  else
    raise Not_found;;

let rec recent_common_ancestor blkh bhh blkh2 bhh2 r =
  if r > 0 then
    if blkh2 > blkh then
      recent_common_ancestor blkh2 bhh2 blkh bhh r
    else if blkh2 < blkh then
      let (bhd,bhs) = fetch_blockheader blkh bhh in
      match bhd.prevblockhash with
      | Some(bhh1) -> recent_common_ancestor (Int64.sub blkh 1L) bhh1 blkh2 bhh2 (r-1)
      | None -> raise Not_found
    else
      recent_common_ancestor_level blkh bhh bhh2 r
  else
    raise Not_found;;

let no_recent_common_ancestor blkh2 bhh2 blkh bhh r =
  try
    ignore (recent_common_ancestor blkh2 bhh2 blkh bhh r);
    false
  with Not_found -> true;;

(*** cannot double sign within six blocks (possible penalty of forfeiture of rewards) ***)
let prohibited blkh bhh =
  try
    ignore (List.find
	      (fun (blkh2,bhh2) ->
		(Int64.abs (Int64.sub blkh2 blkh) <= 6L) &&
	  (*** prohibited unless it's an ancestor of bhh or the fork is longer than 6 blocks ***)
		not (ancestor_of blkh2 bhh2 blkh bhh || no_recent_common_ancestor blkh2 bhh2 blkh bhh 6)
	      )
	      !recentcommitments);
    true
  with Not_found -> false;;

let beststakingoption () =
  (*** first try to find a recent blockheader with the most cumulative stake ***)
  try
    let (bhh,(cs,blkh,(bhd,bhs))) =
      List.find
	(fun (bhh,(cs,blkh,(bhd,bhs))) -> not (prohibited blkh bhh))
	!recentblockheaders
    in
    let (csm1,fsm1,tar1) = bhd.tinfo in
    let csm2 = stakemod_pushbit (stakemod_lastbit fsm1) csm1 in
    let tar2 = retarget tar1 bhd.deltatime in
    (Int64.add blkh 1L,cs,bhd.newledgerroot,bhd.timestamp,Some(bhd),(csm2,fsm1,tar2))
  with Not_found ->
    (*** next fall back on the last checkpoint, if there is one ***)
    match !lastcheckpoint with
    | Some(cs,blkh,bhh) ->
	let (bhd,bhs) = fetch_blockheader blkh bhh in
	let (csm1,fsm1,tar1) = bhd.tinfo in
	let csm2 = stakemod_pushbit (stakemod_lastbit fsm1) csm1 in
	let tar2 = retarget tar1 bhd.deltatime in
	(Int64.add blkh 1L,cs,bhd.newledgerroot,bhd.timestamp,Some(bhd),(csm2,fsm1,tar2))
    | None ->
	(*** finally assume we are starting from the genesis ledger ***)
	(1L,zero_big_int,hexstring_hashval "7b47514ebb7fb6ab06389940224d09df2951e97e",!genesistimestamp,None,(!genesiscurrentstakemod,!genesisfuturestakemod,!genesistarget));;

let stakingproccomm : (in_channel * out_channel * in_channel) option ref = ref None;;

let fallbacknodes = [
"108.61.219.125:20805"
];;

let testnetfallbacknodes = [
"108.61.219.125:20804"
];;

let random_int32_array : int32 array = [| 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l; 0l |];;
let random_initialized : bool ref = ref false;;

(*** generate 512 random bits and then use sha256 on them each time we need a new random number ***)
let initialize_random_seed () =
  let r = open_in_bin "/dev/random" in
  let v = ref 0l in
  for i = 0 to 15 do
    v := 0l;
    for j = 0 to 3 do
      v := Int32.logor (Int32.shift_left !v 8) (Int32.of_int (input_byte r))
    done;
    random_int32_array.(i) <- !v;
  done;
  random_initialized := true;;

let sha256_random_int32_array () =
  Sha256.sha256init();
  for i = 0 to 15 do
    Sha256.currblock.(i) <- random_int32_array.(i)
  done;
  Sha256.sha256round();
  let (x7,x6,x5,x4,x3,x2,x1,x0) = Sha256.getcurrmd256() in
  for i = 0 to 7 do
    random_int32_array.(i+8) <- random_int32_array.(i)
  done;
  random_int32_array.(0) <- x0;
  random_int32_array.(1) <- x1;
  random_int32_array.(2) <- x2;
  random_int32_array.(3) <- x3;
  random_int32_array.(4) <- x4;
  random_int32_array.(5) <- x5;
  random_int32_array.(6) <- x6;
  random_int32_array.(7) <- x7;;

let rand_256 () =
  if not !random_initialized then initialize_random_seed();
  sha256_random_int32_array();
  Sha256.md256_big_int (Sha256.getcurrmd256())

let rand_bit () =
  if not !random_initialized then initialize_random_seed();
  sha256_random_int32_array();
  random_int32_array.(0) < 0l

let rand_int64 () =
  if not !random_initialized then initialize_random_seed();
  sha256_random_int32_array();
  Int64.logor
    (Int64.of_int32 random_int32_array.(0))
    (Int64.shift_right_logical (Int64.of_int32 random_int32_array.(1)) 32);;

let myaddr () =
  match !Config.ip with
  | Some(ip) -> 
      if !Config.ipv6 then
	"[" ^ ip ^ "]:" ^ (string_of_int !Config.port)
      else
	ip ^ ":" ^ (string_of_int !Config.port)
  | None ->
      "";;

let initialize_conn_accept s =
  if List.length !conns < !Config.maxconns then
    begin
      let sin = Unix.in_channel_of_descr s in
      let sout = Unix.out_channel_of_descr s in
      set_binary_mode_in sin true;
      set_binary_mode_out sout true;
      try
	let m2 = rec_msg_nohang sin 5.0 5.0 in
	match m2 with
	| Some(_,_,Version(vers2,srvs2,tm2,addr_recv2,addr_from2,n2,user_agent2,fr20,fr21,fr22,first_header_height2,first_full_height2,last_height2,relay2,lastchkpt2)) ->
	    send_msg sout Verack;
	    let vers = 1l in
	    let srvs = 1L in
	    let tm = Int64.of_float(Unix.time()) in
	    let nonce = rand_int64() in
	    let user_agent = "Qeditas-Testing-Phase" in
	    let fr0 = RFAll in
	    let fr1 = RFAll in
	    let fr2 = RFAll in
	    let first_header_height = 0L in
	    let first_full_height = 0L in
	    let last_height = 0L in
	    let relay = true in
	    let lastchkpt = None in
	    send_msg sout (Version(vers,srvs,tm,addr_from2,myaddr(),nonce,user_agent,fr0,fr1,fr2,first_header_height,first_full_height,last_height,relay,lastchkpt));
	    let m1 = rec_msg_nohang sin 5.0 5.0 in
	    begin
	      match m1 with
	      | Some(_,_,Verack) ->
		  Printf.printf "Added connection; post handshake\nmy time = %Ld\ntheir time = %Ld\naddr_recv2 = %s\naddr_from2 = %s\n" tm tm2 addr_recv2 addr_from2; flush stdout;
		  let cs =
		    { alive = true;
		      lastmsgtm = Unix.time();
		      pending = [];
		      rframe0 = fr20;
		      rframe1 = fr21;
		      rframe2 = fr22;
		      first_header_height = first_header_height2;
		      first_full_height = first_full_height2;
		      last_height = last_height2;
		    }
		  in
		  conns := (s,sin,sout,addr_from2,cs)::!conns;
		  true
	      | _ ->
		  Printf.printf "Handshake failed. (No Verack)\n"; flush stdout;
		  Unix.close s;
		  false
	    end
	| _ ->
	    Printf.printf "Handshake failed. (No Version)\n"; flush stdout;
	    Unix.close s; (*** handshake failed ***)
	    false
      with
      | IllformedMsg ->
	  Printf.printf "Handshake failed. (IllformedMsg)\n"; flush stdout;
	  Unix.close s; (*** handshake failed ***)
	  false
    end
  else
    begin
      Printf.printf "Rejecting connection because of maxconns.\n"; flush stdout;
      Unix.close s;
      false
    end;;

let initialize_conn_2 n s sin sout =
  (*** handshake ***)
  let vers = 1l in
  let srvs = 1L in
  let tm = Int64.of_float(Unix.time()) in
  let nonce = rand_int64() in
  let user_agent = "Qeditas-Testing-Phase" in
  let fr0 = RFAll in
  let fr1 = RFAll in
  let fr2 = RFAll in
  let first_header_height = 0L in
  let first_full_height = 0L in
  let last_height = 0L in
  let relay = true in
  let lastchkpt = None in
  send_msg sout (Version(vers,srvs,tm,myaddr(),n,nonce,user_agent,fr0,fr1,fr2,first_header_height,first_full_height,last_height,relay,lastchkpt));
  try
    let m1 = rec_msg_nohang sin 5.0 5.0 in
    match m1 with
    | Some(_,_,Verack) ->
	begin
	  let m2 = rec_msg_nohang sin 5.0 5.0 in
	  match m2 with
	  | Some(_,_,Version(vers2,srvs2,tm2,addr_recv2,addr_from2,n2,user_agent2,fr20,fr21,fr22,first_header_height2,first_full_height2,last_height2,relay2,lastchkpt2)) ->
	      send_msg sout Verack;
	      Printf.printf "Added connection; post handshake\nmy time = %Ld\ntheir time = %Ld\naddr_recv2 = %s\naddr_from2 = %s\n" tm tm2 addr_recv2 addr_from2; flush stdout;
	      let cs =
		{ alive = true;
		  lastmsgtm = Unix.time();
		  pending = [];
		  rframe0 = fr20;
		  rframe1 = fr21;
		  rframe2 = fr22;
		  first_header_height = first_header_height2;
		  first_full_height = first_full_height2;
		  last_height = last_height2;
		}
	      in
	      conns := (s,sin,sout,addr_from2,cs)::!conns;
	      true
	  | _ ->
	      Printf.printf "Handshake failed. (No Version)\n"; flush stdout;
	      Unix.close s; (*** handshake failed ***)
	      false
	end
    | _ ->
	begin (*** handshake failed ***)
	  Printf.printf "Handshake failed. (No Verack)\n"; flush stdout;
	  Unix.close s;
	  false
	end
  with
  | IllformedMsg ->
      Printf.printf "Handshake failed. (IllformedMsg)\n"; flush stdout;
      Unix.close s; (*** handshake failed ***)
      false;;

let initialize_conn n s =
  let sin = Unix.in_channel_of_descr s in
  let sout = Unix.out_channel_of_descr s in
  set_binary_mode_in sin true;
  set_binary_mode_out sout true;
  initialize_conn_2 n s sin sout;;

exception Connected;;

let search_for_conns () =
  if !conns = [] then
    begin
      try
	List.iter
	  (fun n ->
	    let (ip,port,v6) = extract_ip_and_port n in
	    if not (!Config.ip = Some(ip) && !Config.ipv6 = v6) then (*** if this is a fallback node, do not connect to itself ***)
	      begin
		try
		  match !Config.socks with
		  | None ->
		      let s = connectpeer ip port in
		      ignore (initialize_conn n s)
		  | Some(4) ->
		      let (s,sin,sout) = connectpeer_socks4 !Config.socksport ip port in
		      ignore (initialize_conn_2 n s sin sout)
		  | Some(5) ->
		      raise (Failure "socks5 is not yet supported")
		  | Some(z) ->
		      raise (Failure ("socks" ^ (string_of_int z) ^ " is not yet supported"))
		with
		| RequestRejected -> Printf.printf "RequestRejected\n"; flush stdout;
		| Connected -> raise Connected
		| _ -> ()
	      end
	  )
	(if !Config.testnet then testnetfallbacknodes else fallbacknodes)
      with Connected -> ()
    end;;

let rec hlist_insertstakingassets tostkr alpha hl =
  match hl with
  | HCons((aid,bday,obl,Currency(v)),hr) ->
      Commands.stakingassets := (alpha,aid,bday,obl,v)::!Commands.stakingassets;
      output_byte tostkr 67;
      seocf (seo_hashval seoc alpha (tostkr,None));
      seocf (seo_hashval seoc aid (tostkr,None));
      seocf (seo_int64 seoc bday (tostkr,None));
      seocf (seo_int64 seoc v (tostkr,None));
      seocf (seo_obligation seoc obl (tostkr,None));
      hlist_insertstakingassets tostkr alpha hr
  | HCons(_,hr) -> hlist_insertstakingassets tostkr alpha hr
  | _ -> ();;

let send_assets_to_staker tostkr c =
  let reasontostake = ref false in
  List.iter
    (fun (k,b,(x,y),w,h,alpha) ->
      match ctree_addr (hashval_p2pkh_addr h) c with
      | (Some(CLeaf(_,hl)),_) ->
	  reasontostake := true;
	  hlist_insertstakingassets tostkr h (nehlist_hlist hl)
      | _ ->
	  ()
    )
    !Commands.walletkeys;
  List.iter
    (fun (alpha,beta,_,_,_) ->
      let (p,x4,x3,x2,x1,x0) = alpha in
      let (q,_,_,_,_,_) = beta in
      if not p && not q then (*** only p2pkh can stake ***)
	begin
	  match ctree_addr (payaddr_addr alpha) c with
	  | (Some(CLeaf(_,hl)),_) ->
	      reasontostake := true;
	      hlist_insertstakingassets tostkr (x4,x3,x2,x1,x0) (nehlist_hlist hl)
	  | _ ->
	      ()		
	end)
    !Commands.walletendorsements;
  flush tostkr;
  !reasontostake;;

let main () =
  begin
    process_config_args();
    process_config_file();
    if !Config.seed = "" && !Config.lastcheckpoint = "" then
      begin
	raise (Failure "Need either a seed (to validate the genesis block) or a lastcheckpoint (to start later in the blockchain); have neither")
      end;
    if not (!Config.seed = "") then
      begin
	if not (String.length !Config.seed = 40) then raise (Failure "Bad seed");
	try
	  set_genesis_stakemods !Config.seed
	with
	| Invalid_argument(_) ->
	    raise (Failure "Bad seed")
      end;
    if !Config.testnet then
      begin
	max_target := shift_left_big_int unit_big_int 255; (*** make the max_target much higher (so difficulty can be easier for testing) ***)
	genesistarget := shift_left_big_int unit_big_int 245; (*** make the genesistarget much higher (so difficulty can be easier for testing) ***)
      end;
    localframe := Commands.load_currentframe();
    localframehash := hashframe !localframe;
    Printf.printf "Initializing random seed\n"; flush stdout;
    if not !random_initialized then initialize_random_seed();
    let l = 
      match !Config.ip with
      | Some(ip) ->
	  let l = openlistener ip !Config.port 5 in
	  Printf.printf "Listening for incoming connections.\n";
	  flush stdout;
	  Some(l)
      | None ->
	  Printf.printf "Not listening for incoming connections.\nIf you want Qeditas to listen for incoming connections set ip to your ip address\nusing ip=... in qeditas.conf or -ip=... on the command line.\n";
	  flush stdout;
	  None
    in
    if !Config.staking then (*** start a staking process ***)
      begin
	let stkexec = Filename.concat (Filename.dirname (Sys.argv.(0))) "qeditasstk" in
	Printf.printf "about to start staker\n"; flush stdout;
	let (fromstkr,tostkr,stkerr) = Unix.open_process_full stkexec [||] in
	Printf.printf "started staker\n"; flush stdout;
	stakingproccomm := Some(fromstkr,tostkr,stkerr);
	Commands.load_wallet();
	try
	  let (blkh,cs,currledgerroot,tm,bho,(csm,fsmprev,tar)) = beststakingoption () in
	  let ca = lookup_frame_ctree_root_abbrev !localframehash currledgerroot in
	  if send_assets_to_staker tostkr (CAbbrev(currledgerroot,ca)) then
	    begin
	      output_byte tostkr 66; (*** send the staking process the block height, the target, the stake modifier and the next allowed timestamp ***)
	      seocf (seo_int64 seoc blkh (tostkr,None));
	      seocf (seo_big_int_256 seoc tar (tostkr,None));
	      seocf (seo_stakemod seoc csm (tostkr,None));
	      output_byte tostkr 116;
	      seocf (seo_int64 seoc (Int64.add 1L tm) (tostkr,None));
	      output_byte tostkr 83; (*** start staking ***)
	      flush tostkr;
	      currstaking := Some(blkh,cs,currledgerroot,bho,(csm,fsmprev,tar))
	    end
	  else
	    begin
	      Printf.printf "No wallet assets to stake in the current ctree. Not staking at the moment.\n";
	      flush stdout;
	    end
	with Not_found ->
	  Printf.printf "Could not determine a starting point for staking. Not staking.\n";
	  flush stdout;
      end;
    sethungsignalhandler();
    search_for_conns ();
    while true do (*** main process loop ***)
      try
	begin (*** if staking check to see if staking has found a hit ***)
	  match !stakingproccomm with
	  | None -> ()
	  | Some(fromstkr,tostkr,stkerr) ->
	      try
		match input_byte_nohang fromstkr 0.1 with
		| Some(z) when z = 72 -> (*** hit with no storage ***)
		    (*** pause staking ***)
		    Printf.printf "Pausing staking since found hit\n"; flush stdout;
		    output_byte tostkr 80;
		    flush tostkr;
		    let c = (fromstkr,None) in
		    let (stktm,c) = sei_int64 seic c in
		    let (alpha,c) = sei_hashval seic c in
		    let alpha2 = hashval_p2pkh_addr alpha in
		    let (aid,_) = sei_hashval seic c in
		    Printf.printf "Asset %s at address %s can stake at time %Ld (%Ld seconds from now)\n" (hashval_hexstring aid) (Cryptocurr.addr_qedaddrstr alpha2) stktm (Int64.sub stktm (Int64.of_float (Unix.time())));
		    flush stdout;
		    begin
		      try
			let (_,_,bday,obl,v) = List.find (fun (_,h,_,_,_) -> h = aid) !Commands.stakingassets in
			  match !currstaking with
			  | Some(blkh,cs,prevledgerroot,pbh,(csm,fsmprev,tar)) ->
			      if check_hit_b blkh bday obl v csm tar stktm aid alpha None then (*** confirm the staking process is correct ***)

				let (pbhtm,pbhh) =
				  match pbh with
				  | Some(pbh) -> (pbh.timestamp,Some(hash_blockheaderdata pbh))
				  | None -> (!genesistimestamp,None)
				in
				let newrandbit = rand_bit() in
				let fsm = stakemod_pushbit newrandbit fsmprev in
				let (csm,fsm,tar) =
				  if blkh = 1L then
				    (!genesiscurrentstakemod,!genesisfuturestakemod,!genesistarget)
				  else
				    (csm,fsm,tar)
				in
				let stkoutl = [(alpha2,(None,Currency(v)));(alpha2,(Some(p2pkhaddr_payaddr alpha,Int64.add blkh reward_locktime,true),Currency(rewfn blkh)))] in
				let coinstk : tx = ([(alpha2,aid)],stkoutl) in
				let prevcforblock =
				  match
				    get_tx_supporting_octree
				      coinstk
				      (Some(CAbbrev(prevledgerroot,lookup_ctree_root_abbrev prevledgerroot)))
				  with
				  | Some(c) -> c
				  | None -> raise (Failure "ctree should not have become empty")
				in
				let (prevcforheader,cgr) = factor_inputs_ctree_cgraft [(alpha2,aid)] prevcforblock in
				let (newcr,newca) =
				  match tx_octree_trans blkh coinstk (Some(CAbbrev(prevledgerroot,lookup_ctree_root_abbrev prevledgerroot))) with
				  | None -> raise (Failure "ctree should not have become empty")
				  | Some(c) ->
				      match frame_filter_ctree (wrap_frame !localframe) c with
				      | CAbbrev(cr,ca) -> (cr,ca)
				      | _ -> raise (Failure "frame_filter_ctree was given a wrapped frame but did not return an abbrev")
				in
				Hashtbl.add recentledgerroots newcr (blkh,newca); (*** remember the association so the relevant parts of the new ctree can be reloaded when needed ***)
				let bhdnew : blockheaderdata
				    = { prevblockhash = pbhh;
					newtheoryroot = None; (*** leave this as None for now ***)
					newsignaroot = None; (*** leave this as None for now ***)
					newledgerroot = newcr;
					stakeaddr = alpha;
					stakeassetid = aid;
					stored = None;
					timestamp = stktm;
					deltatime = Int64.to_int32 (Int64.sub stktm pbhtm);
					tinfo = (csm,fsm,tar);
					prevledger = prevcforheader
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
				      let (_,beta,recid,fcomp,esg) =
					List.find
					  (fun (alpha2,beta,recid,fcomp,esg) ->
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
				      raise (Failure("Was staking for " ^ Cryptocurr.addr_qedaddrstr (hashval_p2pkh_addr alpha) ^ " but have neither the private key nor an appropriate endorsement for it."))
				in
				let bhnew = (bhdnew,bhsnew) in
				let bdnew : blockdelta =
				  { stakeoutput = stkoutl;
				    forfeiture = None; (*** leave this as None for now; should check for double signing ***)
				    prevledgergraft = cgr;
				    blockdelta_stxl = []
				  }
				in
				if blkh = 1L then
				  if valid_blockheaderchain blkh (bhnew,[]) then (*** first block, special conditions ***)
				    (Printf.printf "Valid first block.\n"; flush stdout)
				  else
				    (Printf.printf "Not a valid first block.\n"; flush stdout)
				else
				  begin
				    if valid_block None None blkh (bhnew,bdnew) then
				      (Printf.printf "New block is valid\n"; flush stdout)
				    else
				      (Printf.printf "New block is not valid\n"; flush stdout);
				    match pbh with
				    | None -> Printf.printf "No previous block but block height not 1\n"; flush stdout
				    | Some(pbhd) ->
					let tmpsucctest bhd1 bhd2 =
					  bhd2.timestamp = Int64.add bhd1.timestamp (Int64.of_int32 bhd2.deltatime)
					    &&
					  let (csm1,fsm1,tar1) = bhd1.tinfo in
					  let (csm2,fsm2,tar2) = bhd2.tinfo in
					  stakemod_pushbit (stakemod_lastbit fsm1) csm1 = csm2 (*** new stake modifier is old one shifted with one new bit from the future stake modifier ***)
					    &&
					  stakemod_pushbit (stakemod_firstbit fsm2) fsm1 = fsm2 (*** the new bit of the new future stake modifier fsm2 is freely chosen by the staker ***)
					    &&
					  eq_big_int tar2 (retarget tar1 bhd1.deltatime)
					in
					if tmpsucctest pbhd bhdnew then
					  (Printf.printf "Valid successor block\n"; flush stdout)
					else
					  (Printf.printf "Not a valid successor block\n"; flush stdout)
				  end;
				let csnew = cumul_stake cs tar bhdnew.deltatime in
				waitingblock := Some(stktm,blkh,bhdnewh,bhnew,bdnew,csnew);
			  | None -> (*** error, but ignore for now ***)
			      Printf.printf "creating block error\n"; flush stdout;
			      ()
		      with Not_found -> ()
		    end;
		    flush stdout;
		| Some(z) when z = 83 -> (*** hit with storage ***)
		    let c = (fromstkr,None) in
		    let (stktm,c) = sei_int64 seic c in
		    let (alpha,c) = sei_hashval seic c in
		    let (aid,c) = sei_hashval seic c in
		    let (strg,_) = sei_postor seic c in
		    Printf.printf "Found a hit using a stake combined with storage, but the code to handle it isn't written.\n";
		    flush stdout
		| Some(z) -> (*** something has gone wrong. die ***)
		    Printf.printf "The staking process sent back %d which is meaningless.\nKilling staker\n" z;
		    Unix.close_process_full (fromstkr,tostkr,stkerr);
		    stakingproccomm := None
		| None ->
		    match input_byte_nohang stkerr 0.1 with
		    | Some(z) -> (*** something has gone wrong. die ***)
			Printf.printf "The staking process sent error code %d.\nKilling staker\n" z;
			Unix.close_process_full (fromstkr,tostkr,stkerr);
			stakingproccomm := None
		    | None -> ()
	      with
	      | _ ->
		  Printf.printf "Exception thrown while trying to read from the staking process.\nKilling staker\n";
		  Unix.close_process_full (fromstkr,tostkr,stkerr);
		  stakingproccomm := None
	end;
	begin (*** check to see if a new block can be published ***)
	  match !waitingblock with
	  | Some(stktm,blkh,bhh,bh,bd,cs) when Int64.of_float (Unix.time()) >= stktm ->
	      waitingblock := None;
	      insertnewblockheader bhh cs true blkh bh;
	      broadcast_msg (Inv([(1,blkh,bhh)])); (*** broadcast it with Inv ***)
	      begin
		match !stakingproccomm with
		| Some(fromstkr,tostkr,stkerr) ->
		    begin (*** restart staking ***)
		      match !currstaking with
		      | Some(_) ->
			  (*** pause staking ***)
			  output_byte tostkr 80;
			  flush tostkr;
		      | None -> ()
		    end;
		    let (blkh,cs,currledgerroot,tm,bho,(csm,fsmprev,tar)) = beststakingoption () in
		    let ca = lookup_frame_ctree_root_abbrev !localframehash currledgerroot in
		    output_byte tostkr 114; (*** remove all staking and storage assets **)
		    if send_assets_to_staker tostkr (CAbbrev(currledgerroot,ca)) then
		      begin
			output_byte tostkr 66; (*** send the staking process the block height, the target, the stake modifier and the next allowed timestamp ***)
			seocf (seo_int64 seoc blkh (tostkr,None));
			seocf (seo_big_int_256 seoc tar (tostkr,None));
			seocf (seo_stakemod seoc csm (tostkr,None));
			output_byte tostkr 116;
			seocf (seo_int64 seoc (Int64.add 1L tm) (tostkr,None));
			output_byte tostkr 83; (*** start staking ***)
			flush tostkr;
			Printf.printf "Starting staking for block at height %Ld, currledgerroot %s, target %s\n" blkh (hashval_hexstring currledgerroot) (string_of_big_int tar); flush stdout;
			currstaking := Some(blkh,cs,currledgerroot,bho,(csm,fsmprev,tar))
		      end
		    else
		      begin
			Printf.printf "No wallet assets to stake in the current ctree. Not staking at the moment.\n";
			flush stdout;
		      end
		| None -> ()
	      end
	  | None -> (*** if there is no waiting block and we aren't staking, then restart staking ***)
	      begin
		match !stakingproccomm with
		| Some(fromstkr,tostkr,stkerr) ->
		    begin
		      match !currstaking with
		      | None ->
			  let (blkh,cs,currledgerroot,tm,bho,(csm,fsmprev,tar)) = beststakingoption () in
			  let ca = lookup_frame_ctree_root_abbrev !localframehash currledgerroot in
			  output_byte tostkr 114; (*** remove all staking and storage assets **)
			  if send_assets_to_staker tostkr (CAbbrev(currledgerroot,ca)) then
			    begin
			      output_byte tostkr 66; (*** send the staking process the block height, the target, the stake modifier and the next allowed timestamp ***)
			      seocf (seo_int64 seoc blkh (tostkr,None));
			      seocf (seo_big_int_256 seoc tar (tostkr,None));
			      seocf (seo_stakemod seoc csm (tostkr,None));
			      output_byte tostkr 116;
			      seocf (seo_int64 seoc (Int64.add 1L tm) (tostkr,None));
			      output_byte tostkr 83; (*** start staking ***)
			      flush tostkr;
			      Printf.printf "Starting staking for block at height %Ld, currledgerroot %s, target %s\n" blkh (hashval_hexstring currledgerroot) (string_of_big_int tar); flush stdout;
			      currstaking := Some(blkh,cs,currledgerroot,bho,(csm,fsmprev,tar))
			    end
			  else
			    begin
			      Printf.printf "No wallet assets to stake in the current ctree. Not staking at the moment.\n";
			      flush stdout;
			    end		    
		      | Some(_,currcs,_,_,_) -> (*** if we are staking, make sure it's still on top of the best block ***)
			  let (blkh,cs,currledgerroot,tm,bho,(csm,fsmprev,tar)) = beststakingoption () in
			  if lt_big_int currcs cs then
			    begin
			      output_byte tostkr 80;
			      flush tostkr;
			      let ca = lookup_frame_ctree_root_abbrev !localframehash currledgerroot in
			      output_byte tostkr 114; (*** remove all staking and storage assets **)
			      if send_assets_to_staker tostkr (CAbbrev(currledgerroot,ca)) then
				begin
				  output_byte tostkr 66; (*** send the staking process the block height, the target, the stake modifier and the next allowed timestamp ***)
				  seocf (seo_int64 seoc blkh (tostkr,None));
				  seocf (seo_big_int_256 seoc tar (tostkr,None));
				  seocf (seo_stakemod seoc csm (tostkr,None));
				  output_byte tostkr 116;
				  seocf (seo_int64 seoc (Int64.add 1L tm) (tostkr,None));
				  output_byte tostkr 83; (*** start staking ***)
				  flush tostkr;
				  Printf.printf "Starting staking for block at height %Ld, currledgerroot %s, target %s\n" blkh (hashval_hexstring currledgerroot) (string_of_big_int tar); flush stdout;
				  currstaking := Some(blkh,cs,currledgerroot,bho,(csm,fsmprev,tar))
				end
			      else
				begin
				  Printf.printf "No wallet assets to stake in the current ctree. Not staking at the moment.\n";
				  flush stdout;
				end
			    end
		      | _ -> ()
		    end
	      end
	  | _ -> ()
	end;
	begin (*** possibly check for a new incoming connection ***)
	  match l with
	  | Some(l) ->
	      begin
		match accept_nohang l 0.1 with
		| Some(s,a) ->
		    begin
		      match a with
		      | Unix.ADDR_UNIX(x) ->
			Printf.printf "got local connection %s\n" x;
		      | Unix.ADDR_INET(x,y) ->
			  Printf.printf "got remote connection %s %d\n" (Unix.string_of_inet_addr x) y;
		    end;
		    flush stdout;
		    if initialize_conn_accept s then
		      Printf.printf "accepted remote connection\n"
		    else
		      Printf.printf "rejected remote connection\n";
		    flush stdout;
		| None -> ()
	      end
	  | None -> ()
	end;
	(*** check each connection for possible messages ***)
	List.iter
	  (fun (s,sin,sout,peeraddr,cs) ->
	    try
	      let tm = Unix.time() in
	      match rec_msg_nohang sin 0.1 1.0 with
	      | None ->
		  begin
		    try
		      ignore (List.find (fun (h,p,tm1,tm2,f) -> p && (tm -. tm2 > 90.0)) cs.pending);
			(*** Something that required a response didn't respond in time (e.g., a Ping).
			     Drop the connection
			 ***)
                      Printf.printf "Ping-Pong failed. Dropping connection.\n"; flush stdout;
                      Unix.close s;
                      cs.alive <- false
                    with Not_found ->
		    if (tm -. cs.lastmsgtm) > 5400.0 then (*** If no messages in enough time (90 minutes), send a ping. ***)
		      begin
			Printf.printf "Sending Ping.\n"; flush stdout;
			let mh = send_msg sout Ping in
			Printf.printf "Sent Ping.\n"; flush stdout;
                        cs.pending <- (mh,true,tm,tm,None)::cs.pending;
                        cs.lastmsgtm <- tm
		      end
		  end
	      | Some(replyto,mh,m) ->
		begin
		  Printf.printf "got a msg, new lastmsgtm %f\n" tm; flush stdout;
                  cs.lastmsgtm <- tm;
		  handle_msg sin sout cs replyto mh m
		end
	    with
	    | End_of_file ->
		Printf.printf "Lost connection.\n"; flush stdout;
		cs.alive <- false
	    | IllformedMsg ->
		Printf.printf "IllformedMsg. Breaking connection.\n"; flush stdout;
		cs.alive <- false
	    | exn -> (*** unexpected ***)
		Printf.printf "Other exception: %s\nNot dropping connection yet.\n" (Printexc.to_string exn);
	  )
	  !conns;
	conns := List.filter (fun (s,sin,sout,peeraddr,cs) -> cs.alive) !conns
      with
      | Failure(x) -> Printf.printf "Failure: %s\n...but continuing\n" x; flush stdout
      | _ -> () (*** ensuring no exception escapes the main loop ***)
    done
  end;;

let stop_staking () =
  match !stakingproccomm with
  | Some(sti,sto,ste) -> ignore (Unix.close_process_full (sti,sto,ste))
  | None -> ();;

try
  main ()
with
| Failure(x) ->
    Printf.printf "%s\nExiting.\n" x;
    stop_staking ()
| exn -> (*** unexpected ***)
    Printf.printf "Exception: %s\nExiting.\n" (Printexc.to_string exn);
    stop_staking ();;
