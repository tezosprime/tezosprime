(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Json
open Ser
open Sha256
open Hash
open Net
open Db
open Mathdata
open Assets
open Secp256k1
open Cryptocurr
open Signat
open Script

type tx = addr_assetid list * addr_preasset list

let hashtx (inpl,outpl) =
  hashpair
    (hashlist (List.map hash_addr_assetid inpl))
    (hashlist (List.map hash_addr_preasset outpl))

let tx_inputs ((inpl,outpl):tx) = inpl
let tx_outputs ((inpl,outpl):tx) = outpl

let rec no_dups l =
  match l with
  | [] -> true
  | x::r when List.mem x r -> false
  | _::r -> no_dups r

let tx_inputs_valid inpl =
  match inpl with
  | [] -> false
  | _ -> no_dups inpl

let tx_inputs_valid_oc oc inpl =
  match inpl with
  | [] ->
      Printf.fprintf oc "tx invalid since 0 inputs\n";
      false
  | _ ->
      if no_dups inpl then
	true
      else
	begin
	  Printf.fprintf oc "tx invalid since duplicate inputs\n";
	  false
	end

(*** Ensure at most one owner is declared for each object/proposition ***)
let rec tx_outputs_valid_one_owner outpl ool pol nol =
  match outpl with
  | (alpha,(_,OwnsObj(h,beta,io)))::outpr ->
      if List.mem h ool then
	false
      else
	tx_outputs_valid_one_owner outpr (h::ool) pol nol
  | (alpha,(_,OwnsProp(h,beta,io)))::outpr ->
      if List.mem h pol then
	false
      else
	tx_outputs_valid_one_owner outpr ool (h::pol) nol
  | (alpha,(_,OwnsNegProp))::outpr ->
      if List.mem alpha nol then
	false
      else
	tx_outputs_valid_one_owner outpr ool pol (alpha::nol)
  | _::outpr -> tx_outputs_valid_one_owner outpr ool pol nol
  | [] -> true

let rec tx_outputs_valid_one_owner_oc oc outpl ool pol nol =
  match outpl with
  | (alpha,(_,OwnsObj(h,beta,io)))::outpr ->
      if List.mem h ool then
	(Printf.fprintf oc "tx invalid since two OwnObj given for %s\n" (hashval_hexstring h);
	 false)
      else
	tx_outputs_valid_one_owner_oc oc outpr (h::ool) pol nol
  | (alpha,(_,OwnsProp(h,beta,io)))::outpr ->
      if List.mem h pol then
	(Printf.fprintf oc "tx invalid since two OwnsProp given for %s\n" (hashval_hexstring h);
	 false)
      else
	tx_outputs_valid_one_owner_oc oc outpr ool (h::pol) nol
  | (alpha,(_,OwnsNegProp))::outpr ->
      if List.mem alpha nol then
	(Printf.fprintf oc "tx invalid since two owners for OwnsNegProp given: %s\n" (Cryptocurr.addr_daliladdrstr alpha);
	 false)
      else
	tx_outputs_valid_one_owner_oc oc outpr ool pol (alpha::nol)
  | _::outpr -> tx_outputs_valid_one_owner_oc oc outpr ool pol nol
  | [] -> true

(*** Ensure ownership deeds are sent to term addresses and publications are sent to publication addresses. ***)
let rec tx_outputs_valid_addr_cats outpl =
  match outpl with
  | (alpha,(_,Bounty(_)))::outpr -> termaddr_p alpha && tx_outputs_valid_addr_cats outpr (*** bounties should only be published to term addresses ***)
  | (alpha,(_,OwnsObj(h,beta,u)))::outpr -> hashval_term_addr h = alpha && tx_outputs_valid_addr_cats outpr
  | (alpha,(_,OwnsProp(h,beta,u)))::outpr -> hashval_term_addr h = alpha && tx_outputs_valid_addr_cats outpr
  | (alpha,(_,OwnsNegProp))::outpr -> termaddr_p alpha && tx_outputs_valid_addr_cats outpr
  | (alpha,(_,TheoryPublication(beta,h,dl)))::outpr ->
      begin
	match hashtheory (theoryspec_theory dl) with
	| Some(dlh) ->
	    alpha = hashval_pub_addr dlh && tx_outputs_valid_addr_cats outpr
	| None -> false
      end
  | (alpha,(_,SignaPublication(beta,h,th,dl)))::outpr ->
      alpha = hashval_pub_addr (hashopair2 th (hashsigna (signaspec_signa dl))) && tx_outputs_valid_addr_cats outpr
  | (alpha,(_,DocPublication(beta,h,th,dl)))::outpr -> alpha = hashval_pub_addr (hashopair2 th (hashdoc dl)) && tx_outputs_valid_addr_cats outpr
  | (alpha,(_,Marker))::outpr -> pubaddr_p alpha && tx_outputs_valid_addr_cats outpr (*** markers should only be published to publication addresses, since they're used to prepublish an intention to publish ***)
  | _::outpr -> tx_outputs_valid_addr_cats outpr
  | [] -> true

let rec tx_outputs_valid_addr_cats_oc oc outpl =
  match outpl with
  | (alpha,(_,OwnsObj(h,beta,u)))::outpr ->
      if hashval_term_addr h = alpha then
	tx_outputs_valid_addr_cats_oc oc outpr
      else
	(Printf.fprintf oc "tx invalid since OwnsObj %s should be sent to %s\n" (hashval_hexstring h) (Cryptocurr.addr_daliladdrstr alpha); false)
  | (alpha,(_,OwnsProp(h,beta,u)))::outpr ->
      if hashval_term_addr h = alpha then
	tx_outputs_valid_addr_cats_oc oc outpr
      else
	(Printf.fprintf oc "tx invalid since OwnsProp %s should be sent to %s\n" (hashval_hexstring h) (Cryptocurr.addr_daliladdrstr alpha); false)
  | (alpha,(_,OwnsNegProp))::outpr ->
      if termaddr_p alpha then 
	tx_outputs_valid_addr_cats_oc oc outpr
      else
	(Printf.fprintf oc "tx invalid since OwnsNegProp should be sent to a term address\n"; false)
  | (alpha,(_,TheoryPublication(beta,h,dl)))::outpr ->
      begin
	match hashtheory (theoryspec_theory dl) with
	| Some(dlh) ->
	    if alpha = hashval_pub_addr dlh then
	      tx_outputs_valid_addr_cats_oc oc outpr
	    else
	      (Printf.fprintf oc "tx invalid since Theory should be sent to %s\n" (Cryptocurr.addr_daliladdrstr (hashval_pub_addr dlh)); false)
	| None -> false
      end
  | (alpha,(_,SignaPublication(beta,h,th,dl)))::outpr ->
      if alpha = hashval_pub_addr (hashopair2 th (hashsigna (signaspec_signa dl))) then
	tx_outputs_valid_addr_cats_oc oc outpr
      else
	(Printf.fprintf oc "tx invalid since Signature should be sent to %s\n" (Cryptocurr.addr_daliladdrstr (hashval_pub_addr (hashopair2 th (hashsigna (signaspec_signa dl))))); false)
  | (alpha,(_,DocPublication(beta,h,th,dl)))::outpr ->
      if alpha = hashval_pub_addr (hashopair2 th (hashdoc dl)) then
	tx_outputs_valid_addr_cats_oc oc outpr
      else
	(Printf.fprintf oc "tx invalid since Document should be sent to %s\n" (Cryptocurr.addr_daliladdrstr (hashval_pub_addr (hashopair2 th (hashdoc dl)))); false)
  | (alpha,(_,Marker))::outpr ->
      if pubaddr_p alpha then
	tx_outputs_valid_addr_cats outpr (*** markers should only be published to publication addresses, since they're used to prepublish an intention to publish ***)
      else
	(Printf.fprintf oc "tx invalid since Marker not sent to a publication address\n"; false)
  | _::outpr -> tx_outputs_valid_addr_cats_oc oc outpr
  | [] -> true

let tx_outputs_valid (outpl: addr_preasset list) =
  tx_outputs_valid_one_owner outpl [] [] []
    &&
  tx_outputs_valid_addr_cats outpl 

let tx_outputs_valid_oc oc (outpl: addr_preasset list) =
  tx_outputs_valid_one_owner_oc oc outpl [] [] []
    &&
  tx_outputs_valid_addr_cats_oc oc outpl 

let tx_valid tau = tx_inputs_valid (tx_inputs tau) && tx_outputs_valid (tx_outputs tau)

let tx_valid_oc oc tau = tx_inputs_valid_oc oc (tx_inputs tau) && tx_outputs_valid_oc oc (tx_outputs tau)

type gensignat_or_ref = GenSignatReal of gensignat | GenSignatRef of int
type txsigs = gensignat_or_ref option list * gensignat_or_ref option list
type stx = tx * txsigs

exception BadOrMissingSignature

let opmax b1 b2 =
  match (b1,b2) with
  | (Some(b1),Some(b2)) -> if b1 > b2 then Some(b1) else Some(b2)
  | (_,None) -> b1
  | _ -> b2

let check_spend_obligation_upto_blkh alpha (txhe:big_int) s obl =
  match obl with
  | None -> (*** defaults to alpha with no block height restriction ***)
      if verify_gensignat txhe s alpha then
	None
      else
	raise BadOrMissingSignature
  | Some(gamma,b,_) ->
      if verify_gensignat txhe s (Hash.payaddr_addr gamma) then
	Some(b)
      else
	raise BadOrMissingSignature

let check_spend_obligation alpha blkh (txhe:big_int) s obl =
  try
    match check_spend_obligation_upto_blkh alpha txhe s obl with
    | None -> true
    | Some(b) -> b <= blkh
  with BadOrMissingSignature -> false

let check_move_obligation alpha txhe s obl2 u2 outpl =
  try
    if not (payaddr_p alpha) then raise Not_found; (*** things held and termaddrs and pubaddrs should not be "moved" in this way ***)
    ignore (List.find (fun z -> let (beta,(obl,u)) = z in obl = obl2 && u = u2) outpl);
    verify_gensignat txhe s alpha
  with Not_found -> false

let getsig s rl =
  match s with
  | Some(GenSignatReal(s)) -> (s,s::rl)
  | Some(GenSignatRef(i)) -> (*** only allow up to 64K signatures on the list; should be much less than this in practice ***)
      if i < 65535 && i >= 0 then
	(List.nth rl i,rl)
      else
	raise BadOrMissingSignature
  | None ->
      raise BadOrMissingSignature

let marker_or_bounty_p a =
  match assetpre a with
  | Marker -> true
  | Bounty(_) -> true
  | _ -> false

let rec check_tx_in_signatures txhe outpl inpl al sl rl propowns =
  match inpl,al,sl with
  | [],[],[] -> None
  | (alpha,k)::inpr,(a::ar),sl when marker_or_bounty_p a -> (*** don't require signatures to spend markers and bounties; but there are conditions for the tx to be supported by a ctree ***)
      if assetid a = k then
	begin
	  match a with
	  | (_,_,Some(_,_,_),Bounty(_)) when not (List.mem alpha propowns) -> (*** if a is a Bounty and there is no corresponding ownership being spent, then require a signature (allow bounties to be collected according to the obligation after lockheight has passed) ***)
	      begin
		try
		  match sl with
		  | s::sr ->
		      let (s1,rl1) = getsig s rl in
		      let b = check_tx_in_signatures txhe outpl inpr ar sr rl1 propowns in
		      opmax b (check_spend_obligation_upto_blkh alpha txhe s1 (assetobl a))
		  | [] -> raise Not_found
		with Not_found -> raise BadOrMissingSignature
	      end		      
	  | _ ->
	      check_tx_in_signatures txhe outpl inpr ar sl rl propowns
	end
      else
	raise BadOrMissingSignature
  | (alpha,k)::inpr,(a::ar),(s::sr) ->
      begin
	try
	  let (s1,rl1) = getsig s rl in
	  if assetid a = k then
	    let b = check_tx_in_signatures txhe outpl inpr ar sr rl1 propowns in
	    begin
	      try
		opmax b (check_spend_obligation_upto_blkh alpha txhe s1 (assetobl a))
	      with BadOrMissingSignature ->
		if check_move_obligation alpha txhe s1 (assetobl a) (assetpre a) outpl then
		  b
		else
		  raise BadOrMissingSignature
	    end
	  else
	    raise BadOrMissingSignature
	with Not_found -> raise BadOrMissingSignature
      end
  | _,_,_ ->
      raise BadOrMissingSignature

let rec check_tx_out_signatures txhe outpl sl rl =
  match outpl,sl with
  | [],[] -> true
  | [],(_::_) -> false
  | (_,(_,TheoryPublication(alpha,n,thy)))::outpr,s::sr ->
      begin
	try
	  let (s1,rl1) = getsig s rl in
	  check_tx_out_signatures txhe outpr sr rl1
	    &&
	  verify_gensignat txhe s1 (payaddr_addr alpha)
	with Not_found -> false
      end
  | (_,(_,SignaPublication(alpha,n,th,si)))::outpr,s::sr ->
      begin
	try
	  let (s1,rl1) = getsig s rl in
	  check_tx_out_signatures txhe outpr sr rl1
	    &&
	  verify_gensignat txhe s1 (payaddr_addr alpha)
	with Not_found -> false
      end
  | (_,(_,DocPublication(alpha,n,th,d)))::outpr,s::sr ->
      begin
	try
	  let (s1,rl1) = getsig s rl in
	  check_tx_out_signatures txhe outpr sr rl1
	    &&
	  verify_gensignat txhe s1 (payaddr_addr alpha)
	with Not_found -> false
      end
  | _::outpr,_ ->
      check_tx_out_signatures txhe outpr sl rl

let tx_signatures_valid_asof_blkh al stau =
  let (tau,(sli,slo)) = stau in
  let txh = if !Config.testnet then hashtag (hashtx tau) 288l else hashtx tau in (*** sign a modified hash for testnet ***)
  let txhe = hashval_big_int txh in
  let rec get_propowns tauin al =
    match tauin,al with
    | ((alpha,aid1)::tauinr),((aid2,_,_,OwnsProp(_,_,_))::ar) when aid1 = aid2 -> alpha::get_propowns tauinr ar
    | ((alpha,aid1)::tauinr),((aid2,_,_,OwnsNegProp)::ar) when aid1 = aid2 -> alpha::get_propowns tauinr ar
    | ((_,aid1)::tauinr),((aid2,_,_,_)::ar) when aid1 = aid2 -> get_propowns tauinr ar
    | [],[] -> []
    | _,_ -> raise BadOrMissingSignature (*** actually this means the asset list does not match the inputs ***)
  in
  let b = check_tx_in_signatures txhe (tx_outputs tau) (tx_inputs tau) al sli [] (get_propowns (tx_inputs tau) al) in
  if check_tx_out_signatures txhe (tx_outputs tau) slo [] then
    b
  else
    raise BadOrMissingSignature

let tx_signatures_valid blkh al stau =
  try
    match tx_signatures_valid_asof_blkh al stau with
    | Some(b) -> b <= blkh
    | None -> true
  with BadOrMissingSignature -> false

let rec txout_update_ottree outpl tht =
  match outpl with
  | [] -> tht
  | (alpha,(obl,TheoryPublication(gamma,nonce,d)))::outpr ->
      let thy = theoryspec_theory d in
      begin
	match hashtheory thy with
	| Some(thyh) ->
	    txout_update_ottree outpr (Some(ottree_insert tht (hashval_bitseq thyh) thy))
	| _ -> txout_update_ottree outpr tht
      end
  | _::outpr -> txout_update_ottree outpr tht

let tx_update_ottree tau tht = txout_update_ottree (tx_outputs tau) tht

let rec txout_update_ostree outpl sigt =
  match outpl with
  | [] -> sigt
  | (alpha,(obl,SignaPublication(gamma,nonce,th,d)))::outpr ->
      let sg = signaspec_signa d in
      let thsgh = hashopair2 th (hashsigna sg) in
      txout_update_ostree outpr (Some(ostree_insert sigt (hashval_bitseq thsgh) sg))
  | _::outpr -> txout_update_ostree outpr sigt

let tx_update_ostree tau sigt = txout_update_ostree (tx_outputs tau) sigt

let seo_tx o g c = seo_prod (seo_list seo_addr_assetid) (seo_list seo_addr_preasset) o g c
let sei_tx i c = sei_prod (sei_list sei_addr_assetid) (sei_list sei_addr_preasset) i c

let seo_gensignat_or_ref o g c =
  match g with
  | GenSignatReal(s) ->
      let c = o 1 0 c in
      seo_gensignat o s c
  | GenSignatRef(i) ->
      let c = o 1 1 c in
      seo_varintb o i c

let sei_gensignat_or_ref i c =
  let (b,c) = i 1 c in
  if b = 0 then
    let (s,c) = sei_gensignat i c in
    (GenSignatReal(s),c)
  else
    let (j,c) = sei_varintb i c in
    (GenSignatRef(j),c)

let seo_txsigs o g c = seo_prod (seo_list (seo_option seo_gensignat_or_ref)) (seo_list (seo_option seo_gensignat_or_ref)) o g c
let sei_txsigs i c = sei_prod (sei_list (sei_option sei_gensignat_or_ref)) (sei_list (sei_option sei_gensignat_or_ref)) i c
let seo_stx o g c = seo_prod seo_tx seo_txsigs o g c
let sei_stx i c = sei_prod sei_tx sei_txsigs i c

let hashtxsigs g =
  let s = Buffer.create 1000 in
  seosbf (seo_txsigs seosb g (s,None));
  sha256str (Buffer.contents s)

(***
 The hash of signed tx does depend on the signatures, and this hash is used as the id of the tx
 (e.g., by Inv, STx and GetSTx network messages).
 But the assetid created by the tx only depends on the hashtx of tau (see add_vout in assets.ml).
 Since the assetid is what is referenced when spent by future txs, we have behavior like segwit.
***)
let hashstx (tau,tausigs) = hashpair (hashtx tau) (hashtxsigs tausigs)

module DbSTx = Dbbasic (struct type t = stx let basedir = "stx" let seival = sei_stx seic let seoval = seo_stx seoc end)

let json_addr_assetid (alpha,h) =
  JsonObj([("address",JsonStr(addr_daliladdrstr alpha));("assetid",JsonStr(hashval_hexstring h))])

let addr_assetid_from_json j =
  match j with
  | JsonObj(al) ->
      let alpha = addr_from_json(List.assoc "address" al) in
      let h = hashval_from_json(List.assoc "assetid" al) in
      (alpha,h)
  | _ -> raise (Failure("not a pair of an address with an asset id"))

let json_addr_preasset (alpha,(obl,u)) =
  match obl with
  | None ->
      JsonObj([("address",JsonStr(addr_daliladdrstr alpha));
	       ("preasset",json_preasset u)])
  | Some(gamma,lh,r) ->
      JsonObj([("address",JsonStr(addr_daliladdrstr alpha));
	       ("obligation",JsonObj([("lockaddress",JsonStr(addr_daliladdrstr (payaddr_addr gamma)));
				      ("lockheight",JsonNum(Int64.to_string lh));
				      ("reward",JsonBool(r))]));
	       ("preasset",json_preasset u)])

let rec json_txouts txh txouts i =
  match txouts with
  | [] -> []
  | (alpha,(obl,u))::txoutr ->
      let aid = hashpair txh (hashint32 (Int32.of_int i)) in
      let j =
	match json_obligation obl with
	| None ->
	    JsonObj([("address",JsonStr(addr_daliladdrstr alpha));
		     ("assetid",JsonStr(hashval_hexstring aid));
		     ("preasset",json_preasset u)])
	| Some(jobl) ->
	    JsonObj([("address",JsonStr(addr_daliladdrstr alpha));
		     ("assetid",JsonStr(hashval_hexstring aid));
		     ("obligation",jobl);
		     ("preasset",json_preasset u)])
      in
      j::json_txouts txh txoutr (i+1)

let json_tx (inpl,outpl) =
  JsonObj([("vin",JsonArr(List.map json_addr_assetid inpl));("vout",JsonArr(json_txouts (hashtx (inpl,outpl)) outpl 0))])

let tx_from_json j =
  match j with
  | JsonObj(al) ->
      begin
	match List.assoc "vin" al,List.assoc "vout" al with
	| JsonArr(jvinl),JsonArr(jvoutl) ->
	    (List.map addr_assetid_from_json jvinl,
	     List.map
	       (fun jvout ->
		 match jvout with
		 | JsonObj(bl) ->
		     begin
		       let alpha = addr_from_json (List.assoc "address" bl) in
		       let u = preasset_from_json (List.assoc "preasset" bl) in
		       let jobl =
			 try
			   let jobl = List.assoc "obligation" bl in
			   Some(jobl)
			 with Not_found -> None
		       in
		       let obl = obligation_from_json jobl in
		       (alpha,(obl,u))
		     end
		 | _ -> raise Not_found)
	       jvoutl)
	| _,_ -> raise (Failure("not a tx"))
      end
  | _ -> raise (Failure("not a tx"))
      
let json_gensignat_or_ref_option s =
  match s with
  | Some(GenSignatReal(gs)) -> json_gensignat gs
  | Some(GenSignatRef(n)) -> JsonNum(string_of_int n)
  | None -> JsonObj([])

let json_txsigs (insigs,outsigs) =
  JsonObj([("insigs",JsonArr(List.map json_gensignat_or_ref_option insigs));("outsigs",JsonArr(List.map json_gensignat_or_ref_option outsigs))])

let json_stx (tau,stau) = JsonObj([("type",JsonStr("stx"));("tx",json_tx tau);("txsigs",json_txsigs stau)])
