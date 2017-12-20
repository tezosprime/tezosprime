(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Big_int
open Config
open Hashaux
open Ser
open Sha256
open Hash
open Net
open Cryptocurr
open Signat
open Script
open Assets
open Tx
open Ctre
open Block
open Blocktree

let walletkeys = ref []
let walletp2shs = ref []
let walletendorsements = ref []
let walletwatchaddrs = ref []
let stakingassets = ref []
let storagetrmassets = ref []
let storagedocassets = ref []

let cants_balances_in_ledger : (hashval,int64 * int64 * int64 * int64) Hashtbl.t = Hashtbl.create 100

let unconfirmed_spent_assets : (hashval,hashval) Hashtbl.t = Hashtbl.create 100

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

let load_txpool () =
  let fn = Filename.concat (datadir()) "txpool" in
  if Sys.file_exists fn then
    let ch = open_in_bin fn in
    try
      while true do
	let ((txid,stau),_) = sei_prod sei_hashval Tx.sei_stx seic (ch,None) in
	add_to_txpool txid stau
      done
    with
    | End_of_file -> close_in ch
    | exc ->
	Printf.printf "Problem in txpool file: %s\n" (Printexc.to_string exc);
	close_in ch;;

let load_wallet () =
  let wallfn = Filename.concat (datadir()) "wallet" in
  if not (Sys.file_exists wallfn) then
    let s = open_out_bin wallfn in
    begin
      walletkeys := [];
      walletp2shs := [];
      walletendorsements := [];
      walletwatchaddrs := []
    end
  else
    let s = open_in_bin wallfn in
    try
      while true do
	let by = input_byte s in
	match by with
	| 0 ->
	    let ((k,b),_) = sei_prod sei_big_int_256 sei_bool seic (s,None) in
	    walletkeys :=
	      (match Secp256k1.smulp k Secp256k1._g with
	      | Some(x,y) ->
		  let h = pubkey_hashval (x,y) b in
		  let alpha1 = hashval_md160 h in
		  let alpha = addr_daliladdrstr (p2pkhaddr_addr alpha1) in
		  (k,b,(x,y),dalilwif k b,alpha1,alpha)
	      | None ->
		  raise (Failure "A private key in the wallet did not give a public key.")
	      )::!walletkeys
	| 1 ->
	    let (scr,_) = sei_list sei_int8 seic (s,None) in
	    walletp2shs :=
	      (let h = hash160_bytelist scr in
	      let a = addr_daliladdrstr (p2shaddr_addr h) in
	      (h,a,scr))::!walletp2shs
	| 2 ->
	    let (endors,_) = sei_prod6 sei_payaddr sei_payaddr (sei_prod sei_big_int_256 sei_big_int_256) sei_varintb sei_bool sei_signat seic (s,None) in (*** For each (alpha,beta,esg) beta can use esg to justify signing for alpha; endorsements can be used for spending/moving, but not for staking. ***)
	    walletendorsements := endors::!walletendorsements
	| 3 ->
	    let (watchaddr,_) = sei_addr seic (s,None) in
	    walletwatchaddrs := watchaddr::!walletwatchaddrs
	| _ ->
	    raise (Failure "Bad entry in wallet file")
      done
    with
    | End_of_file -> close_in s
    | Failure(x) ->
	Printf.printf "Warning: %s\nIgnoring the rest of the wallet file.\n" x; flush stdout;
	close_in s

let save_wallet () =
  let wallfn = Filename.concat (datadir()) "wallet" in
  let s = open_out_bin wallfn in
  List.iter
    (fun (k,b,_,_,_,_) ->
      output_byte s 0;
      seocf (seo_prod seo_big_int_256 seo_bool seoc (k,b) (s,None)))
    !walletkeys;
  List.iter
    (fun (_,_,scr) ->
      output_byte s 1;
      seocf (seo_list seo_int8 seoc scr (s,None)))
    !walletp2shs;
  List.iter
    (fun endors ->
      output_byte s 2;
      seocf (seo_prod6 seo_payaddr seo_payaddr (seo_prod seo_big_int_256 seo_big_int_256) seo_varintb seo_bool seo_signat seoc endors (s,None)))
    !walletendorsements;
  List.iter
    (fun watchaddr ->
      output_byte s 3;
      seocf (seo_addr seoc watchaddr (s,None)))
    !walletwatchaddrs;
  close_out s

let append_wallet f =
  let wallfn = Filename.concat (datadir()) "wallet" in
  let s = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 wallfn in
  f s;
  close_out s

let addnode remip remport =
  false
(***
let addnode remip remport =
  match !socks with
  | None -> (*** no proxy ***)
      begin
	try
	  let (r,ri,ro) = connectpeer remip remport in
	  true
	with _ ->
	  false
      end
  | Some(s) when s = 4 -> (*** socks4 ***)
      begin
	try
	  let (r,ri,ro) = connectpeer_socks4 !socksport remip remport in
	  true
	with _ -> false
      end
  | Some(s) when s = 5 -> (*** socks5, not yet implemented ***)
      false
  | _ -> (*** unknown ***)
      false
***)

let privkey_in_wallet_p alpha =
  let (p,x4,x3,x2,x1,x0) = alpha in
  if p = 0 then
    begin
      try
	ignore (List.find (fun (_,_,_,_,h,_) -> h = (x4,x3,x2,x1,x0)) !walletkeys);
	true
      with Not_found -> false
    end
  else
    false

let endorsement_in_wallet_p alpha =
  let (p,x4,x3,x2,x1,x0) = alpha in
  if p = 0 || p = 1 then
    let b = (p = 1) in
    begin
      try
	ignore (List.find (fun (beta,_,_,_,_,_) -> beta = (b,x4,x3,x2,x1,x0)) !walletendorsements);
	true
      with Not_found -> false
    end
  else
    false

let endorsement_in_wallet_2_p alpha beta =
  let (p,x4,x3,x2,x1,x0) = alpha in
  let (q,y4,y3,y2,y1,y0) = beta in
  if (p = 0 || p = 1) && (q = 0 || q = 1) then
    let b = (p = 1) in
    let c = (q = 1) in
    begin
      try
	ignore (List.find (fun (alpha2,beta2,_,_,_,_) -> alpha2 = (b,x4,x3,x2,x1,x0) && beta2 = (c,y4,y3,y2,y1,y0)) !walletendorsements);
	true
      with Not_found -> false
    end
  else
    false

let watchaddr_in_wallet_p alpha =
  List.mem alpha !walletwatchaddrs

let hexchar_invi x =
  match x with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'A' -> 10
  | 'B' -> 11
  | 'C' -> 12
  | 'D' -> 13
  | 'E' -> 14
  | 'F' -> 15
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | _ -> raise (Failure("not a hex: " ^ (string_of_int (Char.code x))))

let hexsubstring_int8 h i =
  (hexchar_invi h.[i]) lsl 4 + (hexchar_invi h.[i+1])

let bytelist_of_hexstring h =
  let l = ref (String.length h) in
  let bl = ref [] in
  l := !l-2;
  while (!l > 0) do
    bl := hexsubstring_int8 h !l::!bl;
    l := !l-2
  done;
  !bl

let btctodaliladdr a =
  let alpha = btcaddrstr_addr a in
  let a2 = addr_daliladdrstr alpha in
  Printf.printf "Dalilcoin address %s corresponds to Bitcoin address %s\n" a2 a

let importprivkey_real (k,b) =
  match Secp256k1.smulp k Secp256k1._g with
  | Some(x,y) ->
      let h = hashval_md160 (pubkey_hashval (x,y) b) in
      let alpha = p2pkhaddr_addr h in
      let a = addr_daliladdrstr alpha in
      let replwall = ref false in
      if privkey_in_wallet_p alpha then raise (Failure "Private key already in wallet.");
      walletkeys := (k,b,(x,y),dalilwif k b,h,a)::!walletkeys;
      walletendorsements := (*** remove endorsements if the wallet has the private key for the address, since it can now sign directly ***)
	List.filter
	  (fun (alpha2,beta,(x,y),recid,fcomp,esg) -> if alpha = payaddr_addr alpha2 then (replwall := true; false) else true)
	  !walletendorsements;
      walletwatchaddrs :=
	List.filter
	  (fun alpha2 -> if alpha = alpha2 then (replwall := true; false) else true)
	  !walletwatchaddrs;
      if !replwall then
	save_wallet()
      else
	append_wallet (*** this doesn't work. find out why ***)
	  (fun s ->
	    output_byte s 0;
	    seocf (seo_prod seo_big_int_256 seo_bool seoc (k,b) (s,None)));
      Printf.printf "Imported key for address %s\n" a;
      flush stdout
  | None ->
      raise (Failure "This private key does not give a public key.")

let importprivkey w =
  let (k,b) = privkey_from_wif w in
  let w2 = dalilwif k b in
  if not (w2 = w) then raise (Failure (w ^ " is not a valid Dalilcoin wif"));
  importprivkey_real (k,b)

let importbtcprivkey w =
  let (k,b) = privkey_from_btcwif w in
  importprivkey_real (k,b)

let importendorsement a b s =
  let alpha = daliladdrstr_addr a in
  let beta = daliladdrstr_addr b in
  if endorsement_in_wallet_2_p alpha beta then raise (Failure ("An endorsement from " ^ a ^ " to " ^ b ^ " is already in the wallet."));
  let (q,y4,y3,y2,y1,y0) = beta in
  if q = 0 && not (privkey_in_wallet_p beta) then raise (Failure ("The private key for " ^ b ^ " must be in the wallet before an endorsement to it can be added."));
  let betap = (q=1,y4,y3,y2,y1,y0) in
  let (recid,fcomp,esg) = decode_signature s in
  let (p,x4,x3,x2,x1,x0) = alpha in
  if p = 0 then
    begin
      let alphap = (false,x4,x3,x2,x1,x0) in
      if privkey_in_wallet_p alpha then raise (Failure "Not adding endorsement since the wallet already has the private key for this address.");
      match verifybitcoinmessage_recover (x4,x3,x2,x1,x0) recid fcomp esg ("endorse " ^ b) with
      | None ->
	  if !Config.testnet then
	    begin
	      match verifybitcoinmessage_recover (-916116462l, -1122756662l, 602820575l, 669938289l, 1956032577l) recid fcomp esg ("fakeendorsement " ^ b ^ " (" ^ (addr_daliladdrstr alpha) ^ ")") with
	      | None ->
		  raise (Failure "endorsement signature verification failed; not adding endorsement to wallet")
	      | Some(x,y) ->
		  Printf.printf "Fake endorsement acceptable for testnet; adding to wallet.\n";
		  walletendorsements := (alphap,betap,(x,y),recid,fcomp,esg)::!walletendorsements;
		  save_wallet() (*** overkill, should append if possible ***)
	    end
	  else
	    raise (Failure "endorsement signature verification failed; not adding endorsement to wallet")
      | Some(x,y) ->
(*	  Printf.printf "just verified endorsement signature:\naddrhex = %s\nrecid = %d\nfcomp = %s\nesgr = %s\nesgs = %s\nendorse %s\n" (hashval_hexstring (x4,x3,x2,x1,x0)) recid (if fcomp then "true" else "false") (let (r,s) = esg in string_of_big_int r) (let (r,s) = esg in string_of_big_int s) b; flush stdout; *)
	  Printf.printf "Verified endorsement; adding to wallet.\n";
	  walletendorsements := (alphap,betap,(x,y),recid,fcomp,esg)::!walletendorsements;
	  save_wallet() (*** overkill, should append if possible ***)
    end
  else if p = 1 then (*** endorsement by a p2sh address, endorsement can only be checked if the script for alpha is known, so it should have been imported earlier ***)
    begin
      raise (Failure "Code for importing endorsements by a p2sh addresses has not yet been written.")
    end
  else
    raise (Failure (a ^ " expected to be a p2pkh or p2sh Dalilcoin address."))

let importwatchaddr a =
  let alpha = daliladdrstr_addr a in
  let a2 = addr_daliladdrstr alpha in
  if not (a2 = a) then raise (Failure (a ^ " is not a valid Dalilcoin address"));
  if privkey_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has the private key for this address.");
  if endorsement_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has an endorsement for this address.");
  if watchaddr_in_wallet_p alpha then raise (Failure "Watch address is already in wallet.");
  walletwatchaddrs := alpha::!walletwatchaddrs;
  save_wallet() (*** overkill, should append if possible ***)

let importwatchbtcaddr a =
  let alpha = btcaddrstr_addr a in
  let a2 = addr_daliladdrstr alpha in
  Printf.printf "Importing as Dalilcoin address %s\n" a2;
  if privkey_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has the private key for this address.");
  if endorsement_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has an endorsement for this address.");
  if watchaddr_in_wallet_p alpha then raise (Failure "Watch address is already in wallet.");
  walletwatchaddrs := alpha::!walletwatchaddrs;
  save_wallet() (*** overkill, should append if possible ***)

let assets_at_address_in_ledger_json alpha ledgerroot blkh =
  let alphas = addr_daliladdrstr alpha in
  let ctr = Ctre.CHash(ledgerroot) in
  let warned = ref false in
  let jwl = ref [] in
  let jal = ref [] in
  let alpha_hl = ref (None,-1) in
  let tot = ref 0L in
  let handler f =
    try
      f()
    with
    | Not_found ->
	jwl := JsonObj([("warning",JsonStr("The complete ledger is not in the local database; some assets in the ledger might not be displayed."))])::!jwl;
	warned := true
    | e ->
      jwl := JsonObj([("warning",JsonStr(Printexc.to_string e))])::!jwl;
      warned := true
  in
  handler (fun () -> alpha_hl := Ctre.ctree_addr true false alpha ctr None);
  let sumcurr tot a =
    match a with
    | (_,_,_,Currency(v)) -> tot := Int64.add !tot v
    | _ -> ()
  in
  begin
    match !alpha_hl with
    | (Some(hl),_) ->
	let s = Buffer.create 100 in
	Ctre.print_hlist_to_buffer_gen s blkh (Ctre.nehlist_hlist hl) (sumcurr tot);
	jal := [("address",JsonStr(alphas));("total",JsonNum(fraenks_of_cants !tot));("contents",JsonStr(Buffer.contents s))]
    | (None,z) ->
	if z < 0 then
	  begin
	    jwl := JsonObj([("warning",JsonStr("Problem obtaining contents of address."))])::!jwl;
	    jal := [("address",JsonStr(alphas))]
	  end
	else
	  jal := [("address",JsonStr(alphas));("contents",JsonStr("empty"))]
    | _ ->
	jal := [("address",JsonStr(alphas));("contents",JsonStr("no information"))]
  end;
  (!jal,!jwl)

let printassets_in_ledger oc ledgerroot =
  let ctr = Ctre.CHash(ledgerroot) in
  let warned = ref false in
  let waitprinted = ref false in
  let al1 = ref [] in
  let tot1 = ref 0L in
  let al2 = ref [] in
  let tot2 = ref 0L in
  let al3 = ref [] in
  let tot3 = ref 0L in
  let al4 = ref [] in
  let tot4 = ref 0L in
  let numtrys = ref 11 in
  let handler f =
    try
      if !numtrys > 1 then decr numtrys;
      for i = 1 to !numtrys do
	try
	  f();
	  raise Exit
	with GettingRemoteData ->
	  if !netconns = [] then
	    begin (** ignore if there are no connections **)
	      if not !warned then
		begin
		  Printf.fprintf oc "Warning: The complete ledger is not in the local database and there are no connections to request missing data.\n";
		  Printf.fprintf oc "Some assets in the ledger might not be displayed.\n";
		  warned := true
		end;
	      raise Exit
	    end
	  else
	    begin
	      if not !waitprinted then (Printf.fprintf oc "Some data is being requested from remote nodes...please wait a minute or two...\n"; flush oc; waitprinted := true);
              Thread.delay 2.0
	    end
      done;
      if not !warned then
	begin
	  Printf.fprintf oc "Warning: The complete ledger is not in the local database.\n";
	  Printf.fprintf oc "Remote data is being requested, but is taking too long.\n";
	  Printf.fprintf oc "Some assets in the ledger might not be displayed.\n";
	  warned := true
	end
    with Exit -> ()
  in
  List.iter
    (fun (k,b,(x,y),w,h,z) ->
      handler (fun () -> al1 := (z,Ctre.ctree_addr true true (p2pkhaddr_addr h) ctr None)::!al1))
    !walletkeys;
  List.iter
    (fun (h,z,scr) ->
      handler (fun () -> al2 := (z,Ctre.ctree_addr true true (p2shaddr_addr h) ctr None)::!al2))
    !walletp2shs;
  List.iter
    (fun (alpha,beta,(x,y),recid,fcomp,esg) -> 
      let alpha2 = payaddr_addr alpha in
      handler (fun () -> al3 := (alpha2,Ctre.ctree_addr true true alpha2 ctr None)::!al3))
    !walletendorsements;
  List.iter
    (fun alpha ->
      handler (fun () -> al4 := (alpha,Ctre.ctree_addr true true alpha ctr None)::!al4))
    !walletwatchaddrs;
  let sumcurr tot a =
    match a with
    | (_,_,_,Currency(v)) -> tot := Int64.add !tot v
    | _ -> ()
  in
  Printf.fprintf oc "Assets in ledger with root %s:\n" (hashval_hexstring ledgerroot);
  Printf.fprintf oc "Controlled p2pkh assets:\n";
  List.iter
    (fun (z,x) ->
      match x with
      | (Some(hl),_) ->
	  Printf.fprintf oc "%s:\n" z;
	  Ctre.print_hlist_gen oc (Ctre.nehlist_hlist hl) (sumcurr tot1)
      | (None,_) ->
	  Printf.fprintf oc "%s: empty\n" z;
      | _ ->
	  Printf.fprintf oc "%s: no information\n" z;
    )
    !al1;
  Printf.fprintf oc "Possibly controlled p2sh assets:\n";
  List.iter
    (fun (z,x) ->
      match x with
      | (Some(hl),_) ->
	  Printf.fprintf oc "%s:\n" z;
	  Ctre.print_hlist_gen oc (Ctre.nehlist_hlist hl) (sumcurr tot2)
      | (None,_) ->
	  Printf.fprintf oc "%s: empty\n" z;
      | _ ->
	  Printf.fprintf oc "%s: no information\n" z;
    )
    !al2;
  Printf.fprintf oc "Assets via endorsement:\n";
  List.iter
    (fun (alpha2,x) ->
      match x with
      | (Some(hl),_) ->
	  Printf.fprintf oc "%s:\n" (addr_daliladdrstr alpha2);
	  Ctre.print_hlist_gen oc (Ctre.nehlist_hlist hl) (sumcurr tot3)
      | (None,_) ->
	  Printf.fprintf oc "%s: empty\n" (addr_daliladdrstr alpha2);
      | _ ->
	  Printf.fprintf oc "%s: no information\n" (addr_daliladdrstr alpha2);
    )
    !al3;
  Printf.fprintf oc "Watched assets:\n";
  List.iter
    (fun (alpha,x) ->
      match x with
      | (Some(hl),_) ->
	  Printf.fprintf oc "%s:\n" (addr_daliladdrstr alpha);
	  Ctre.print_hlist_gen oc (Ctre.nehlist_hlist hl) (sumcurr tot4)
      | (None,_) ->
	  Printf.fprintf oc "%s: empty\n" (addr_daliladdrstr alpha);
      | _ ->
	  Printf.fprintf oc "%s: no information\n" (addr_daliladdrstr alpha);
    )
    !al4;
  Printf.fprintf oc "Total p2pkh: %s fraenks\n" (fraenks_of_cants !tot1);
  Printf.fprintf oc "Total p2sh: %s fraenks\n" (fraenks_of_cants !tot2);
  Printf.fprintf oc "Total via endorsement: %s fraenks\n" (fraenks_of_cants !tot3);
  Printf.fprintf oc "Total watched: %s fraenks\n" (fraenks_of_cants !tot4);
  Hashtbl.replace cants_balances_in_ledger ledgerroot (!tot1,!tot2,!tot3,!tot4) (*** preventing recomputation for getting balances if the ledger has not changed ***)

let printassets oc =
  let (bn,cwl) = get_bestnode true in
  let BlocktreeNode(_,_,_,_,_,ledgerroot,_,_,_,_,_,_,_,_) = bn in
  printassets_in_ledger oc ledgerroot

let get_cants_balances_in_ledger oc ledgerroot =
  try
    Hashtbl.find cants_balances_in_ledger ledgerroot
  with Not_found ->
    let ctr = Ctre.CHash(ledgerroot) in
    let warned = ref false in
    let waitprinted = ref false in
    let tot1 = ref 0L in
    let tot2 = ref 0L in
    let tot3 = ref 0L in
    let tot4 = ref 0L in
    let numtrys = ref 11 in
    let handler f =
      try
	if !numtrys > 1 then decr numtrys;
	for i = 1 to !numtrys do
	  try
	    f();
	    raise Exit
	  with GettingRemoteData ->
	    if !netconns = [] then
	      begin (** ignore if there are no connections **)
		if not !warned then
		  begin
		    Printf.fprintf oc "Warning: The complete ledger is not in the local database and there are no connections to request missing data.\n";
		    Printf.fprintf oc "Some displayed balances may be too small.\n";
		    warned := true
		  end;
		raise Exit
	      end
	    else
	      begin
		if not !waitprinted then (Printf.fprintf oc "Some data is being requested from remote nodes...please wait a minute or two...\n"; flush oc; waitprinted := true);
                Thread.delay 2.0
	      end
	done;
	if not !warned then
	  begin
	    Printf.fprintf oc "Warning: The complete ledger is not in the local database.\n";
	    Printf.fprintf oc "Remote data is being requested, but is taking too long.\n";
	    Printf.fprintf oc "Some assets in the ledger might not be displayed.\n";
	    warned := true
	  end
      with Exit -> ()
    in
    let asset_sumcurr tot a =
      match a with
      | (_,_,_,Currency(v)) -> tot := Int64.add !tot v
      | _ -> ()
    in
    let rec hlist_sumcurr tot (hl:hlist) =
      match hl with
      | HNil -> ()
      | HConsH(ah,hr) -> hlist_sumcurr tot hr; asset_sumcurr tot (get_asset ah)
      | HCons(a,hr) -> hlist_sumcurr tot hr; asset_sumcurr tot a
      | HHash(hh) -> hlist_sumcurr tot (get_hlist_element hh)
    in
    let rec nehlist_sumcurr tot (hl:nehlist) =
      match hl with
      | NehConsH(ah,hr) -> hlist_sumcurr tot hr; asset_sumcurr tot (get_asset ah)
      | NehCons(a,hr) -> hlist_sumcurr tot hr; asset_sumcurr tot a
      | NehHash(hh) -> nehlist_sumcurr tot (get_nehlist_element hh)
    in
    List.iter
      (fun (k,b,(x,y),w,h,z) ->
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true (p2pkhaddr_addr h) ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot1 hl
	    | _ -> ()))
      !walletkeys;
    List.iter
      (fun (h,z,scr) ->
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true (p2shaddr_addr h) ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot2 hl
	    | _ -> ()))
      !walletp2shs;
    List.iter
      (fun (alpha,beta,(x,y),recid,fcomp,esg) -> 
	let alpha2 = payaddr_addr alpha in
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true alpha2 ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot3 hl
	    | _ -> ()))
      !walletendorsements;
    List.iter
      (fun alpha ->
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true alpha ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot4 hl
	    | _ -> ()))
      !walletwatchaddrs;
    Hashtbl.add cants_balances_in_ledger ledgerroot (!tot1,!tot2,!tot3,!tot4);
    (!tot1,!tot2,!tot3,!tot4)

let printasset h =
  try
    let (aid,bday,obl,u) = DbAsset.dbget h in
    Printf.printf "%s: %s [%Ld] %s %s\n" (hashval_hexstring h) (hashval_hexstring aid) bday (preasset_string u) (obligation_string obl)
  with Not_found ->
    Printf.printf "No asset with hash %s found. (Did you give the asset id instead of the asset hash?)\n" (hashval_hexstring h)

let printhconselt h =
  try
    let (aid,k) = DbHConsElt.dbget h in
    Printf.printf "assetid %s\n" (hashval_hexstring aid);
    match k with
    | Some(k) -> Printf.printf "next hcons elt %s\n" (hashval_hexstring k)
    | None -> Printf.printf "last on the list\n"
  with Not_found ->
    Printf.printf "No hcons elt %s found\n" (hashval_hexstring h)

let printctreeelt h =
  try
    let c = DbCTreeElt.dbget h in
    print_ctree c
  with Not_found ->
    Printf.printf "No ctree elt %s found\n" (hashval_hexstring h)

let printctreeinfo h =
  try
    let c = DbCTreeElt.dbget h in
    let n = ref 0 in
    let v = ref 0L in
    let b = ref 0L in
    let e = ref 1 in
    let l = ref 0 in
    let a = ref 0 in
    let own = ref 0 in
    let rght = ref 0 in
    let mrk = ref 0 in
    let pub = ref 0 in
    let ah = ref 0 in
    let hh = ref 0 in
    let ch = ref 0 in
    let rec hconseltinfo (aid,k) =
      try
	let (_,_,_,preast) = DbAsset.dbget aid in
	incr a;
	match preast with
	| Currency(u) -> v := Int64.add u !v
	| Bounty(u) -> b := Int64.add u !b
	| OwnsObj(_,_,_) -> incr own
	| OwnsProp(_,_,_) -> incr own
	| OwnsNegProp -> incr own
	| RightsObj(_,_) -> incr rght
	| RightsProp(_,_) -> incr rght
	| Marker -> incr mrk
	| _ -> incr pub
      with Not_found ->
	incr ah;
	match k with
	| None -> ()
	| Some(k) ->
	    try
	      hconseltinfo (DbHConsElt.dbget k)
	    with Not_found ->
	      incr hh
    in
    let rec ctreeeltinfo c =
      match c with
      | CHash(h) ->
	  begin
	    try
	      incr e;
	      ctreeeltinfo (DbCTreeElt.dbget h)
	    with Not_found -> incr ch
	  end
      | CLeaf(_,NehHash(h)) ->
	  begin
	    try
	      incr l;
	      hconseltinfo (DbHConsElt.dbget h)
	    with Not_found -> incr hh
	  end
      | CLeaf(_,_) -> raise (Failure "ctree was not an element")
      | CLeft(c0) -> ctreeeltinfo c0
      | CRight(c1) -> ctreeeltinfo c1
      | CBin(c0,c1) -> ctreeeltinfo c0; ctreeeltinfo c1
    in
    ctreeeltinfo c;
    Printf.printf "Number of abstract unknown ctrees %d\n" !ch;
    Printf.printf "Number of abstract unknown hcons elts %d\n" !hh;
    Printf.printf "Number of abstract unknown assets %d\n" !ah;
    Printf.printf "Number of known ctree elts %d\n" !e;
    Printf.printf "Number of known leaves %d\n" !l;
    Printf.printf "Number of known assets %d\n" !a;
    Printf.printf "Number of ownership assets %d\n" !own;
    Printf.printf "Number of rights assets %d\n" !rght;
    Printf.printf "Number of marker assets %d\n" !mrk;
    Printf.printf "Number of publication assets %d\n" !pub;
    Printf.printf "Total cants in known currency assets %Ld\n" !v;
    Printf.printf "Total cants in known bounty assets %Ld\n" !b;
  with Not_found ->
    Printf.printf "No ctree %s found\n" (hashval_hexstring h)
  
let printtx_a (tauin,tauout) =
  let i = ref 0 in
  Printf.printf "Inputs (%d):\n" (List.length tauin);
  List.iter
    (fun (alpha,aid) ->
      Printf.printf "Input %d:%s %s\n" !i (addr_daliladdrstr alpha) (hashval_hexstring aid);
      incr i)
    tauin;      
  i := 0;
  Printf.printf "Outputs (%d):\n" (List.length tauout);
  List.iter
    (fun (alpha,(obl,u)) ->
      Printf.printf "Output %d:%s %s %s\n" !i (addr_daliladdrstr alpha) (preasset_string u) (obligation_string obl);
      incr i)
    tauout

let printtx txid =
  try
    let (tau,_) = Hashtbl.find stxpool txid in
    Printf.printf "Tx %s in pool.\n" (hashval_hexstring txid);
    printtx_a tau
  with Not_found ->
    try
      let tau = DbTx.dbget txid in
      Printf.printf "Tx %s in local database.\n" (hashval_hexstring txid);
      printtx_a tau
    with Not_found ->
      Printf.printf "Unknown tx %s.\n" (hashval_hexstring txid)

let createtx inpj outpj =
  match (inpj,outpj) with
  | (JsonArr(inpl),JsonArr(outpl)) ->
      begin
	let tauinl =
	  List.map
	    (fun inp ->
	      match inp with
	      | JsonObj([(alpha,JsonStr(aidhex))]) ->
		  (daliladdrstr_addr alpha,hexstring_hashval aidhex)
	      | _ -> raise Exit)
	    inpl
	in
	  let tauoutl =
	    List.map
	      (fun outp ->
		match outp with
		| JsonObj(al) ->
		    begin
		      try
			let betaj = List.assoc "addr" al in
			let valj = List.assoc "val" al in
			match (betaj,valj) with
			| (JsonStr(beta),JsonNum(x)) ->
			    begin
			      let beta2 = daliladdrstr_addr beta in
			      let v = cants_of_fraenks x in
			      try
				let lockj = List.assoc "lock" al in
				match lockj with
				| JsonNum(lockstr) ->
				    let lock = Int64.of_string lockstr in
				    begin
				      try
					let obladdrj = List.assoc "obligationaddr" al in
					match obladdrj with
					| JsonStr(obladdr) ->
					    let gamma2 = daliladdrstr_addr obladdr in
					    if not (payaddr_p gamma2) then raise (Failure (Printf.sprintf "obligation address %s must be a payaddr (p2pkh or p2sh)" obladdr));
					    let (i,c4,c3,c2,c1,c0) = gamma2 in
					    let gamma_as_payaddr = (i=1,c4,c3,c2,c1,c0) in
					    let obl = Some(gamma_as_payaddr,lock,false) in
					    (beta2,(obl,Currency(v)))
					| _ -> raise Exit
				      with Not_found ->
					if not (payaddr_p beta2) then raise (Failure (Printf.sprintf "since output will be locked, receiving address %s must be a payaddr (p2pkh or p2sh) or a payaddr as obligation address must be given" beta));
					let (i,b4,b3,b2,b1,b0) = beta2 in
					let beta_as_payaddr = (i=1,b4,b3,b2,b1,b0) in
					let obl = Some(beta_as_payaddr,lock,false) in
					(beta2,(obl,Currency(v)))
				    end
				| _ -> raise Exit
			      with Not_found ->
				(beta2,(None,Currency(v)))
			    end
			| _ -> raise Exit
		      with Not_found -> raise Exit
		    end
		| _ -> raise Exit)
	      outpl
	  in
	  (tauinl,tauoutl)
      end
  | _ -> raise Exit

let createsplitlocktx ledgerroot alpha beta gamma aid i lkh fee =
  if i <= 0 then raise (Failure ("Cannot split into " ^ (string_of_int i) ^ " assets"));
  let alpha2 = payaddr_addr alpha in
  let ctr = Ctre.CHash(ledgerroot) in
  match ctree_lookup_asset true false aid ctr (addr_bitseq alpha2) with
  | None -> Printf.printf "Could not find asset %s at %s\n" (hashval_hexstring aid) (addr_daliladdrstr alpha2); flush stdout
  | Some(_,bday,obl,Currency(v)) ->
      if v > fee then
	begin
	  let rem = ref (Int64.sub v fee) in
	  let u = Int64.div !rem (Int64.of_int i) in
	  if u > 0L then
	    begin
	      let outl = ref [] in
	      for j = 0 to i-2 do
		outl := (gamma,(Some(beta,lkh,false),Currency(u)))::!outl;
		rem := Int64.sub !rem u
	      done;
	      outl := (gamma,(Some(beta,lkh,false),Currency(!rem)))::!outl;
	      let tau : tx = ([(alpha2,aid)],!outl) in
	      printtx_a tau;
	      let s = Buffer.create 100 in
	      seosbf (seo_stx seosb (tau,([],[])) (s,None));
	      let hs = string_hexstring (Buffer.contents s) in
	      Printf.printf "%s\n" hs
	    end
	  else
	    begin
	      Printf.printf "Asset %s is %s fraenks, which is smaller than %d cants after subtracting the fee of %s\n" (hashval_hexstring aid) (fraenks_of_cants v) i (fraenks_of_cants v); flush stdout
	    end	  
	end
      else
	begin
	  Printf.printf "Asset %s is %s fraenks, which is not greater the fee of %s\n" (hashval_hexstring aid) (fraenks_of_cants v) (fraenks_of_cants v); flush stdout
	end
  | _ -> Printf.printf "Asset %s is not currency.\n" (hashval_hexstring aid); flush stdout

(*** first see if private key for beta is in the wallet; if not check if an endorsement is in the wallet; if not fail ***)
let signtx_p2pkh beta taue =
  try
    let (k,b,(x,y),w,h,z) = List.find (fun (_,_,_,_,h,_) -> h = beta) !walletkeys in
    let r = rand_256() in
    P2pkhSignat(Some(x,y),b,signat_big_int taue k r)
  with Not_found ->
    let (alpha,gamma,(x,y),recid,fcomp,esg) =
      List.find 
	(fun (alpha,gam,_,_,_,_) ->
	  let (p,a4,a3,a2,a1,a0) = alpha in
	  not p && (a4,a3,a2,a1,a0) = beta)
	!walletendorsements
    in
    let (p,c4,c3,c2,c1,c0) = gamma in
    if p then
      raise (Failure "p2psh signing not yet supported")
    else
      let (k,b2,(x2,y2),w,h,z) = List.find (fun (_,_,_,_,h,_) -> h = (c4,c3,c2,c1,c0)) !walletkeys in
      let r = rand_256() in
      let s1 = signat_big_int taue k r in
      let s = EndP2pkhToP2pkhSignat(Some(x,y),fcomp,Some(x2,y2),b2,esg,s1) in
      s

let getsig s rl =
  match s with
  | GenSignatReal(s) -> (s,fun gam -> (gam,Some(s))::rl)
  | GenSignatRef(i) -> (*** only allow up to 64K signatures on the list; should be much less than this in practice ***)
      if i < 65535 && i >= 0 then
	match List.nth rl i with
	| (gam,Some(s)) -> (s,fun _ -> rl)
	| (gam,None) -> raise BadOrMissingSignature
      else
	raise BadOrMissingSignature

let rec assoc_pos b l p =
  match l with
  | ((x,v)::r) when x = b -> (v,p)
  | (_::r) -> assoc_pos b r (p+1)
  | [] -> raise Not_found

let rec signtx_ins taue inpl al outpl sl rl (rsl:gensignat_or_ref option list) ci =
  match inpl,al with
  | (alpha,k)::inpr,(a::ar) ->
      begin
	if not (assetid a = k) then raise (Failure "Asset mismatch when trying to sign inputs");
	match assetpre a with
	| Marker -> signtx_ins taue inpr ar outpl sl rl rsl ci
	| Bounty(_) -> signtx_ins taue inpr ar outpl sl rl rsl ci
	| _ ->
	    let obl = assetobl a in
	    match sl with
	    | [] -> signtx_ins taue inpl al outpl [None] rl rsl ci
	    | (None::sr) -> (*** missing signature ***)
		begin
		  (*** check if one of the existing signatures can be used for this ***)
		  try
		    let beta =
		      match obl with
		      | None ->
			  let (p,a4,a3,a2,a1,a0) = alpha in
			  (p=1,a4,a3,a2,a1,a0)
		      | Some(beta,_,_) -> beta
		    in
		    match assoc_pos beta rl 0 with
		    | (Some(s),p) ->
			signtx_ins taue inpr ar outpl sr rl (Some(GenSignatRef(p))::rsl) ci
		    | (None,p) -> raise Not_found
		  with Not_found ->
		    (*** otherwise, try to sign for this input ***)
		    match obl with
		    | Some(beta,lkh,r) ->
			let (p,b4,b3,b2,b1,b0) = beta in
			if p then
			  raise (Failure "p2sh signing is not yet supported")		  
			else
			  begin
			    try
			      let s = signtx_p2pkh (b4,b3,b2,b1,b0) taue in
			      signtx_ins taue inpr ar outpl sr ((beta,Some(s))::rl) (Some(GenSignatReal(s))::rsl) ci
			    with _ ->
			      signtx_ins taue inpr ar outpl sr ((beta,None)::rl) (None::rsl) false
			  end
		  | None ->
		      if p2pkhaddr_p alpha then
			let (_,a4,a3,a2,a1,a0) = alpha in
			begin
			  try
			    let s = signtx_p2pkh (a4,a3,a2,a1,a0) taue in
			    signtx_ins taue inpr ar outpl sr (((false,a4,a3,a2,a1,a0),Some(s))::rl) (Some(GenSignatReal(s))::rsl) ci
			  with _ ->
			    signtx_ins taue inpr ar outpl sr (((false,a4,a3,a2,a1,a0),None)::rl) (None::rsl) false
			end
		      else if p2shaddr_p alpha then
			raise (Failure "p2sh signing is not yet supported")
		      else
			raise (Failure "tx attempts to spend a non-Marker and non-Bounty without an explicit obligation from an address other than a pay address")

		end
	    | (Some(s)::sr) ->
		try
		  let (s1,rl1) = getsig s rl in
		  let blkh = match obl with Some(_,lkh,_) -> lkh | None -> 1L in (*** artificial block height just for checking signatures ***)
		  if check_spend_obligation alpha blkh taue s1 obl then
		    begin
		      match obl with
		      | None -> 
			  let (p,a4,a3,a2,a1,a0) = alpha in
			  signtx_ins taue inpr ar outpl sr (rl1 (p=1,a4,a3,a2,a1,a0)) (Some(GenSignatReal(s1))::rsl) ci
		      | Some(gam,_,_) ->
			  signtx_ins taue inpr ar outpl sr (rl1 gam) (Some(GenSignatReal(s1))::rsl) ci
		    end
		  else if check_move_obligation alpha taue s1 obl (assetpre a) outpl then
		    let (p,a4,a3,a2,a1,a0) = alpha in
		    signtx_ins taue inpr ar outpl sr (rl1 (p=1,a4,a3,a2,a1,a0)) (Some(GenSignatReal(s1))::rsl) ci
		  else
		    raise (Failure "bad signature already part of stx")
		with BadOrMissingSignature ->
		  raise (Failure "bad signature already part of stx")
      end
  | [],[] -> if sl = [] then (List.rev rsl,ci) else raise (Failure "extra unused signature")
  | _,_ -> raise (Failure "problem signing inputs")

let rec signtx_outs taue outpl sl rl rsl co =
  match outpl with
  | (_,(_,TheoryPublication(alpha,n,thy)))::outpr ->
      raise (Failure "to do: write signtx_outs")
  | (_,(_,SignaPublication(alpha,n,th,si)))::outpr ->
      raise (Failure "to do: write signtx_outs")
  | (_,(_,DocPublication(alpha,n,th,si)))::outpr ->
      raise (Failure "to do: write signtx_outs")
  | _::outpr -> signtx_outs taue outpr sl rl rsl co
  | [] -> (List.rev rsl,co)

let signtx lr taustr =
  let s = hexstring_string taustr in
  let (((tauin,tauout) as tau,(tausgin,tausgout) as tausg),_) = sei_stx seis (s,String.length s,None,0,0) in (*** may be partially signed ***)
  let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr))) in
  let tauh = hashtx tau in
  let tauh2 = if !Config.testnet then hashtag tauh 288l else tauh in
  let taue = hashval_big_int tauh2 in
  let (tausgin1,ci) = signtx_ins taue tauin al tauout tausgin [] [] true in
  let (tausgout1,co) = signtx_outs taue tauout tausgout [] [] true in
  let stau = (tau,(tausgin1,tausgout1)) in
  let s = Buffer.create 100 in
  seosbf (seo_stx seosb (tau,(tausgin1,tausgout1)) (s,None));
  let hs = string_hexstring (Buffer.contents s) in
  Printf.printf "%s\n" hs;
  if ci && co then
    Printf.printf "Completely signed.\n"
  else
    Printf.printf "Partially signed.\n"

let savetxtopool blkh lr staustr =
  let s = hexstring_string staustr in
  let (((tauin,tauout) as tau,tausg),_) = sei_stx seis (s,String.length s,None,0,0) in
  if tx_valid tau then
    let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr))) in
    if tx_signatures_valid blkh al (tau,tausg) then
      let txh = hashtx tau in
      let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 (Filename.concat (datadir()) "txpool") in
      seocf (seo_prod seo_hashval seo_stx seoc (txh,(tau,tausg)) (ch,None));
      close_out ch
    else
      Printf.printf "Invalid or incomplete signatures\n"
  else
    Printf.printf "Invalid tx\n"

let sendtx blkh lr staustr =
  let s = hexstring_string staustr in
  Printf.printf "sendtx 1\n";
  let (((tauin,tauout) as tau,tausg) as stau,_) = sei_stx seis (s,String.length s,None,0,0) in
  if tx_valid tau then
begin
    let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr))) in
    if tx_signatures_valid blkh al (tau,tausg) then
      let txh = hashtx tau in
      let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 (Filename.concat (datadir()) "txpool") in
      seocf (seo_prod seo_hashval seo_stx seoc (txh,(tau,tausg)) (ch,None));
      close_out ch;
      publish_stx txh stau;
      Printf.printf "%s\n" (hashval_hexstring txh);
      flush stdout;
    else
      Printf.printf "Invalid or incomplete signatures\n"
end
  else
    Printf.printf "Invalid tx\n"

let dalilcoin_addr_jsoninfo alpha =
  let (bn,cwl) = get_bestnode true in
  let BlocktreeNode(_,_,pbh,_,_,ledgerroot,_,_,_,_,blkh,_,_,_) = bn in
  let blkh = Int64.sub blkh 1L in
  let jpbh =
    match pbh with
    | None -> JsonObj([("block",JsonStr("genesis"))])
    | Some(prevh,Block.Poburn(lblkh,ltxh,lmedtm,burned)) ->
	JsonObj([("block",JsonStr(hashval_hexstring prevh));
		 ("height",JsonNum(Int64.to_string blkh));
		 ("ltcblock",JsonStr(hashval_hexstring lblkh));
		 ("ltcburntx",JsonStr(hashval_hexstring ltxh));
		 ("ltcmedtm",JsonNum(Int64.to_string lmedtm));
		 ("ltcburned",JsonNum(Int64.to_string burned))])
  in
  let (jal,jwl) = assets_at_address_in_ledger_json alpha ledgerroot blkh in
  if jwl = [] then
    JsonObj(("ledgerroot",JsonStr(hashval_hexstring ledgerroot))::("block",jpbh)::jal)
  else
    JsonObj(("ledgerroot",JsonStr(hashval_hexstring ledgerroot))::("block",jpbh)::("warnings",JsonArr(jwl))::jal)
    
let query q =
  if String.length q = 64 || String.length q = 40 then
    begin
      try
	let q = if String.length q = 64 then q else "000000000000000000000000" ^ q in
	let h = hexstring_hashval q in
	let dbentries = ref [] in
	let (bn,cwl) = get_bestnode true in
	let BlocktreeNode(_,_,pbh,_,_,ledgerroot,_,_,_,_,blkh,_,_,_) = bn in
	let blkh = Int64.sub blkh 1L in
	begin
	  try
	    let e = Assets.DbAsset.dbget h in
	    let s = Buffer.create 100 in
	    print_hlist_to_buffer s blkh (HCons(e,HNil));
	    let j = JsonObj([("type",JsonStr("asset"));("description",JsonStr(Buffer.contents s))]) in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let e = Tx.DbTx.dbget h in
	    let j = JsonObj([("type",JsonStr("tx"))]) in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let (k,r) = Ctre.DbHConsElt.dbget h in
	    let j =
	      match r with
	      | None ->
		  JsonObj([("type",JsonStr("hconselt"));("asset",JsonStr(hashval_hexstring k))])
	      | Some(r) ->
		  JsonObj([("type",JsonStr("hconselt"));("asset",JsonStr(hashval_hexstring k));("next",JsonStr(hashval_hexstring r))])
	    in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let e = Ctre.DbCTreeElt.dbget h in
	    let j = JsonObj([("type",JsonStr("ctreeelt"))]) in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let (bhd,bhs) = Block.DbBlockHeader.dbget h in
	    let invalid = Block.DbInvalidatedBlocks.dbexists h in
	    let pbh = bhd.prevblockhash in
	    let alpha = bhd.stakeaddr in
	    let aid = bhd.stakeassetid in
	    let timestamp = bhd.timestamp in
	    let deltatime = bhd.deltatime in
	    let tinfo = bhd.tinfo in
	    let bblkh =
	      try
		Some(Int64.sub (node_blockheight (Hashtbl.find blkheadernode (Some(h)))) 1L)
	      with Not_found ->
		None
	    in
	    let jpb =
	      match pbh with
	      | None -> []
	      | Some(prevh,Block.Poburn(lblkh,ltxh,lmedtm,burned)) ->
		  match bblkh with
		  | Some(bblkh) ->
		      [("prevblock",
			JsonObj([("block",JsonStr(hashval_hexstring prevh));
				 ("height",JsonNum(Int64.to_string bblkh));
				 ("ltcblock",JsonStr(hashval_hexstring lblkh));
				 ("ltcburntx",JsonStr(hashval_hexstring ltxh));
				 ("ltcmedtm",JsonNum(Int64.to_string lmedtm));
				 ("ltcburned",JsonNum(Int64.to_string burned))]))]
		  | None ->
		      [("prevblock",
			JsonObj([("block",JsonStr(hashval_hexstring prevh));
				 ("ltcblock",JsonStr(hashval_hexstring lblkh));
				 ("ltcburntx",JsonStr(hashval_hexstring ltxh));
				 ("ltcmedtm",JsonNum(Int64.to_string lmedtm));
				 ("ltcburned",JsonNum(Int64.to_string burned))]))]
	    in
	    let jr =
	      jpb @
	      [("stakeaddress",JsonStr(addr_daliladdrstr (p2pkhaddr_addr alpha)));
	       ("stakeassetid",JsonStr(hashval_hexstring aid));
	       ("timestamp",JsonNum(Int64.to_string timestamp));
	       ("deltatime",JsonNum(Int32.to_string deltatime));
	       ("target",JsonStr(string_of_big_int tinfo));
	       ("difficulty",JsonStr(string_of_big_int (difficulty tinfo)))]
	    in
	    let jr =
	      if invalid then
		(("invalid",JsonBool(true))::jr)
	      else
		jr
	    in
	    begin
	      try
		let bd = Block.DbBlockDelta.dbget h in
		let jcoinstkid = JsonStr(hashval_hexstring(hashtx (coinstake ((bhd,bhs),bd)))) in
		let jtxhl =
		  List.map
		    (fun (tau,stau) -> JsonStr(hashval_hexstring(hashtx tau)))
		    bd.blockdelta_stxl
		in
		let j = JsonObj(("type",JsonStr("block"))::jr @ [("coinstk",jcoinstkid);("txs",JsonArr(jtxhl))]) in
		dbentries := j::!dbentries
	      with Not_found ->
		let j = JsonObj(("type",JsonStr("block"))::jr) in
		dbentries := j::!dbentries
	    end
	  with Not_found -> ()
	end;
(***
	begin
	  try
	    let e = Ltcrpc.DbLtcDacStatus.dbget h in
	    let j = JsonObj([("type",JsonStr("ltcblock"))]) in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
***)
	begin
	  try
	    let (burned,lprevtx,dnxt) = Ltcrpc.DbLtcBurnTx.dbget h in
	    let j = JsonObj([("type",JsonStr("ltcburntx"));
			     ("burned",JsonNum(Int64.to_string burned));
			     ("previousltcburntx",JsonStr(hashval_hexstring lprevtx));
			     ("dalilblock",JsonStr(hashval_hexstring dnxt))]) in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let (prevh,tm,hght,txhhs) = Ltcrpc.DbLtcBlock.dbget h in
	    let j = JsonObj([("type",JsonStr("ltcblock"))]) in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	if !dbentries = [] then
	  JsonObj([("response",JsonStr("unknown"));("msg",JsonStr("No associated information found"))])
	else
	  JsonObj([("response",JsonStr("known"));("dbdata",JsonArr(!dbentries))])
      with _ ->
	JsonObj([("response",JsonStr("unknown"));("msg",JsonStr("Cannot interpret as hash value"))])
    end
  else
    begin
      try
	let d = daliladdrstr_addr q in
	let j = dalilcoin_addr_jsoninfo d in
	JsonObj([("response",JsonStr("daliladdress"));("info",j)])
      with _ ->
	try
	  let b = btcaddrstr_addr q in
	  let j = dalilcoin_addr_jsoninfo b in
	  let d = addr_daliladdrstr b in
	  JsonObj([("response",JsonStr("bitcoin address"));("daliladdress",JsonStr(d));("info",j)])
	with _ ->
	  JsonObj([("response",JsonStr("unknown"));("msg",JsonStr("Cannot interpret as dalilcoin value"))])
    end
