(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Copyright (c) 2018 The Tezos' (Tezos Prime) developers *)
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

let walletkeys_staking = ref []
let walletkeys_nonstaking = ref []
let walletkeys_staking_fresh = ref []
let walletkeys_nonstaking_fresh = ref []
let walletp2shs = ref []
let walletendorsements = ref []
let walletwatchaddrs = ref []
let walletwatchaddrs_offlinekey = ref []
let walletwatchaddrs_offlinekey_fresh = ref []
let stakingassets = ref []

let meuniers_balances_in_ledger : (hashval,int64 * int64 * int64 * int64) Hashtbl.t = Hashtbl.create 100

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

let save_txpool () =
  let fn = Filename.concat (datadir()) "txpool" in
  let ch = open_out_bin fn in
  Hashtbl.iter
    (fun txid stau -> seocf (seo_prod seo_hashval Tx.seo_stx seoc (txid,stau) (ch,None)))
    stxpool;
  close_out ch;;

let load_wallet () =
  let wallfn = Filename.concat (datadir()) "wallet" in
  if not (Sys.file_exists wallfn) then
    let s = open_out_bin wallfn in
    begin
      walletkeys_staking := [];
      walletkeys_nonstaking := [];
      walletkeys_staking_fresh := [];
      walletkeys_nonstaking_fresh := [];
      walletp2shs := [];
      walletendorsements := [];
      walletwatchaddrs := [];
      walletwatchaddrs_offlinekey := [];
      walletwatchaddrs_offlinekey_fresh := []
    end
  else
    let s = open_in_bin wallfn in
    try
      while true do
	let by = input_byte s in
	match by with
	| 0 ->
	    let ((k,b),_) = sei_prod sei_big_int_256 sei_bool seic (s,None) in
	    walletkeys_staking :=
	      (match Secp256k1.smulp k Secp256k1._g with
	      | Some(x,y) ->
		  let h = pubkey_hashval (x,y) b in
		  let alpha1 = hashval_md160 h in
		  let alpha = addr_tzpaddrstr (p2pkhaddr_addr alpha1) in
		  (k,b,(x,y),tzpwif k b,alpha1,alpha)
	      | None ->
		  raise (Failure "A private key in the wallet did not give a public key.")
	      )::!walletkeys_staking
	| 4 ->
	    let ((k,b),_) = sei_prod sei_big_int_256 sei_bool seic (s,None) in
	    walletkeys_nonstaking :=
	      (match Secp256k1.smulp k Secp256k1._g with
	      | Some(x,y) ->
		  let h = pubkey_hashval (x,y) b in
		  let alpha1 = hashval_md160 h in
		  let alpha = addr_tzpaddrstr (p2pkhaddr_addr alpha1) in
		  (k,b,(x,y),tzpwif k b,alpha1,alpha)
	      | None ->
		  raise (Failure "A private key in the wallet did not give a public key.")
	      )::!walletkeys_nonstaking
	| 5 ->
	    let ((k,b),_) = sei_prod sei_big_int_256 sei_bool seic (s,None) in
	    walletkeys_staking_fresh :=
	      (match Secp256k1.smulp k Secp256k1._g with
	      | Some(x,y) ->
		  let h = pubkey_hashval (x,y) b in
		  let alpha1 = hashval_md160 h in
		  let alpha = addr_tzpaddrstr (p2pkhaddr_addr alpha1) in
		  (k,b,(x,y),tzpwif k b,alpha1,alpha)
	      | None ->
		  raise (Failure "A private key in the wallet did not give a public key.")
	      )::!walletkeys_staking_fresh
	| 6 ->
	    let ((k,b),_) = sei_prod sei_big_int_256 sei_bool seic (s,None) in
	    walletkeys_nonstaking_fresh :=
	      (match Secp256k1.smulp k Secp256k1._g with
	      | Some(x,y) ->
		  let h = pubkey_hashval (x,y) b in
		  let alpha1 = hashval_md160 h in
		  let alpha = addr_tzpaddrstr (p2pkhaddr_addr alpha1) in
		  (k,b,(x,y),tzpwif k b,alpha1,alpha)
	      | None ->
		  raise (Failure "A private key in the wallet did not give a public key.")
	      )::!walletkeys_nonstaking_fresh
	| 1 ->
	    let (scr,_) = sei_list sei_int8 seic (s,None) in
	    walletp2shs :=
	      (let h = hash160_bytelist scr in
	      let a = addr_tzpaddrstr (p2shaddr_addr h) in
	      (h,a,scr))::!walletp2shs
	| 2 ->
	    let (endors,_) = sei_prod6 sei_payaddr sei_payaddr (sei_prod sei_big_int_256 sei_big_int_256) sei_varintb sei_bool sei_signat seic (s,None) in (*** For each (alpha,beta,esg) beta can use esg to justify signing for alpha; endorsements can be used for spending/moving, but not for staking. ***)
	    walletendorsements := endors::!walletendorsements
	| 3 ->
	    let (watchaddr,_) = sei_addr seic (s,None) in
	    walletwatchaddrs := watchaddr::!walletwatchaddrs
	| 7 ->
	    let (watchaddr,_) = sei_addr seic (s,None) in
	    walletwatchaddrs_offlinekey := watchaddr::!walletwatchaddrs_offlinekey
	| 8 ->
	    let (watchaddr,_) = sei_addr seic (s,None) in
	    walletwatchaddrs_offlinekey_fresh := watchaddr::!walletwatchaddrs_offlinekey_fresh
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
    !walletkeys_staking;
  List.iter
    (fun (k,b,_,_,_,_) ->
      output_byte s 4;
      seocf (seo_prod seo_big_int_256 seo_bool seoc (k,b) (s,None)))
    !walletkeys_nonstaking;
  List.iter
    (fun (k,b,_,_,_,_) ->
      output_byte s 5;
      seocf (seo_prod seo_big_int_256 seo_bool seoc (k,b) (s,None)))
    !walletkeys_staking_fresh;
  List.iter
    (fun (k,b,_,_,_,_) ->
      output_byte s 6;
      seocf (seo_prod seo_big_int_256 seo_bool seoc (k,b) (s,None)))
    !walletkeys_nonstaking_fresh;
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
  List.iter
    (fun watchaddr ->
      output_byte s 7;
      seocf (seo_addr seoc watchaddr (s,None)))
    !walletwatchaddrs_offlinekey;
  List.iter
    (fun watchaddr ->
      output_byte s 8;
      seocf (seo_addr seoc watchaddr (s,None)))
    !walletwatchaddrs_offlinekey_fresh;
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
      let s kl =
	try
	  ignore (List.find (fun (_,_,_,_,h,_) -> h = (x4,x3,x2,x1,x0)) kl);
	  true
	with Not_found -> false
      in
      s !walletkeys_staking || s !walletkeys_nonstaking || s !walletkeys_staking_fresh || s !walletkeys_nonstaking_fresh
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
  List.mem alpha !walletwatchaddrs || List.mem alpha !walletwatchaddrs_offlinekey || List.mem alpha !walletwatchaddrs_offlinekey_fresh

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

let btctotzpaddr a =
  let alpha = btcaddrstr_addr a in
  let a2 = addr_tzpaddrstr alpha in
  Printf.printf "Tezos' address %s corresponds to Bitcoin address %s\n" a2 a

let importprivkey_real oc (k,b) cls report =
  match Secp256k1.smulp k Secp256k1._g with
  | Some(x,y) ->
      let h = hashval_md160 (pubkey_hashval (x,y) b) in
      let alpha = p2pkhaddr_addr h in
      let a = addr_tzpaddrstr alpha in
      let replwall = ref false in
      if privkey_in_wallet_p alpha then raise (Failure "Private key already in wallet.");
      let clsn =
	if cls = "nonstaking" then 4 else if cls = "staking_fresh" then 5 else if cls = "nonstaking_fresh" then 6 else 0
      in
      let wr = if clsn = 4 then walletkeys_nonstaking else if clsn = 5 then walletkeys_staking_fresh else if clsn = 6 then walletkeys_nonstaking_fresh else walletkeys_staking in
      wr := (k,b,(x,y),tzpwif k b,h,a)::!wr;
      walletendorsements := (*** remove endorsements if the wallet has the private key for the address, since it can now sign directly ***)
	List.filter
	  (fun (alpha2,beta,(x,y),recid,fcomp,esg) -> if alpha = payaddr_addr alpha2 then (replwall := true; false) else true)
	  !walletendorsements;
      walletwatchaddrs :=
	List.filter
	  (fun alpha2 -> if alpha = alpha2 then (replwall := true; false) else true)
	  !walletwatchaddrs;
      walletwatchaddrs_offlinekey :=
	List.filter
	  (fun alpha2 -> if alpha = alpha2 then (replwall := true; false) else true)
	  !walletwatchaddrs_offlinekey;
      walletwatchaddrs_offlinekey_fresh :=
	List.filter
	  (fun alpha2 -> if alpha = alpha2 then (replwall := true; false) else true)
	  !walletwatchaddrs_offlinekey_fresh;
      if !replwall then
	save_wallet()
      else
	append_wallet
	  (fun s ->
	    output_byte s clsn;
	    seocf (seo_prod seo_big_int_256 seo_bool seoc (k,b) (s,None)));
      if report then Printf.fprintf oc "Imported key for address %s\n" a;
      flush stdout
  | None ->
      raise (Failure "This private key does not give a public key.")

let importprivkey oc w cls =
  let (k,b) = privkey_from_wif w in
  let w2 = tzpwif k b in
  if not (w2 = w) then raise (Failure (w ^ " is not a valid Tezos' wif"));
  importprivkey_real oc (k,b) cls true

let importbtcprivkey oc w cls =
  let (k,b) = privkey_from_btcwif w in
  importprivkey_real oc (k,b) cls true

let importendorsement a b s =
  let alpha = tzpaddrstr_addr a in
  let beta = tzpaddrstr_addr b in
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
	      match verifybitcoinmessage_recover (-916116462l, -1122756662l, 602820575l, 669938289l, 1956032577l) recid fcomp esg ("fakeendorsement " ^ b ^ " (" ^ (addr_tzpaddrstr alpha) ^ ")") with
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
    raise (Failure (a ^ " expected to be a p2pkh or p2sh Tezos' address."))

let importwatchaddr oc a cls =
  let alpha = tzpaddrstr_addr a in
  let a2 = addr_tzpaddrstr alpha in
  if not (a2 = a) then raise (Failure (a ^ " is not a valid Tezos' address"));
  if privkey_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has the private key for this address.");
  if endorsement_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has an endorsement for this address.");
  if watchaddr_in_wallet_p alpha then raise (Failure "Watch address is already in wallet.");
  if cls = "offlinekey" then
    walletwatchaddrs_offlinekey := alpha::!walletwatchaddrs_offlinekey
  else if cls = "offlinekey_fresh" then
    walletwatchaddrs_offlinekey_fresh := alpha::!walletwatchaddrs_offlinekey_fresh
  else
    walletwatchaddrs := alpha::!walletwatchaddrs;
  save_wallet() (*** overkill, should append if possible ***)

let importwatchbtcaddr oc a cls =
  let alpha = btcaddrstr_addr a in
  let a2 = addr_tzpaddrstr alpha in
  Printf.printf "Importing as Tezos' address %s\n" a2;
  if privkey_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has the private key for this address.");
  if endorsement_in_wallet_p alpha then raise (Failure "Not adding as a watch address since the wallet already has an endorsement for this address.");
  if watchaddr_in_wallet_p alpha then raise (Failure "Watch address is already in wallet.");
  if cls = "offlinekey" then
    walletwatchaddrs_offlinekey := alpha::!walletwatchaddrs_offlinekey
  else if cls = "offlinekey_fresh" then
    walletwatchaddrs_offlinekey_fresh := alpha::!walletwatchaddrs_offlinekey_fresh
  else
    walletwatchaddrs := alpha::!walletwatchaddrs;
  save_wallet() (*** overkill, should append if possible ***)

(*** make sure we locally know the contents of the address (which should be empty, of course) ***)
let rec randomly_generate_newkeyandaddress ledgerroot cls =
  let giveup = ref 65536 in
  let k = strong_rand_256() in
  let b = true in (*** compressed ***)
  let rec newkeyandaddress_rec k =
    match Secp256k1.smulp k Secp256k1._g with
    | None -> (*** try again, in the very unlikely event this happened ***)
	randomly_generate_newkeyandaddress ledgerroot cls
    | Some(x,y) ->
	let w = tzpwif k true in
	let h = hashval_md160 (pubkey_hashval (x,y) b) in
	let alpha = p2pkhaddr_addr h in
	try
	  ignore (ctree_addr true false alpha (CHash(ledgerroot)) None);
	  let a = addr_tzpaddrstr alpha in
	  Utils.log_string (Printf.sprintf "Importing privkey %s for address %s\n" w a);
	  importprivkey_real !Utils.log (k,b) cls false;
	  (k,h)
	with Not_found ->
	  decr giveup;
	  if !giveup > 0 then
	    newkeyandaddress_rec (succ_big_int k)
	  else
	    raise (Failure "could not generature a new address accessible by the local ledger")
  in
  newkeyandaddress_rec k

let generate_newkeyandaddress ledgerroot cls =
  if cls = "" || cls = "staking" then
    begin
      match !walletkeys_staking_fresh with
      | (k::wr) ->
	  walletkeys_staking_fresh := wr;
	  walletkeys_staking := k::!walletkeys_staking;
	  save_wallet();
	  let (k,_,_,_,h,_) = k in
	  (k,h)
      | [] ->
	  randomly_generate_newkeyandaddress ledgerroot cls
    end
  else
    begin
      match !walletkeys_nonstaking_fresh with
      | (k::wr) ->
	  walletkeys_nonstaking_fresh := wr;
	  walletkeys_nonstaking := k::!walletkeys_nonstaking;
	  save_wallet();
	  let (k,_,_,_,h,_) = k in
	  (k,h)
      | [] ->
	  randomly_generate_newkeyandaddress ledgerroot cls
    end

let get_fresh_offline_address oc =
  match !walletwatchaddrs_offlinekey_fresh with
  | alpha::wr ->
      walletwatchaddrs_offlinekey := alpha::!walletwatchaddrs_offlinekey;
      walletwatchaddrs_offlinekey_fresh := wr;
      save_wallet();
      alpha
  | _ ->
      Printf.fprintf oc "No fresh offline addresses\n";
      raise (Failure("out of fresh offline addresses"))

let reclassify_staking oc alpha b =
  let (p,x4,x3,x2,x1,x0) = tzpaddrstr_addr alpha in
  if not (p = 0) then
    Printf.fprintf oc "%s is not p2pkh\n" alpha
  else if b then
    begin (*** from staking to nonstaking ***)
      try
	let ke = List.find (fun (_,_,_,_,h,_) -> h = (x4,x3,x2,x1,x0)) !walletkeys_nonstaking in
	let (k,b,(x,y),w,h,a) = ke in
	walletkeys_staking := (k,b,(x,y),w,h,a)::!walletkeys_staking;
	walletkeys_nonstaking := List.filter (fun (_,_,_,_,h,_) -> not (h = (x4,x3,x2,x1,x0))) !walletkeys_nonstaking;
	save_wallet()
      with Not_found ->
	Printf.fprintf oc "%s is not among the nonstaking keys in the wallet\n" alpha
    end
  else
    begin (*** from nonstaking to staking ***)
      try
	let ke = List.find (fun (_,_,_,_,h,_) -> h = (x4,x3,x2,x1,x0)) !walletkeys_staking in
	let (k,b,(x,y),w,h,a) = ke in
	walletkeys_nonstaking := (k,b,(x,y),w,h,a)::!walletkeys_nonstaking;
	walletkeys_staking := List.filter (fun (_,_,_,_,h,_) -> not (h = (x4,x3,x2,x1,x0))) !walletkeys_staking;
	save_wallet()
      with Not_found ->
	Printf.fprintf oc "%s is not among the staking keys in the wallet\n" alpha
    end

exception EmptyAddress

let assets_at_address_in_ledger_json raiseempty alpha par ledgerroot blkh =
  let cache : (hashval,nehlist option * int) Hashtbl.t = Hashtbl.create 100 in
  let reported : (hashval,unit) Hashtbl.t = Hashtbl.create 100 in
  let alphas = addr_tzpaddrstr alpha in
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
  handler (fun () -> alpha_hl := Ctre.ctree_addr_cache cache true false alpha ctr None);
  let sumcurr a =
    match a with
    | (_,_,_,Currency(v)) -> tot := Int64.add !tot v
    | _ -> ()
  in
  let rec hlist_report_assets_json hl =
    match hl with
    | HHash(_,_) -> []
    | HNil -> []
    | HCons(a,hr) ->
	let ah = hashasset a in
	if not (Hashtbl.mem reported ah) then
	  begin
	    Hashtbl.add reported ah ();
	    json_asset a::hlist_report_assets_json hr
	  end
	else
	  hlist_report_assets_json hr
    | HConsH(ah,hr) ->
	hlist_report_assets_json hr
  in
  begin
    match !alpha_hl with
    | (Some(hl),_) ->
	let jhl = hlist_report_assets_json (Ctre.nehlist_hlist hl) in
	let s = Buffer.create 100 in
	Ctre.print_hlist_to_buffer_gen s blkh (Ctre.nehlist_hlist hl) sumcurr;
	jal := [("address",JsonStr(alphas));("total",JsonNum(tezzies_of_meuniers !tot));("contents",JsonStr(Buffer.contents s));("currentassets",JsonArr(jhl))]
    | (None,z) ->
	if raiseempty then
	  raise EmptyAddress
	else if z < 0 then
	  begin
	    jwl := JsonObj([("warning",JsonStr("Problem obtaining contents of address."))])::!jwl;
	    jal := [("address",JsonStr(alphas))]
	  end
	else
	  jal := [("address",JsonStr(alphas));("contents",JsonStr("empty"))]
    | _ ->
	jal := [("address",JsonStr(alphas));("contents",JsonStr("no information"))]
  end;
  let rec assets_at_address_in_ledger_json_history alpha par =
    match par with
    | None -> []
    | Some(BlocktreeNode(par,_,_,_,_,ledgerroot,_,_,_,_,blkh,_,_,_)) ->
	handler (fun () -> alpha_hl := Ctre.ctree_addr_cache cache true false alpha (Ctre.CHash(ledgerroot)) None);	
	match !alpha_hl with
	| (Some(hl),_) ->
	    let jhl =
	      List.map (fun j -> JsonObj([("type",JsonStr("spentasset"));
					  ("spentheight",JsonNum(Int64.to_string blkh));
					  ("asset",j)]))
		(hlist_report_assets_json (Ctre.nehlist_hlist hl))
	    in
	    jhl @ assets_at_address_in_ledger_json_history alpha par
	| (None,z) ->
	    assets_at_address_in_ledger_json_history alpha par
  in
  let jhl = assets_at_address_in_ledger_json_history alpha par in
  if not (jhl = []) then jal := ("historic",JsonArr(jhl))::!jal;
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
    !walletkeys_staking;
  List.iter
    (fun (k,b,(x,y),w,h,z) ->
      handler (fun () -> al1 := (z,Ctre.ctree_addr true true (p2pkhaddr_addr h) ctr None)::!al1))
    !walletkeys_nonstaking;
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
  List.iter
    (fun alpha ->
      handler (fun () -> al4 := (alpha,Ctre.ctree_addr true true alpha ctr None)::!al4))
    !walletwatchaddrs_offlinekey;
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
	  Printf.fprintf oc "%s:\n" (addr_tzpaddrstr alpha2);
	  Ctre.print_hlist_gen oc (Ctre.nehlist_hlist hl) (sumcurr tot3)
      | (None,_) ->
	  Printf.fprintf oc "%s: empty\n" (addr_tzpaddrstr alpha2);
      | _ ->
	  Printf.fprintf oc "%s: no information\n" (addr_tzpaddrstr alpha2);
    )
    !al3;
  Printf.fprintf oc "Watched assets:\n";
  List.iter
    (fun (alpha,x) ->
      match x with
      | (Some(hl),_) ->
	  Printf.fprintf oc "%s:\n" (addr_tzpaddrstr alpha);
	  Ctre.print_hlist_gen oc (Ctre.nehlist_hlist hl) (sumcurr tot4)
      | (None,_) ->
	  Printf.fprintf oc "%s: empty\n" (addr_tzpaddrstr alpha);
      | _ ->
	  Printf.fprintf oc "%s: no information\n" (addr_tzpaddrstr alpha);
    )
    !al4;
  Printf.fprintf oc "Total p2pkh: %s prime tezzies\n" (tezzies_of_meuniers !tot1);
  Printf.fprintf oc "Total p2sh: %s prime tezzies\n" (tezzies_of_meuniers !tot2);
  Printf.fprintf oc "Total via endorsement: %s prime tezzies\n" (tezzies_of_meuniers !tot3);
  Printf.fprintf oc "Total watched: %s prime tezzies\n" (tezzies_of_meuniers !tot4);
  Hashtbl.replace meuniers_balances_in_ledger ledgerroot (!tot1,!tot2,!tot3,!tot4) (*** preventing recomputation for getting balances if the ledger has not changed ***)

let printassets oc =
  match !artificialledgerroot with
  | Some(ledgerroot) ->
      printassets_in_ledger oc ledgerroot
  | None ->
      let (bn,cwl) = get_bestnode true in
      let BlocktreeNode(_,_,_,_,_,ledgerroot,_,_,_,_,_,_,_,_) = bn in
      printassets_in_ledger oc ledgerroot

let get_meuniers_balances_in_ledger oc ledgerroot =
  try
    Hashtbl.find meuniers_balances_in_ledger ledgerroot
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
      | HHash(hh,_) -> hlist_sumcurr tot (get_hlist_element hh)
    in
    let rec nehlist_sumcurr tot (hl:nehlist) =
      match hl with
      | NehConsH(ah,hr) -> hlist_sumcurr tot hr; asset_sumcurr tot (get_asset ah)
      | NehCons(a,hr) -> hlist_sumcurr tot hr; asset_sumcurr tot a
      | NehHash(hh,_) -> nehlist_sumcurr tot (get_nehlist_element hh)
    in
    List.iter
      (fun (k,b,(x,y),w,h,z) ->
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true (p2pkhaddr_addr h) ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot1 hl
	    | _ -> ()))
      !walletkeys_staking;
    List.iter
      (fun (k,b,(x,y),w,h,z) ->
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true (p2pkhaddr_addr h) ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot1 hl
	    | _ -> ()))
      !walletkeys_nonstaking;
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
    List.iter
      (fun alpha ->
	handler
	  (fun () ->
	    match Ctre.ctree_addr true true alpha ctr None with
	      (Some(hl),_) -> nehlist_sumcurr tot4 hl
	    | _ -> ()))
      !walletwatchaddrs_offlinekey;
    Hashtbl.add meuniers_balances_in_ledger ledgerroot (!tot1,!tot2,!tot3,!tot4);
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
    | Some(k,l) -> Printf.printf "next hcons elt %s[%d]\n" (hashval_hexstring k) l
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
	| Some(k,_) ->
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
      | CLeaf(_,NehHash(h,_)) ->
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
    Printf.printf "Total meuniers in known currency assets %Ld\n" !v;
    Printf.printf "Total meuniers in known bounty assets %Ld\n" !b;
  with Not_found ->
    Printf.printf "No ctree %s found\n" (hashval_hexstring h)
  
let printtx_a (tauin,tauout) =
  let i = ref 0 in
  Printf.printf "Inputs (%d):\n" (List.length tauin);
  List.iter
    (fun (alpha,aid) ->
      Printf.printf "Input %d:%s %s\n" !i (addr_tzpaddrstr alpha) (hashval_hexstring aid);
      incr i)
    tauin;      
  i := 0;
  Printf.printf "Outputs (%d):\n" (List.length tauout);
  List.iter
    (fun (alpha,(obl,u)) ->
      Printf.printf "Output %d:%s %s %s\n" !i (addr_tzpaddrstr alpha) (preasset_string u) (obligation_string obl);
      incr i)
    tauout

let printtx txid =
  try
    let (tau,_) = Hashtbl.find stxpool txid in
    Printf.printf "Tx %s in pool.\n" (hashval_hexstring txid);
    printtx_a tau
  with Not_found ->
    try
      let (tau,_) = DbSTx.dbget txid in
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
		  (tzpaddrstr_addr alpha,hexstring_hashval aidhex)
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
			      let beta2 = tzpaddrstr_addr beta in
			      let v = meuniers_of_tezzies x in
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
					    let gamma2 = tzpaddrstr_addr obladdr in
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
  | None -> Printf.printf "Could not find asset %s at %s in ledger %s\n" (hashval_hexstring aid) (addr_tzpaddrstr alpha2) (hashval_hexstring ledgerroot); flush stdout
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
	      Printf.printf "Asset %s is %s prime tezzies, which is smaller than %d meuniers after subtracting the fee of %s\n" (hashval_hexstring aid) (tezzies_of_meuniers v) i (tezzies_of_meuniers v); flush stdout
	    end	  
	end
      else
	begin
	  Printf.printf "Asset %s is %s prime tezzies, which is not greater the fee of %s\n" (hashval_hexstring aid) (tezzies_of_meuniers v) (tezzies_of_meuniers v); flush stdout
	end
  | _ -> Printf.printf "Asset %s is not currency.\n" (hashval_hexstring aid); flush stdout

(*** first see if private key for beta is in the wallet; if not check if an endorsement is in the wallet; if not fail ***)
let signtx_p2pkh beta taue =
  try
    let (k,b,(x,y),w,h,z) = List.find (fun (_,_,_,_,h,_) -> h = beta) !walletkeys_staking in
    let r = rand_256() in
    P2pkhSignat(Some(x,y),b,signat_big_int taue k r)
  with Not_found ->
    try
      let (k,b,(x,y),w,h,z) = List.find (fun (_,_,_,_,h,_) -> h = beta) !walletkeys_nonstaking in
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
	try
	  let (k,b2,(x2,y2),w,h,z) = List.find (fun (_,_,_,_,h,_) -> h = (c4,c3,c2,c1,c0)) !walletkeys_staking in
	  let r = rand_256() in
	  let s1 = signat_big_int taue k r in
	  let s = EndP2pkhToP2pkhSignat(Some(x,y),fcomp,Some(x2,y2),b2,esg,s1) in
	  s
	with Not_found ->
	  let (k,b2,(x2,y2),w,h,z) = List.find (fun (_,_,_,_,h,_) -> h = (c4,c3,c2,c1,c0)) !walletkeys_nonstaking in
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

let rec signtx_ins taue inpl al outpl sl rl (rsl:gensignat_or_ref option list) ci propowns =
  match inpl,al with
  | (alpha,k)::inpr,(a::ar) ->
      begin
	if not (assetid a = k) then raise (Failure "Asset mismatch when trying to sign inputs");
	match a with
	| (_,_,_,Marker) -> signtx_ins taue inpr ar outpl sl rl rsl ci propowns
	| (_,_,obl,Bounty(_)) ->
	    begin
	      if List.mem alpha propowns then (*** no signature required, bounty is being collected by owner of prop/negprop ***)
		signtx_ins taue inpr ar outpl sl rl rsl ci propowns
	      else
		match obl with
		| None -> (*** Bounty cannot be spent, but can only be collected ***)
		    raise (Failure("bad attempt to spend bounty"))
		| Some(gamma,lkh,_) -> (*** gamma must sign to spend bounty where prop (or neg prop) owner is not part of tx ***)
		    begin
		      match sl with
		      | [] -> signtx_ins taue inpl al outpl [None] rl rsl ci propowns
		      | (None::sr) -> (*** missing signature ***)
			  begin
			    try
			      match assoc_pos gamma rl 0 with
			      | (Some(s),p) ->
				  signtx_ins taue inpr ar outpl sr rl (Some(GenSignatRef(p))::rsl) ci propowns
			      | (None,p) -> raise Not_found
			    with Not_found ->
			      let (p,b4,b3,b2,b1,b0) = gamma in
			      if p then
				raise (Failure "p2sh signing is not yet supported")
			      else
				begin
				  try
				    let s = signtx_p2pkh (b4,b3,b2,b1,b0) taue in
				    signtx_ins taue inpr ar outpl sr ((gamma,Some(s))::rl) (Some(GenSignatReal(s))::rsl) ci propowns
				  with _ ->
				    signtx_ins taue inpr ar outpl sr ((gamma,None)::rl) (None::rsl) false propowns
				end
			  end
		      | (Some(s)::sr) ->
			  try
			    let obl = assetobl a in
			    let (s1,rl1) = getsig s rl in
			    let blkh = lkh in (*** actually, should allow signing before the lockheight, it just can't be confirmed before the lockheight ***)
			    if check_spend_obligation alpha blkh taue s1 obl then
			      begin
				match obl with
				| None -> 
				    let (p,a4,a3,a2,a1,a0) = alpha in
				    signtx_ins taue inpr ar outpl sr (rl1 (p=1,a4,a3,a2,a1,a0)) (Some(GenSignatReal(s1))::rsl) ci propowns
				| Some(gam,_,_) ->
				    signtx_ins taue inpr ar outpl sr (rl1 gam) (Some(GenSignatReal(s1))::rsl) ci propowns
			      end
			    else
			      raise (Failure "bad signature already part of stx")
			  with BadOrMissingSignature ->
			    raise (Failure "bad signature already part of stx")
		    end
	    end
	| _ ->
	    let obl = assetobl a in
	    match sl with
	    | [] -> signtx_ins taue inpl al outpl [None] rl rsl ci propowns
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
			signtx_ins taue inpr ar outpl sr rl (Some(GenSignatRef(p))::rsl) ci propowns
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
			      signtx_ins taue inpr ar outpl sr ((beta,Some(s))::rl) (Some(GenSignatReal(s))::rsl) ci propowns
			    with _ ->
			      signtx_ins taue inpr ar outpl sr ((beta,None)::rl) (None::rsl) false propowns
			  end
		  | None ->
		      if p2pkhaddr_p alpha then
			let (_,a4,a3,a2,a1,a0) = alpha in
			begin
			  try
			    let s = signtx_p2pkh (a4,a3,a2,a1,a0) taue in
			    signtx_ins taue inpr ar outpl sr (((false,a4,a3,a2,a1,a0),Some(s))::rl) (Some(GenSignatReal(s))::rsl) ci propowns
			  with _ ->
			    signtx_ins taue inpr ar outpl sr (((false,a4,a3,a2,a1,a0),None)::rl) (None::rsl) false propowns
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
			  signtx_ins taue inpr ar outpl sr (rl1 (p=1,a4,a3,a2,a1,a0)) (Some(GenSignatReal(s1))::rsl) ci propowns
		      | Some(gam,_,_) ->
			  signtx_ins taue inpr ar outpl sr (rl1 gam) (Some(GenSignatReal(s1))::rsl) ci propowns
		    end
		  else if check_move_obligation alpha taue s1 obl (assetpre a) outpl then
		    let (p,a4,a3,a2,a1,a0) = alpha in
		    signtx_ins taue inpr ar outpl sr (rl1 (p=1,a4,a3,a2,a1,a0)) (Some(GenSignatReal(s1))::rsl) ci propowns
		  else
		    raise (Failure "bad signature already part of stx")
		with BadOrMissingSignature ->
		  raise (Failure "bad signature already part of stx")
      end
  | [],[] -> if sl = [] then (List.rev rsl,ci) else raise (Failure "extra unused signature")
  | _,_ -> raise (Failure "problem signing inputs")

let rec signtx_outs taue outpl sl rl rsl co =
  let publication_signtx_out alpha outpr =
      begin
	match sl with
	| [] -> signtx_outs taue outpl [None] rl rsl co
	| (None::sr) -> (*** missing signature ***)
	    begin
	      let (p,a4,a3,a2,a1,a0) = alpha in
	      if p then
		raise (Failure "p2sh signing is not yet supported")
	      else
		begin
		  try
		    let s = signtx_p2pkh (a4,a3,a2,a1,a0) taue in
		    signtx_outs taue outpr sr ((alpha,Some(s))::rl) (Some(GenSignatReal(s))::rsl) co
		  with _ ->
		    signtx_outs taue outpr sr ((alpha,None)::rl) (None::rsl) false
		end
	    end
	| (Some(s)::sr) ->
	    begin
	      let (s1,rl1) = getsig s rl in
	      let blkh = 1L in
	      if check_spend_obligation (payaddr_addr alpha) blkh taue s1 None then
		signtx_outs taue outpr sr (rl1 alpha) (Some(GenSignatReal(s1))::rsl) co
	      else
		raise (Failure "bad signature already part of stx")
	    end
      end
  in
  match outpl with
  | (_,(_,DocPublication(alpha,n,si)))::outpr ->
      publication_signtx_out alpha outpr
  | _::outpr -> signtx_outs taue outpr sl rl rsl co
  | [] -> (List.rev rsl,co)

let signtx oc lr taustr =
  let s = hexstring_string taustr in
  let (((tauin,tauout) as tau,(tausgin,tausgout) as tausg),_) = sei_stx seis (s,String.length s,None,0,0) in (*** may be partially signed ***)
  let unsupportederror alpha h = Printf.printf "Could not find asset %s at address %s in ledger %s\n" (hashval_hexstring h) (addr_tzpaddrstr alpha) (hashval_hexstring lr) in
  let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr)) unsupportederror) in
  let rec get_propowns tauin al =
    match tauin,al with
    | ((alpha,aid1)::tauinr),((aid2,_,_,OwnsProp(_,_,_))::ar) when aid1 = aid2 -> alpha::get_propowns tauinr ar
    | ((alpha,aid1)::tauinr),((aid2,_,_,OwnsNegProp)::ar) when aid1 = aid2 -> alpha::get_propowns tauinr ar
    | ((_,aid1)::tauinr),((aid2,_,_,_)::ar) when aid1 = aid2 -> get_propowns tauinr ar
    | [],[] -> []
    | _,_ -> raise BadOrMissingSignature (*** actually this means the asset list does not match the inputs ***)
  in
  let tauh = hashtx tau in
  let tauh2 = if !Config.testnet then hashtag tauh 288l else tauh in
  let taue = hashval_big_int tauh2 in
  let (tausgin1,ci) = signtx_ins taue tauin al tauout tausgin [] [] true (get_propowns tauin al) in
  let (tausgout1,co) = signtx_outs taue tauout tausgout [] [] true in
  let stau = (tau,(tausgin1,tausgout1)) in
  let s = Buffer.create 100 in
  seosbf (seo_stx seosb (tau,(tausgin1,tausgout1)) (s,None));
  let hs = string_hexstring (Buffer.contents s) in
  Printf.fprintf oc "%s\n" hs;
  if ci && co then
    Printf.fprintf oc "Completely signed.\n"
  else
    Printf.fprintf oc "Partially signed.\n"

let savetxtopool blkh lr staustr =
  let s = hexstring_string staustr in
  let (((tauin,tauout) as tau,tausg),_) = sei_stx seis (s,String.length s,None,0,0) in
  if tx_valid tau then
    let unsupportederror alpha h = Printf.printf "Could not find asset %s at address %s in ledger %s\n" (hashval_hexstring h) (addr_tzpaddrstr alpha) (hashval_hexstring lr) in
    let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr)) unsupportederror) in
    if tx_signatures_valid blkh al (tau,tausg) then
      let txid = hashstx (tau,tausg) in
      savetxtopool_real txid (tau,tausg)
    else
      Printf.printf "Invalid or incomplete signatures\n"
  else
    Printf.printf "Invalid tx\n"

let validatetx oc blkh tr sr lr staustr =
  let s = hexstring_string staustr in
  let (((tauin,tauout) as tau,tausg) as stau,_) = sei_stx seis (s,String.length s,None,0,0) in
  if tx_valid_oc oc tau then
    begin
      let unsupportederror alpha h = Printf.fprintf oc "Could not find asset %s at address %s in ledger %s\n" (hashval_hexstring h) (addr_tzpaddrstr alpha) (hashval_hexstring lr) in
      let validatetx_report() =
	let stxh = hashstx stau in
	Printf.fprintf oc "Tx is valid and has id %s\n" (hashval_hexstring stxh);
	begin
	  try
	    verbose_supportedcheck := Some(oc);
	    let nfee = ctree_supports_tx true false blkh tau (CHash(lr)) in
	    verbose_supportedcheck := None;
	    let fee = Int64.sub 0L nfee in
	    if fee < 0L then
              Printf.fprintf oc "Tx is supported by the current ledger and but requires %s prime tezzies more input.\n" (Cryptocurr.tezzies_of_meuniers (Int64.neg fee))
	    else if fee >= !Config.minrelayfee then
	      Printf.fprintf oc "Tx is supported by the current ledger and has fee %s prime tezzies (above minrelayfee %s prime tezzies)\n" (Cryptocurr.tezzies_of_meuniers fee) (Cryptocurr.tezzies_of_meuniers !Config.minrelayfee)
            else
	      Printf.fprintf oc "Tx is supported by the current ledger and has fee %s prime tezzies (below minrelayfee %s prime tezzies)\n" (Cryptocurr.tezzies_of_meuniers fee) (Cryptocurr.tezzies_of_meuniers !Config.minrelayfee);
	    flush oc
	  with
	  | NotSupported ->
	      verbose_supportedcheck := None;
	      Printf.fprintf oc "Tx is not supported by the current ledger\n";
	      flush oc;
	  | exn ->
	      verbose_supportedcheck := None;
	      Printf.fprintf oc "Tx is not supported by the current ledger: %s\n" (Printexc.to_string exn);
	      flush oc;
	end
      in
      let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr)) unsupportederror) in
      try
	let b = tx_signatures_valid_asof_blkh al (tau,tausg) in
	match b with
	| None ->
	    validatetx_report()
	| Some(b) ->
	    if b > blkh then
	      begin
		Printf.fprintf oc "Tx is not valid until block height %Ld\n" b;
		flush oc
	      end
	    else
	      validatetx_report()
      with BadOrMissingSignature ->
	Printf.fprintf oc "Invalid or incomplete signatures\n";
	validatetx_report()
    end
  else
    Printf.fprintf oc "Invalid tx\n"

let sendtx oc blkh tr sr lr staustr =
  let s = hexstring_string staustr in
  let (((tauin,tauout) as tau,tausg) as stau,_) = sei_stx seis (s,String.length s,None,0,0) in
  if tx_valid tau then
    begin
      let unsupportederror alpha h = Printf.fprintf oc "Could not find asset %s at address %s in ledger %s\n" (hashval_hexstring h) (addr_tzpaddrstr alpha) (hashval_hexstring lr) in
      let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr)) unsupportederror) in
      try
	let b = tx_signatures_valid_asof_blkh al (tau,tausg) in
	match b with
	| None ->
	    let stxh = hashstx stau in
	    savetxtopool_real stxh stau;
	    publish_stx stxh stau;
	    Printf.fprintf oc "%s\n" (hashval_hexstring stxh);
	    flush stdout;
	| Some(b) ->
	    if b > blkh then
	      begin
		Printf.fprintf oc "Tx is not valid until block height %Ld\n" b;
		flush stdout
	      end
	    else
	      let stxh = hashstx stau in
	      begin
		try
		  let nfee = ctree_supports_tx true false blkh tau (CHash(lr)) in
		  let fee = Int64.sub 0L nfee in
		  if fee >= !Config.minrelayfee then
		    begin
		      savetxtopool_real stxh stau;
		      publish_stx stxh stau;
		      Printf.fprintf oc "%s\n" (hashval_hexstring stxh);
		    end
		  else
		    Printf.fprintf oc "Tx is supported by the current ledger, but has too low fee of %s prime tezzies (below minrelayfee %s prime tezzies)\n" (Cryptocurr.tezzies_of_meuniers fee) (Cryptocurr.tezzies_of_meuniers !Config.minrelayfee);
		  flush oc
		with
		| NotSupported ->
		  Printf.fprintf oc "Tx is not supported by the current ledger\n";
		  flush oc;
		| exn ->
		  Printf.fprintf oc "Tx is not supported by the current ledger: %s\n" (Printexc.to_string exn);
		  flush oc;
	      end
      with BadOrMissingSignature ->
	Printf.fprintf oc "Invalid or incomplete signatures\n"
    end
  else
    Printf.fprintf oc "Invalid tx\n"

(*** should gather historic information as well ***)
let tzp_addr_jsoninfo raiseempty alpha pbh ledgerroot blkh =
  let blkh = Int64.sub blkh 1L in
  let (jpbh,par) =
    match pbh with
    | None -> (JsonObj([("block",JsonStr("genesis"))]),None)
    | Some(prevh) ->
	begin
	  let jpbh = JsonObj([("block",JsonStr(hashval_hexstring prevh));
			      ("height",JsonNum(Int64.to_string blkh))])
	  in
	  try
	    let BlocktreeNode(par,_,_,_,_,_,_,_,_,_,_,_,_,_) = Hashtbl.find blkheadernode (Some(prevh)) in
	    (jpbh,par)
	  with Not_found ->
	    (jpbh,None)
	end
  in
  let (jal,jwl) = assets_at_address_in_ledger_json raiseempty alpha par ledgerroot blkh in
  if jwl = [] then
    JsonObj(("ledgerroot",JsonStr(hashval_hexstring ledgerroot))::("block",jpbh)::jal)
  else
    JsonObj(("ledgerroot",JsonStr(hashval_hexstring ledgerroot))::("block",jpbh)::("warnings",JsonArr(jwl))::jal)
    
let query_at_block q pbh ledgerroot blkh =
  if String.length q = 64 || String.length q = 40 then
    begin
      try
	let q = if String.length q = 64 then q else "000000000000000000000000" ^ q in
	let h = hexstring_hashval q in
	let dbentries = ref [] in
	let blkh = Int64.sub blkh 1L in
	begin
	  try
	    let e = Assets.DbAsset.dbget h in
	    let s = Buffer.create 100 in
	    print_hlist_to_buffer s blkh (HCons(e,HNil));
	    let j = json_asset e in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let alpha = Assets.DbAssetIdAt.dbget h in
	    let j = tzp_addr_jsoninfo true alpha pbh ledgerroot blkh in
	    dbentries := j::!dbentries
	  with
	  | EmptyAddress -> ()
	  | Not_found -> ()
	end;
	begin
	  try
	    let e = Tx.DbSTx.dbget h in
	    let j = json_stx e in
	    dbentries := j::!dbentries
	  with Not_found ->
	    try
	      let e = Hashtbl.find stxpool h in
	      let j = json_stx e in
	      dbentries := j::!dbentries
	    with Not_found ->
	      ()
	end;
	begin
	  try
	    let (k,r) = Ctre.DbHConsElt.dbget h in
	    let j =
	      match r with
	      | None ->
		  JsonObj([("type",JsonStr("hconselt"));("asset",JsonStr(hashval_hexstring k))])
	      | Some(r,l) ->
		  JsonObj([("type",JsonStr("hconselt"));("asset",JsonStr(hashval_hexstring k));("next",JsonStr(hashval_hexstring r));("nextlen",JsonStr(string_of_int l))])
	    in
	    dbentries := j::!dbentries
	  with Not_found -> ()
	end;
	begin
	  try
	    let e = Ctre.DbCTreeElt.dbget h in
	    let j = JsonObj([("type",JsonStr("ctreeelt"));("ctree",json_ctree(e))]) in
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
	      | Some(prevh) ->
		  match bblkh with
		  | Some(bblkh) ->
		      [("height",JsonNum(Int64.to_string bblkh));
		        ("prevblock",
			 JsonObj([("block",JsonStr(hashval_hexstring prevh))]))]
		  | None ->
		      [("prevblock",
			JsonObj([("block",JsonStr(hashval_hexstring prevh))]))]
	    in
	    let jr =
	      jpb @
	      [("stakeaddress",JsonStr(addr_tzpaddrstr (p2pkhaddr_addr alpha)));
	       ("stakeassetid",JsonStr(hashval_hexstring aid));
	       ("timestamp",JsonNum(Int64.to_string timestamp));
	       ("deltatime",JsonNum(Int32.to_string deltatime));
	       ("prevledgerroot",JsonStr(hashval_hexstring (ctree_hashroot bhd.prevledger)));
	       ("newledgerroot",JsonStr(hashval_hexstring bhd.newledgerroot));
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
		let jcoinstk = json_tx(coinstake ((bhd,bhs),bd)) in
		let jtxhl =
		  List.map
		    (fun (tau,stau) -> JsonStr(hashval_hexstring(hashstx (tau,stau))))
		    bd.blockdelta_stxl
		in
		let j = JsonObj(("type",JsonStr("block"))::jr @ [("coinstk",jcoinstk);("txs",JsonArr(jtxhl))]) in
		dbentries := j::!dbentries
	      with Not_found ->
		let j = JsonObj(("type",JsonStr("block"))::jr) in
		dbentries := j::!dbentries
	    end
	  with Not_found -> ()
	end;
	begin
	  try
            let d = termaddr_addr (hashval_md160 h) in
            let j = tzp_addr_jsoninfo true d pbh ledgerroot blkh in
	    dbentries := JsonObj([("type",JsonStr("termid"));("termaddress",JsonStr(addr_tzpaddrstr d));("termaddressinfo",j)])::!dbentries
	  with _ -> ()
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
	let d = tzpaddrstr_addr q in
	let j = tzp_addr_jsoninfo false d pbh ledgerroot blkh in
	JsonObj([("response",JsonStr("tzpaddress"));("info",j)])
      with _ ->
	try
	  let b = btcaddrstr_addr q in
	  let j = tzp_addr_jsoninfo false b pbh ledgerroot blkh in
	  let d = addr_tzpaddrstr b in
	  JsonObj([("response",JsonStr("bitcoin address"));("tzpaddress",JsonStr(d));("info",j)])
	with _ ->
	  JsonObj([("response",JsonStr("unknown"));("msg",JsonStr("Cannot interpret as tzp value"))])
    end

let query q =
  match !artificialledgerroot with
  | Some(ledgerroot) ->
      query_at_block q None ledgerroot (-1L)
  | None ->
      let (bn,cwl) = get_bestnode true in
      let BlocktreeNode(_,_,pbh,_,_,ledgerroot,_,_,_,_,blkh,_,_,_) = bn in
      query_at_block q pbh ledgerroot blkh

let query_blockheight findblkh =
  if findblkh < 1L then
    JsonObj([("response",JsonStr("no block at height < 1"))])
  else
    let (bn,cwl) = get_bestnode true in
    let BlocktreeNode(par,_,pbh,_,_,ledgerroot,_,_,_,_,blkh,_,_,_) = bn in
    if findblkh >= blkh then
      JsonObj([("response",JsonStr("no block at height " ^ (Int64.to_string findblkh)))])
    else
      let rec query_blockheight_search par pbhi blkhi =
	if findblkh = Int64.sub blkhi 1L then
	  begin
	    match pbhi with
	    | Some(h) -> query_at_block (hashval_hexstring h) pbh ledgerroot blkh
	    | None -> JsonObj([("response",JsonStr("error"))])
	  end
	else
	  begin
	    match par with
	    | Some(BlocktreeNode(par,_,pbhi,_,_,_,_,_,_,_,blkhi,_,_,_)) ->
		query_blockheight_search par pbhi blkhi
	    | None ->
		JsonObj([("response",JsonStr("failed to find block at height " ^ (Int64.to_string findblkh)))])
	  end
      in
      query_blockheight_search par pbh blkh

let preassetinfo_report oc u =
  match u with
  | Currency(v) ->
      Printf.fprintf oc "Currency: %s prime tezzies (%Ld meuniers)\n" (tezzies_of_meuniers v) v
  | Bounty(v) ->
      Printf.fprintf oc "Bounty: %s prime tezzies (%Ld meuniers)\n" (tezzies_of_meuniers v) v
  | OwnsObj(h,alpha,None) ->
      Printf.fprintf oc "Ownership deed for object with id %s (which must be held at address %s).\n" (hashval_hexstring h) (addr_tzpaddrstr (termaddr_addr (hashval_md160 h)));
      Printf.fprintf oc "Rights to import the object cannot be purchased. It must be redefined in new documents.\n";
  | OwnsObj(h,alpha,Some(r)) ->
      Printf.fprintf oc "Ownership deed for object with id %s (which must be held at address %s).\n" (hashval_hexstring h) (addr_tzpaddrstr (termaddr_addr (hashval_md160 h)));
      if r = 0L then
	Printf.fprintf oc "The object can be freely imported into documents and signatures.\n"
      else
	Printf.fprintf oc "Each right to import the object into a document costs %s prime tezzies (%Ld meuniers), payable to %s.\n" (tezzies_of_meuniers r) r (addr_tzpaddrstr (payaddr_addr alpha))
  | OwnsProp(h,alpha,r) ->
      Printf.fprintf oc "Ownership deed for proposition with id %s (which must be held at address %s).\n" (hashval_hexstring h) (addr_tzpaddrstr (termaddr_addr (hashval_md160 h)));
      Printf.fprintf oc "Rights to import the proposition cannot be purchased. It must be reproven in new documents.\n";
  | OwnsNegProp ->
      Printf.fprintf oc "Ownership deed for negation of proposition, controlled by whomever proved negation of proposition.\n"
  | RightsObj(h,v) ->
      Printf.fprintf oc "%Ld rights to import object with id %s into documents.\n" v (hashval_hexstring h)
  | RightsProp(h,v) ->
      Printf.fprintf oc "%Ld rights to import proposition with id %s into documents.\n" v (hashval_hexstring h)
  | Marker ->
      Printf.fprintf oc "Marker committing to publish a document, theory or signature with fixed contents.\n"
  | DocPublication(alpha,nonce,dl) ->
      Printf.fprintf oc "Document with publisher %s\n"
	(addr_tzpaddrstr (payaddr_addr alpha));
      let dlh = Mathdata.hashdoc dl in
      let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr alpha)) (hashpair nonce dlh)) in
      Printf.fprintf oc "Document must be published to address %s\n" (addr_tzpaddrstr (hashval_pub_addr dlh));
      Printf.fprintf oc "and can only be published by spending a Marker at least 4 blocks old held at %s.\n" (addr_tzpaddrstr beta);
      let usesobjs = Mathdata.doc_uses_objs dl in
      let usesprops = Mathdata.doc_uses_props dl in
      let createsobjs = Mathdata.doc_creates_objs dl in
      let createsprops = Mathdata.doc_creates_props dl in
      let createsnegprops = Mathdata.doc_creates_neg_props dl in
      if not (usesobjs = []) then
	begin
	  Printf.fprintf oc "2*%d rights required to use objects must be consumed:\n" (List.length usesprops);
	  List.iter
	    (fun (h,k) ->
	      Printf.fprintf oc "Right to use (pure) object %s (%s)\n" (hashval_hexstring h) (addr_tzpaddrstr (hashval_term_addr h));
	      let h2 = hashtag (hashpair h k) 32l in
	      Printf.fprintf oc "Right to use (theory) object %s (%s)\n" (hashval_hexstring h2) (addr_tzpaddrstr (hashval_term_addr h2)))
	    usesobjs
	end;
      if not (usesprops = []) then
	begin
	  Printf.fprintf oc "2*%d rights required to use propositions must be consumed:\n" (List.length usesprops);
	  List.iter
	    (fun h ->
	      Printf.fprintf oc "Right to use (pure) proposition %s (%s)\n" (hashval_hexstring h) (addr_tzpaddrstr (hashval_term_addr h));
	      let h2 = hashtag h 33l in
	      Printf.fprintf oc "Right to use (theory) proposition %s (%s)\n" (hashval_hexstring h2) (addr_tzpaddrstr (hashval_term_addr h2)))
	    usesprops
	end;
      if not (createsobjs = []) then
	begin
	  Printf.fprintf oc "%d objects possibly created:\n" (List.length createsobjs);
	  List.iter
	    (fun (h,k) ->
	      Printf.fprintf oc "If there is no owner of (pure) object %s (%s), OwnsObj must be declared.\n" (hashval_hexstring h) (addr_tzpaddrstr (hashval_term_addr h));
	      let h2 = hashtag (hashpair h k) 32l in
	      Printf.fprintf oc "If there is no owner of (theory) object %s (%s), OwnsObj must be declared.\n" (hashval_hexstring h2) (addr_tzpaddrstr (hashval_term_addr h2)))
	    createsobjs
	end;
      if not (createsprops = []) then
	begin
	  Printf.fprintf oc "%d propositions possibly created:\n" (List.length createsprops);
	  List.iter
	    (fun h ->
	      Printf.fprintf oc "If there is no owner of (pure) proposition %s (%s), OwnsProp must be declared.\n" (hashval_hexstring h) (addr_tzpaddrstr (hashval_term_addr h));
	      let h2 = hashtag h 33l in
	      Printf.fprintf oc "If there is no owner of (theory) proposition %s (%s), OwnsProp must be declared.\n" (hashval_hexstring h2) (addr_tzpaddrstr (hashval_term_addr h2)))
	    createsprops
	end;
      if not (createsnegprops = []) then
	begin
	  Printf.fprintf oc "%d negated propositions possibly created:\n" (List.length createsnegprops);
	  List.iter
	    (fun h ->
	      let h2 = hashtag h 33l in
	      Printf.fprintf oc "If there is no OwnsNegProp at %s (for id %s), one must be declared.\n" (addr_tzpaddrstr (hashval_term_addr h2)) (hashval_hexstring h2))
	    createsnegprops
	end

let requestfullledger oc h =
  Printf.fprintf oc "Checking for missing elements of %s to request from peers. This may take several hours.\n" (hashval_hexstring h);
  flush oc;
  let reqh = ref [] in
  let reqc = ref [] in
  let cnt = ref 0 in
  let topcnt = ref 0 in
  let rec requestasset oc h =
    if not (DbAsset.dbexists h) then
      begin
	broadcast_requestdata GetAsset h;
	incr cnt
      end
  in
  let rec requestfullhlist_1 oc h =
    let (k,hr) = DbHConsElt.dbget h in
    requestasset oc k;
    match hr with
    | Some(k,_) -> requestfullhlist oc k
    | _ -> ()
  and requestfullhlist oc h =
    try
      requestfullhlist_1 oc h
    with Not_found ->
      broadcast_requestdata GetHConsElement h;
      incr cnt;
      reqh := h::!reqh
  in
  let rec requestfullctree oc h top =
    try
      let e = DbCTreeElt.dbget h in
      requestfullctree_2 oc e top
    with Not_found ->
      broadcast_requestdata GetCTreeElement h;
      incr cnt;
      reqc := h::!reqc
  and requestfullctree_2 oc c top =
    match c with
    | CHash(h) ->
	if top then
	  begin
	    incr topcnt;
	    if !topcnt mod 6 = 0 then
	      begin
		Printf.fprintf oc "%d%% through tree traversal.\n" (!topcnt * 100 / 512);
		flush oc
	      end;
	  end;
	requestfullctree oc h false
    | CLeaf(_,NehHash(h,_)) -> requestfullhlist oc h
    | CLeaf(_,_) -> Printf.fprintf oc "Bug: Unexpected ctree elt case of nehhlist other than hash"
    | CLeft(c0) -> requestfullctree_2 oc c0 top
    | CRight(c1) -> requestfullctree_2 oc c1 top
    | CBin(c0,c1) -> requestfullctree_2 oc c0 top; requestfullctree_2 oc c1 top
  in
  requestfullctree oc h true;
  if !cnt = 0 then
    Printf.fprintf oc "Verified node already has full ledger.\n"
  else
    begin
      Printf.fprintf oc "Made %d requests on first pass. Will try up to 10 passes to try to get all.\n" !cnt;
      flush oc;
      try
	for i = 2 to 10 do
	  Thread.delay 2.0;
	  Printf.fprintf oc "Beginning Pass %d.\n" i;
	  flush oc;
	  let reqhp = !reqh in
	  let reqcp = !reqc in
	  cnt := 0;
	  reqh := [];
	  reqc := [];
	  List.iter
	    (fun h ->
	      try
		requestfullhlist_1 oc h
	      with Not_found ->
		incr cnt;
		reqh := h::!reqh)
	    reqhp;
	  List.iter
	    (fun h ->
	      try
		requestfullctree_2 oc (DbCTreeElt.dbget h) false
	      with Not_found ->
		incr cnt;
		reqc := h::!reqc)
	    reqcp;
	  if !cnt = 0 then
	    begin
	      Printf.fprintf oc "Ending Pass %d and ledger is complete.\n" i;
	      raise Exit
	    end
	  else
	    Printf.fprintf oc "Ending Pass %d with %d outstanding requests.\n" i !cnt;
	done;
	Printf.fprintf oc "Failed to obtain complete ledger. Try again, possibly after connecting to different peers.\n";
	flush oc
      with Exit ->
	flush oc
    end
