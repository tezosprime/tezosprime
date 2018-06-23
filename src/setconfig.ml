(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Copyright (c) 2018 The Tezos' (Tezos Prime) developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash
open Cryptocurr

let stringconfigvars = [
("seed",fun x -> Config.seed := x);
("lastcheckpoint",fun x -> Config.lastcheckpoint := x);
("ltcblockcheckpoint",fun x -> Config.ltcblockcheckpoint := x);
("prompt",fun x -> Config.prompt := x);
("rpcuser",fun x -> Config.rpcuser := x);
("rpcpass",fun x -> Config.rpcpass := x);
("ltcrpcuser",fun x -> Config.ltcrpcuser := x);
("ltcrpcpass",fun x -> Config.ltcrpcpass := x);
("curl",fun x -> Config.curl := x)
];;
let boolconfigvars = [
("offline",fun x -> Config.offline := x);
("ltcoffline",fun x -> Config.ltcoffline := x);
("daemon",fun x -> Config.daemon := x);
("staking",fun x -> Config.staking := x);
("ipv6",fun x -> Config.ipv6 := x);
("extraindex",fun x -> Config.extraindex := x);
("generatenewrewardaddresses",fun x -> Config.generatenewrewardaddresses := x);
("stakewithrewards",fun x -> Config.stakewithrewards := x);
("offlinestakerewardsdest",fun x -> Config.offlinestakerewardsdest := x)
];;
let intconfigvars = [
("port",fun x -> Config.port := x);
("socksport",fun x -> Config.socksport := x);
("rpcport",fun x -> Config.rpcport := x);
("rpcport",fun x -> Config.rpcport := x);
("ltcrpcport",fun x -> Config.ltcrpcport := x);
("ltcnotifyport",fun x -> Config.ltcnotifyport := x);
("maxconns",fun x -> Config.maxconns := x);
("burnifleq",fun x -> Config.burnifleq := x);
("minconnstostake",fun x -> Config.minconnstostake := x)
];;
let int64configvars = [
("genesistime",fun x -> Config.genesistimestamp := x);
("maxburn",fun x -> Config.maxburn := x);
("maxburnrate",fun x -> Config.maxburnrate := x);
("ltctxfee",fun x -> Config.ltctxfee := x);
("mintimebetweenburns",fun x -> Config.mintimebetweenburns := x);
("reward_lock_relative",fun x -> Config.reward_lock_relative := Some(x));
("reward_lock_absolute",fun x -> Config.reward_lock_absolute := Some(x))
];;
let meuniersoftezziesconfigvars = [
("minrelayfee",fun x -> Config.minrelayfee := x)
];;
let stringoptionconfigvars = [
("ip",fun x -> Config.ip := x);
("randomseed",fun x -> Config.randomseed := x);
("offlinestakerewardslock",fun x -> Config.offlinestakerewardslock := x);
];;
let intoptionconfigvars = [
("socks",fun x -> Config.socks := x)
];;
let stringlistconfigvars = [
("ltcaddress",fun x -> Config.ltcaddresses := x::!Config.ltcaddresses)
];;

exception Done

let setl = ref []

let process_config_line l =
  let ll = String.length l in
  begin
    try
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll >= 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (String.sub l (vl+1) (ll-(vl+1)));
	      raise Done
	    end
	  )
	stringconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if l = v then
	    begin
	      setl := v::!setl;
	      r true;
	      raise Done
	    end
	  else if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    let s = String.sub l (vl+1) (ll-(vl+1)) in
	    begin
	      setl := v::!setl;
	      r (s = "1" || s = "t" || s = "true");
	      raise Done
	    end
	  )
	boolconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (int_of_string (String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	intconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (Int64.of_string (String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	int64configvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (meuniers_of_tezzies (String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	meuniersoftezziesconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (Some(String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	stringoptionconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (Some(int_of_string (String.sub l (vl+1) (ll-(vl+1)))));
	      raise Done
	    end
	  )
	intoptionconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (String.sub l (vl+1) (ll-(vl+1)));
	      raise Done
	    end
	  )
	stringlistconfigvars;
      raise Not_found
    with Done -> ()
  end

let datadir () = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir

let process_config_file () =
  let fn = Filename.concat (datadir()) "tezosprime.conf" in
  if Sys.file_exists fn then
    begin
      let ch = open_in fn in
      try
	while true do
	  let l = input_line ch in
	  try
	    if String.length l > 0 && not (l.[0] = '%') then
	      process_config_line l
	  with Not_found ->
	    Printf.printf "Do not understand %s in tezosprime.conf; skipping\n" l
	done
      with End_of_file -> ()
    end
  else
    Printf.printf "No tezosprime.conf file found. Using default configuration.\n";;

let datadir_from_command_line () =
  let a = Array.length Sys.argv in
  for i = 1 to a-1 do
    let arg = Sys.argv.(i) in
    try
      if String.length arg > 9 && String.sub arg 0 9 = "-datadir=" then
	Config.datadir := String.sub arg 9 (String.length arg - 9);
      if arg = "-testnet" || arg = "-testnet=1" then (*** if testnet, then change some default values ***)
        begin
          Config.testnet := true;
          if not (List.mem "port" !setl) then Config.port := 20804;
          if not (List.mem "seed" !setl) then Config.seed := "05db5c435747921b3fe9d39cbf5089c2918e4055ef11843a8b36eecf2a90f0a2"; (*** litecoin block 1341062 ***)
        end
    with Not_found -> ()
  done;;

exception CreateSnapshot of int;;
exception ImportSnapshot of int;;
exception CheckLedger of int;;
exception BuildExtraIndex of int;;
exception NetLogReport of int;;

let createsnapshot = ref false;;
let importsnapshot = ref false;;
let snapshot_dir = ref None;;
let snapshot_headers = ref [];;
let snapshot_blocks = ref [];;
let snapshot_ledgerroots = ref [];;
let snapshot_full = ref true;;
let snapshot_addresses = ref [];;
let snapshot_shards = ref None;;
let check_ledger = ref None;;
let build_extraindex = ref None;;
let netlogreport = ref None;;

let process_config_args () =
  let a = Array.length Sys.argv in
  try
    for i = 1 to a-1 do
      let arg = Sys.argv.(i) in
      if arg = "-createsnapshot" then
	raise (CreateSnapshot(i))
      else if arg = "-importsnapshot" then
	raise (ImportSnapshot(i))
      else if arg = "-checkledger" then
	raise (CheckLedger(i))
      else if arg = "-buildextraindex" then
	raise (BuildExtraIndex(i))
      else if arg = "-netlogreport" then
	raise (NetLogReport(i))
      else if String.length arg > 1 && arg.[0] = '-' then
	try
	  process_config_line (String.sub arg 1 ((String.length arg) - 1))
	with Not_found -> ()
    done
  with
  | CreateSnapshot(i) -> (*** tezosprime is being started only to take a snapshot of the current state; this can be used to help new people easily bootstrap with partial or full databases; the snapshot requires tezosprime to otherwise not be running so that the database remains fixed during the creation of the snapshot ***)
      createsnapshot := true;
      let ledgerrooteql = String.length "-ledgerroot=" in
      let headereql = String.length "-header=" in
      let blockeql = String.length "-block=" in
      let addresseql = String.length "-address=" in
      let shardeql = String.length "-shard=" in
      if i+1 >= a then
	begin
	  Printf.printf "Expected -createsnapshot <newsnapshotdirectory> [-ledgerroot=<hashval>]* [-block=<hashval>]* [-header=<hashval>]* [-address=<address>]*\n";
	  exit 1
	end;
      snapshot_dir := Some(Sys.argv.(i+1));
      for j = i+2 to a-1 do
	let arg = Sys.argv.(j) in
	let argl = String.length arg in
	if argl > ledgerrooteql && String.sub arg 0 ledgerrooteql = "-ledgerroot=" then
	  begin
	    let hh = String.sub arg ledgerrooteql (argl-ledgerrooteql) in
	    try
	      let h = hexstring_hashval hh in
	      snapshot_ledgerroots := h::!snapshot_ledgerroots
	    with _ ->
	      Printf.printf "Could not understand %s as a ledgerroot\n" hh;
	      exit 1
	  end
	else if argl > headereql && String.sub arg 0 headereql = "-header=" then
	  begin
	    let hh = String.sub arg headereql (argl-headereql) in
	    try
	      let h = hexstring_hashval hh in
	      snapshot_headers := h::!snapshot_headers
	    with _ ->
	      Printf.printf "Could not understand %s as a header\n" hh;
	      exit 1
	  end
	else if argl > blockeql && String.sub arg 0 blockeql = "-block=" then
	  begin
	    let hh = String.sub arg blockeql (argl-blockeql) in
	    try
	      let h = hexstring_hashval hh in
	      snapshot_blocks := h::!snapshot_blocks
	    with _ ->
	      Printf.printf "Could not understand %s as a block\n" hh;
	      exit 1
	  end
	else if argl > addresseql && String.sub arg 0 addresseql = "-address=" then
	  begin
	    snapshot_full := false; (*** if at least one specific address to support is given, then assume a partial snapshot of the ledger is desired ***)
	    let a = String.sub arg addresseql (argl-addresseql) in
	    try
	      let alpha =
		if String.length a > 0 && (a.[0] = '1' || a.[0] = '3') then
		  btcaddrstr_addr a
		else
		  tzpaddrstr_addr a
	      in
	      snapshot_addresses := alpha::!snapshot_addresses
	    with _ ->
	      Printf.printf "Could not understand %s as an address\n" a;
	      exit 1
	  end
	else if argl > shardeql && String.sub arg 0 shardeql = "-shard=" then
	  begin
	    let s = String.sub arg shardeql (argl-shardeql) in
	    try
	      let i = int_of_string s in
	      if i < 0 || i > 511 then raise Exit;
	      match !snapshot_shards with
	      | None -> snapshot_shards := Some([i])
	      | Some(il) -> if not (List.mem i il) then snapshot_shards := Some(i::il)
	    with _ ->
	      Printf.printf "Could not understand %s as an shard (int in [0,511])\n" s;
	      exit 1
	  end
	else
	  begin
	    Printf.printf "Could not understand %s\n" arg;
	    exit 1
	  end
      done
  | ImportSnapshot(i) -> (*** tezosprime is being started only to import a snapshot into the local database ***)
      importsnapshot := true;
      if not (i = a-2) then
	begin
	  Printf.printf "Expected -importsnapshot <snapshotdirectory>\n";
	  exit 1
	end;
      snapshot_dir := Some(Sys.argv.(i+1))
  | CheckLedger(i) ->
      if not (i = a-2) then
	begin
	  Printf.printf "Expected -checkledger <ledgerroot>\n";
	  exit 1
	end;
      check_ledger := Some(hexstring_hashval Sys.argv.(i+1))
  | BuildExtraIndex(i) ->
      if not (i = a-2) then
	begin
	  Printf.printf "Expected -buildextraindex <ledgerroot>\n";
	  exit 1
	end;
      build_extraindex := Some(hexstring_hashval Sys.argv.(i+1))
  | NetLogReport(i) -> (*** tezosprime is being started only to generate a readable report of network activity from reclog* files and sentlog file ***)
      let fl = ref [] in
      for j = a-1 downto i+1 do
	fl := Sys.argv.(j) :: !fl
      done;
      netlogreport := Some(!fl)
      
