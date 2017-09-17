(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Hashaux
open Hash
open Json

let json_assoc_string k al =
  match List.assoc k al with
  | JsonStr(x) -> x
  | _ -> raise Not_found

let json_assoc_int64 k al =
  match List.assoc k al with
  | JsonNum(x) -> Int64.of_string x
  | _ -> raise Not_found

let json_assoc_int k al =
  match List.assoc k al with
  | JsonNum(x) -> int_of_string x
  | _ -> raise Not_found

let litecoins_of_litoshis v =
  let w = Int64.div v 100000000L in
  let d = Int64.to_string (Int64.rem v 100000000L) in
  let dl = String.length d in
  let ez = ref 0 in
  begin
    try
      for i = dl-1 downto 0 do
	if d.[i] = '0' then
	  incr ez
	else
	  raise Exit
      done
    with Exit -> ()
  end;
  let b = Buffer.create 20 in
  Buffer.add_string b (Int64.to_string w);
  Buffer.add_char b '.';
  for i = 1 to 11 - dl do
    Buffer.add_char b '0'
  done;
  for i = 0 to dl - (1 + !ez) do
    Buffer.add_char b d.[i]
  done;
  Buffer.contents b

let litoshis_of_litecoins s =
  let f = ref 0L in
  let w = ref true in
  let c = ref 0L in
  let d = ref 10000000L in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let cc = Char.code s.[!i] in
    incr i;
    if !w then
      if cc = 46 then
	w := false
      else if cc >= 48 && cc < 58 then
	f := Int64.add (Int64.mul !f 10L) (Int64.of_int (cc-48))
      else
	raise (Failure ("cannot interpret " ^ s ^ " as a number of litecoins"))
    else
      if cc >= 48 && cc < 58 then
	begin
	  c := Int64.add !c (Int64.mul !d (Int64.of_int (cc-48)));
	  d := Int64.div !d 10L
	end
      else
	raise (Failure ("cannot interpret " ^ s ^ " as a number of litecoins"))
  done;
  Int64.add (Int64.mul !f 100000000L) !c

let json_assoc_litoshis k al =
  match List.assoc k al with
  | JsonNum(x) -> litoshis_of_litecoins x
  | _ -> raise Not_found

let ltc_getbestblockhash () =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"gbbh\", \"method\": \"getbestblockhash\", \"params\": [] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) -> json_assoc_string "result" al
    | _ -> raise Not_found
  with _ ->
    raise Not_found

let dalilcoin_candidate_p h =
  String.length h = 64 && h.[0] = '4' && h.[1] = '4' && h.[2] = '6' && h.[3] = '1'

let ltc_getblock h =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"gb\", \"method\": \"getblock\", \"params\": [\"" ^ h ^ "\"] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonObj(bl) ->
	      begin
		let pbh = json_assoc_string "previousblockhash" bl in
		let tm = json_assoc_int64 "time" bl in
		let txl = ref [] in
		match List.assoc "tx" bl with
		| JsonArr(txs) ->
		    begin
		      List.iter
			(fun jtxh ->
			  match jtxh with
			  | JsonStr(txh) when dalilcoin_candidate_p txh -> txl := txh::!txl
			  | _ -> ())
			txs;
		      (pbh,tm,!txl)
		    end
		| _ -> raise Not_found
	      end
	  | _ -> raise Not_found
	end
    | _ -> raise Not_found
  with _ ->
    raise Not_found

let ltc_listunspent () =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let addrl = Buffer.create 40 in
    let fstaddr = ref true in
    List.iter
      (fun a ->
	if !fstaddr then fstaddr := false else Buffer.add_char addrl ',';
	Buffer.add_char addrl '"';
	Buffer.add_string addrl a;
	Buffer.add_char addrl '"')
      !Config.ltcaddresses;
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"lu\", \"method\": \"listunspent\", \"params\": [1,9999999,[" ^ (Buffer.contents addrl) ^ "]] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    let utxol = ref [] in
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonArr(ul) ->
	      begin
		List.iter
		  (fun u ->
		    match u with
		    | JsonObj(bl) ->
			begin
			  try
			    let txh = json_assoc_string "txid" bl in
			    let vout = json_assoc_int "vout" bl in
			    let rs = json_assoc_string "redeemScript" bl in
			    let spk = json_assoc_string "scriptPubKey" bl in
			    let amt = json_assoc_litoshis "amount" bl in
			    utxol := (txh,vout,rs,spk,amt)::!utxol
			  with Not_found -> ()
			end
		    | _ -> ())
		  ul;
		!utxol
	      end
	  | _ -> raise Not_found
	end
    | _ -> raise Not_found
  with _ ->
    raise Not_found

exception InsufficientLtcFunds

let le_num24 x =
  let strb = Buffer.create 3 in
  Buffer.add_char strb (Char.chr (x land 255));
  Buffer.add_char strb (Char.chr ((x lsr 8) land 255));
  Buffer.add_char strb (Char.chr ((x lsr 16) land 255));
  Buffer.contents strb

let finddatx txs1 txs2 =
  let i = ref (-1) in
  let rtxid = ref (0l,0l,0l,0l,0l,0l,0l,0l) in
  let txs = ref "" in
  let daid ri =
    let (_,_,_,_,_,_,_,x) = ri in
    Int32.logand x 0xffffl = 0x6144l
  in
  while not (daid !rtxid) do
    incr i;
    if !i >= 16777216 then raise Not_found; (** probably will never happen **)
    txs := txs1 ^ (le_num24 !i) ^ txs2;
    rtxid := Sha256.sha256dstr !txs
  done;
  (!i,!rtxid,!txs);;

let blnum32 x =
  [Int32.to_int (Int32.logand x 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 8) 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 16) 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 24) 255l)]

let blnum64 x =
  [Int64.to_int (Int64.logand x 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 8) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 16) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 24) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 32) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 40) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 48) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 56) 255L)]

let ltc_createburntx h1 h2 toburn =
  let utxol = ltc_listunspent () in
  let toburn_plus_fee = Int64.add toburn !Config.ltctxfee in
  try
    let (txid,vout,rs,spk,amt) = List.find (fun (_,_,_,_,amt) -> amt > toburn_plus_fee) utxol in (*** only consider single spends ***)
    let txs1b = Buffer.create 100 in
    let txs2b = Buffer.create 100 in
    let txs3b = Buffer.create 100 in
    Buffer.add_string txs1b "\001"; (*** assume one input ***)
    let txidrh = hashval_rev (hexstring_hashval txid) in
    ignore (seo_hashval seosb txidrh (txs1b,None));
    List.iter (fun z -> Buffer.add_char txs1b (Char.chr z)) (blnum32 (Int32.of_int vout));
    let txs1 = Buffer.contents txs1b in
    Buffer.add_char txs1b '\023';
    Buffer.add_char txs1b '\022';
    Buffer.add_string txs1b (hexstring_string rs);
    Buffer.add_string txs2b "\255\255\255\255\002";
    List.iter (fun z -> Buffer.add_char txs2b (Char.chr z)) (blnum64 toburn);
    Buffer.add_char txs2b (Char.chr 69);
    Buffer.add_char txs2b (Char.chr 0x6a); (*** OP_RETURN ***)
    Buffer.add_char txs2b (Char.chr 67); (*** PUSH 67 ***)
    ignore (seo_hashval seosb h1 (txs2b,None));
    ignore (seo_hashval seosb h2 (txs2b,None));
    List.iter (fun z -> Buffer.add_char txs3b (Char.chr z)) (blnum64 (Int64.sub amt toburn_plus_fee));
    let spks = hexstring_string spk in
    Buffer.add_char txs3b (Char.chr (String.length spks));
    Buffer.add_string txs3b spks;
    Buffer.add_string txs3b "\000\000\000\000"; (*** locktime ***)
    let txs2 = Buffer.contents txs2b in
    let txs3 = Buffer.contents txs3b in
    let (i,rtxid,txs) = finddatx ("\002\000\000\000" ^ (Buffer.contents txs1b) ^ txs2) txs3 in
    let txsb = Buffer.create 100 in
    Buffer.add_string txsb "\002\000\000\000";
    Buffer.add_string txsb txs1;
    Buffer.add_string txsb "\000";
    Buffer.add_string txsb txs2;
    Buffer.add_string txsb (le_num24 i);
    Buffer.add_string txsb txs3;
    Buffer.contents txsb
  with Not_found -> raise InsufficientLtcFunds

let ltc_signrawtransaction txs =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"signrawtransaction\", \"params\": [\"" ^ txs ^ "\"] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin 
	  match List.assoc "result" al with
	  | JsonObj(bl) -> json_assoc_string "hex" bl
	  | _ -> raise Not_found
	end
    | _ -> raise Not_found
  with _ -> raise Not_found

let ltc_sendrawtransaction txs =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"sendrawtransaction\", \"params\": [\"" ^ txs ^ "\"] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) -> json_assoc_string "result" al
    | _ -> raise Not_found
  with _ -> raise Not_found

let ltc_gettransactioninfo h =
  try
    let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
    let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"grtx\", \"method\": \"getrawtransaction\", \"params\": [\"" ^ h ^ "\",1] }'" in
    let url = "http://127.0.0.1:" ^ (string_of_int !Config.ltcrpcport) ^ "/" in
    let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
    let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
    let l = input_line inc in
    ignore (Unix.close_process_full (inc,outc,errc));
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonObj(bl) ->
	      begin
		match List.assoc "vout" bl with
		| JsonArr(JsonObj(vout1)::_) ->
		    let litoshisburned = json_assoc_litoshis "value" vout1 in
		    begin
		      match List.assoc "scriptPubKey" vout1 with
		      | JsonObj(cl) ->
			  let hex = json_assoc_string "hex" cl in
			  if String.length hex >= 132 && hex.[0] = '6' && hex.[1] = 'a' then
			    (litoshisburned,hexstring_hashval (String.sub hex 4 64),hexstring_hashval (String.sub hex 68 64))
			  else
			    raise Not_found
		      | _ -> raise Not_found
		    end
		| _ -> raise Not_found
	      end
	  | _ -> raise Not_found
	end
    | _ -> raise Not_found
  with _ -> raise Not_found
