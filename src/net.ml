(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Utils
open Ser
open Hashaux
open Sha256
open Hash

let shutdown_close s =
  try
    Unix.shutdown s Unix.SHUTDOWN_ALL;
    Unix.close s
  with _ ->
    try
      Unix.close s
    with _ -> ()

let missingheaders = ref [];;

let netblkh : int64 ref = ref 0L

type msgtype =
  | Version
  | Verack
  | Addr
  | Inv
  | GetSTx
  | GetHeaders
  | GetHeader
  | GetBlock
  | GetBlockdelta
  | STx
  | Block
  | Headers
  | Blockdelta
  | GetAddr
  | Alert
  | Ping
  | Pong
  | GetCTreeElement
  | GetHConsElement
  | GetAsset
  | CTreeElement
  | HConsElement
  | Asset

let msgtype_of_int i =
  try
    List.nth
      [Version;Verack;Addr;Inv;GetSTx;GetHeaders;GetHeader;GetBlock;GetBlockdelta;
       STx;Block;Headers;Blockdelta;GetAddr;Alert;Ping;Pong;
       GetCTreeElement;GetHConsElement;GetAsset;CTreeElement;HConsElement;Asset]
      i
  with Failure("nth") -> raise Not_found

let int_of_msgtype mt =
  match mt with
  | Version -> 0
  | Verack -> 1
  | Addr -> 2
  | Inv -> 3
  | GetSTx -> 4
  | GetHeaders -> 5
  | GetHeader -> 6
  | GetBlock -> 7
  | GetBlockdelta -> 8
  | STx -> 9
  | Block -> 10
  | Headers -> 11
  | Blockdelta -> 12
  | GetAddr -> 13
  | Alert -> 14
  | Ping -> 15
  | Pong -> 16
  | GetCTreeElement -> 17
  | GetHConsElement -> 18
  | GetAsset -> 19
  | CTreeElement -> 20
  | HConsElement -> 21
  | Asset -> 22

let inv_of_msgtype mt =
  try
    int_of_msgtype
      (match mt with
      | GetSTx -> STx
      | GetBlock -> Block
      | GetHeaders -> Headers
      | GetHeader -> Headers
      | GetBlockdelta -> Blockdelta
      | GetCTreeElement -> CTreeElement
      | GetHConsElement -> HConsElement
      | GetAsset -> Asset
      | _ -> raise Not_found)
  with Not_found -> (-1)

let string_of_msgtype mt =
  match mt with
  | Version -> "Version"
  | Verack -> "Verack"
  | Addr -> "Addr"
  | Inv -> "Inv"
  | GetSTx -> "GetSTx"
  | GetHeaders -> "GetHeaders"
  | GetHeader -> "GetHeader"
  | GetBlock -> "GetBlock"
  | GetBlockdelta -> "GetBlockdelta"
  | STx -> "STx"
  | Block -> "Block"
  | Headers -> "Headers"
  | Blockdelta -> "Blockdelta"
  | GetAddr -> "GetAddr"
  | Alert -> "Alert"
  | Ping -> "Ping"
  | Pong -> "Pong"
  | GetCTreeElement -> "GetCTreeElement"
  | GetHConsElement -> "GetHConsElement"
  | GetAsset -> "GetAsset"
  | CTreeElement -> "CTreeElement"
  | HConsElement -> "HConsElement"
  | Asset -> "Asset"

let myaddr () =
  match !Config.ip with
  | Some(ip) -> 
      if !Config.ipv6 then
	"[" ^ ip ^ "]:" ^ (string_of_int !Config.port)
      else
	ip ^ ":" ^ (string_of_int !Config.port)
  | None ->
      ""

let fallbacknodes = [
"87.121.52.180:20805";
"87.121.52.180:20815";
"87.121.52.180:20825"
(* ":20805" *)
]

let testnetfallbacknodes = [
"87.121.52.180:20804";
"87.121.52.180:20814";
"87.121.52.180:20824"
(* ":20804" *)
]

let getfallbacknodes () =
  if !Config.testnet then
    testnetfallbacknodes
  else
    fallbacknodes

exception BannedPeer
let bannedpeers : (string,unit) Hashtbl.t = Hashtbl.create 1000
let banpeer n = Hashtbl.add bannedpeers n ()
let clearbanned () = Hashtbl.clear bannedpeers

let knownpeers : (string,int64) Hashtbl.t = Hashtbl.create 1000
let newpeers : string list ref = ref []

let addknownpeer lasttm n =
  if not (n = "") && not (n = myaddr()) && not (List.mem n (getfallbacknodes())) && not (Hashtbl.mem bannedpeers n) then
    try
      let oldtm = Hashtbl.find knownpeers n in
      Hashtbl.replace knownpeers n lasttm
    with Not_found ->
      Hashtbl.add knownpeers n lasttm;
      let peerfn = Filename.concat (if !Config.testnet then Filename.concat !Config.datadir "testnet" else !Config.datadir) "peers" in
      if Sys.file_exists peerfn then
	let s = open_out_gen [Open_append;Open_wronly] 0o644 peerfn in
	output_string s n;
	output_char s '\n';
	output_string s (Int64.to_string lasttm);
	output_char s '\n';
	close_out s
      else
	let s = open_out peerfn in
	output_string s n;
	output_char s '\n';
	output_string s (Int64.to_string lasttm);
	output_char s '\n';
	close_out s

let removeknownpeer n =
  if not (n = "") && not (n = myaddr()) && not (List.mem n (getfallbacknodes())) then
    Hashtbl.remove knownpeers n

let getknownpeers () =
  let cnt = ref 0 in
  let peers = ref [] in
  let currtm = Int64.of_float (Unix.time()) in
  Hashtbl.iter (fun n lasttm -> if !cnt < 1000 && Int64.sub currtm lasttm < 604800L then (incr cnt; peers := n::!peers)) knownpeers;
  !peers

let loadknownpeers () =
  let currtm = Int64.of_float (Unix.time()) in
  let peerfn = Filename.concat (if !Config.testnet then Filename.concat !Config.datadir "testnet" else !Config.datadir) "peers" in
  if Sys.file_exists peerfn then
    let s = open_in peerfn in
    try
      while true do
	let n = input_line s in
	let lasttm = Int64.of_string (input_line s) in
	if Int64.sub currtm lasttm < 604800L then
	  Hashtbl.add knownpeers n lasttm
      done
    with End_of_file -> ()

let saveknownpeers () =
  let peerfn = Filename.concat (if !Config.testnet then Filename.concat !Config.datadir "testnet" else !Config.datadir) "peers" in
  let s = open_out peerfn in
  Hashtbl.iter
    (fun n lasttm ->
      output_string s n;
      output_char s '\n';
      output_string s (Int64.to_string lasttm);
      output_char s '\n')
    knownpeers;
  close_out s

exception GettingRemoteData
exception RequestRejected
exception IllformedMsg
exception ProtocolViolation of string
exception SelfConnection
exception DupConnection

let openlistener ip port numconns =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let ia = Unix.inet_addr_of_string ip in
  Unix.bind s (Unix.ADDR_INET(ia, port));
  Unix.listen s numconns;
  s

let connectpeer ip port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let ia = Unix.inet_addr_of_string ip in
  Unix.connect s (Unix.ADDR_INET(ia, port));
  s

let extract_ipv4 ip =
  let x = Array.make 4 0 in
  let j = ref 0 in
  for i = 0 to String.length ip - 1 do
    let c = Char.code ip.[i] in
    if c = 46 && !j < 3 then
      incr j
    else if c >= 48 && c < 58 then
      x.(!j) <- x.(!j) * 10 + (c-48)
    else
      raise (Failure "Not an ipv4 address")
  done;
  (x.(0),x.(1),x.(2),x.(3))

let rec extract_ipv4_and_port ipp i l =
  if i+2 < l then
    if ipp.[i] = ':' then
      (String.sub ipp 0 i,int_of_string (String.sub ipp (i+1) (l-(i+1))))
    else
      extract_ipv4_and_port ipp (i+1) l
  else
    raise (Failure "not an ipv4 address with a port number")

let rec extract_ipv6_and_port ipp i l =
  if i+3 < l then
    if ipp.[i] = ']' then
      if ipp.[i+1] = ':' then
	(String.sub ipp 0 i,int_of_string (String.sub ipp (i+2) (l-(i+2))))
      else
	raise (Failure "not an ipv4 address with a port number")
    else
      extract_ipv6_and_port ipp (i+1) l
  else
    raise (Failure "not an ipv6 address with a port number")

let extract_ip_and_port ipp =
  let l = String.length ipp in
  if l = 0 then
    raise (Failure "Not an ip address with a port number")
  else if ipp.[0] = '[' then
    let (ip,port) = extract_ipv6_and_port ipp 1 l in
    (ip,port,true)
  else
    let (ip,port) = extract_ipv4_and_port ipp 0 l in
    (ip,port,false)

let connectpeer_socks4 proxyport ip port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect s (Unix.ADDR_INET(Unix.inet_addr_loopback, proxyport));
  let sin = Unix.in_channel_of_descr s in
  let sout = Unix.out_channel_of_descr s in
  set_binary_mode_in sin true;
  set_binary_mode_out sout true;
  output_byte sout 4;
  output_byte sout 1;
  (** port, big endian **)
  output_byte sout ((port asr 8) land 255);
  output_byte sout (port land 255);
  (** ip **)
  let (x0,x1,x2,x3) = extract_ipv4 ip in
  output_byte sout x0;
  output_byte sout x1;
  output_byte sout x2;
  output_byte sout x3;
  output_byte sout 0;
  flush sout;
  let z = input_byte sin in
  let cd = input_byte sin in
  if not (cd = 90) then raise RequestRejected;
  for i = 1 to 6 do
    ignore (input_byte sin)
  done;
  (s,sin,sout)

type connstate = {
    conntime : float;
    realaddr : string;
    connmutex : Mutex.t;
    sendqueue : (hashval * hashval option * msgtype * string) Queue.t;
    sendqueuenonempty : Condition.t;
    mutable nonce : int64 option;
    mutable handshakestep : int;
    mutable peertimeskew : int;
    mutable protvers : int32;
    mutable useragent : string;
    mutable addrfrom : string;
    mutable banned : bool;
    mutable lastmsgtm : float;
    mutable pending : (hashval * (bool * float * float * (msgtype * string -> unit))) list;
    mutable sentinv : (int * hashval * float) list;
    mutable rinv : (int * hashval) list;
    mutable invreq : (int * hashval * float) list;
    mutable first_header_height : int64; (*** how much header history is stored at the node ***)
    mutable first_full_height : int64; (*** how much block/ctree history is stored at the node ***)
    mutable last_height : int64; (*** how up to date the node is ***)
  }

let send_inv_fn : (int -> out_channel -> connstate -> unit) ref = ref (fun _ _ _ -> ())
let msgtype_handler : (msgtype,in_channel * out_channel * connstate * string -> unit) Hashtbl.t = Hashtbl.create 50

let send_msg_real c mh replyto mt ms =
  let magic = if !Config.testnet then 0x54657a54l else 0x54657a4dl in (*** Magic Number for testnet: DalT and for mainnet: DalM ***)
  let msl = String.length ms in
  seocf (seo_int32 seoc magic (c,None));
  begin
    match replyto with
    | None ->
	output_byte c 0
    | Some(h) ->
	output_byte c 1;
	seocf (seo_hashval seoc h (c,None))
  end;
  output_byte c (int_of_msgtype mt);
  seocf (seo_int32 seoc (Int32.of_int msl) (c,None));
  seocf (seo_hashval seoc mh (c,None));
  for j = 0 to msl-1 do
    output_byte c (Char.code ms.[j])
  done;
  flush c

let send_msg c mh replyto mt ms =
  send_msg_real c mh replyto mt ms;
  let f = open_out_gen [Open_wronly;Open_creat;Open_append] 0o644 (!Config.datadir ^ (if !Config.testnet then "/testnet/sentlog" else "/sentlog")) in
  seocf (seo_int64 seoc (Int64.of_float (Unix.time())) (f,None));
  send_msg_real f mh replyto mt ms;
  close_out f

let queue_msg_real cs replyto mt m =
  let mh = sha256str m in
  Mutex.lock cs.connmutex;
  Queue.add (mh,replyto,mt,m) cs.sendqueue;
  Mutex.unlock cs.connmutex;
  Condition.signal cs.sendqueuenonempty;
  mh

let queue_msg cs mt m = queue_msg_real cs None mt m
let queue_reply cs h mt m = queue_msg_real cs (Some(h)) mt m

(***
 Throw IllformedMsg if something's wrong with the format or if it reads the first byte but times out before reading the full message.
 If IllformedMsg is thrown, the connection should be severed.
 ***)
let rec_msg blkh c =
  let (mag0,mag1,mag2,mag3) = if !Config.testnet then (0x54,0x65,0x7a,0x54) else (0x54,0x65,0x7a,0x4d) in
  let by0 = input_byte c in
  if not (by0 = mag0) then raise IllformedMsg;
  try
    let by1 = input_byte c in
    if not (by1 = mag1) then raise IllformedMsg;
    let by2 = input_byte c in
    if not (by2 = mag2) then raise IllformedMsg;
    let by3 = input_byte c in
    if not (by3 = mag3) then raise IllformedMsg;
    let replyto =
      let by4 = input_byte c in
      if by4 = 0 then (*** not a reply ***)
	None
      else if by4 = 1 then
	let (h,_) = sei_hashval seic (c,None) in
	(Some(h))
      else
	raise IllformedMsg
    in
    let mt =
      try
	msgtype_of_int (input_byte c)
      with Not_found -> raise IllformedMsg
    in
    let (msl,_) = sei_int32 seic (c,None) in
    if msl > Int32.of_int (maxblockdeltasize blkh) then raise IllformedMsg;
    let msl = Int32.to_int msl in
    let (mh,_) = sei_hashval seic (c,None) in
    let sb = Buffer.create msl in
    for j = 0 to msl-1 do
      let by = input_byte c in
      Buffer.add_char sb (Char.chr by)
    done;
    let ms = Buffer.contents sb in
    if not (mh = sha256str ms) then raise IllformedMsg;
    (replyto,mh,mt,ms)
  with
  | _ -> (*** consider it an IllformedMsg no matter what the exception raised was ***)
      raise IllformedMsg

let netlistenerth : Thread.t option ref = ref None
let netseekerth : Thread.t option ref = ref None
let netconns : (Thread.t * Thread.t * (Unix.file_descr * in_channel * out_channel * connstate option ref)) list ref = ref []
let netconnsmutex : Mutex.t = Mutex.create()
let this_nodes_nonce = ref 0L

let peeraddr gcs =
  match gcs with
  | Some(cs) -> cs.addrfrom
  | None -> "[dead]"

let log_msg m =
  let h = string_hexstring m in
  log_string (Printf.sprintf "\nmsg: %s\n" h)

let network_time () =
  let mytm = Int64.of_float (Unix.time()) in
  let offsets = ref [] in
  List.iter (fun (_,_,(_,_,_,gcs)) -> match !gcs with Some(cs) -> offsets := List.merge compare [cs.peertimeskew] !offsets | None -> ()) !netconns;
  if !offsets = [] then
    (mytm,0)
  else
    let m = (List.length !offsets) lsr 1 in
    let mskew = List.nth !offsets m in
    (Int64.add mytm (Int64.of_int mskew),mskew)

let handle_msg replyto mt sin sout cs mh m =
  match replyto with
  | Some(h) ->
      begin
	try
	  let (b,tm1,tm2,f) = List.assoc h cs.pending in
	  cs.pending <- List.filter (fun (k,_) -> not (h = k)) cs.pending;
	  f(mt,m)
	with Not_found -> () (*** Reply to unknown request, ignore for now ***)
      end
  | None ->
      if cs.handshakestep < 5 then
	begin
	  match mt with
	  | Version ->
	      let (((vers,srvs,tm,addr_recv,addr_from,n),(ua,fhh,ffh,lh,relay,lastchkpt)),_) =
		sei_prod
		  (sei_prod6 sei_int32 sei_int64 sei_int64 sei_string sei_string sei_int64)
		  (sei_prod6 sei_string sei_int64 sei_int64 sei_int64 sei_bool (sei_option (sei_prod sei_int64 sei_hashval)))
		  seis (m,String.length m,None,0,0)
	      in
	      begin
		if n = !this_nodes_nonce then
		  raise SelfConnection
		else if (try ignore (List.find (fun (_,_,(_,_,_,gcs)) -> match !gcs with Some(cs) -> cs.nonce = Some(n) | None -> false) !netconns); true with Not_found -> false) then
		  raise DupConnection
		else
		  cs.nonce = Some(n); (** remember the nonce to prevent duplicate connections to the same node **)
		  let minvers = if vers > Version.protocolversion then Version.protocolversion else 0l in
		  let mytm = Int64.of_float (Unix.time()) in
		  let tmskew = Int64.sub tm mytm in
		  if tmskew > 7200L then
		    raise (ProtocolViolation("Peer rejected due to excessive time skew"))
		  else
		    let tmskew = Int64.to_int tmskew in
		    if cs.handshakestep = 1 then
		      begin
			ignore (queue_msg cs Verack "");
			let vm = Buffer.create 100 in
			seosbf
			  (seo_prod
			     (seo_prod6 seo_int32 seo_int64 seo_int64 seo_string seo_string seo_int64)
			     (seo_prod6 seo_string seo_int64 seo_int64 seo_int64 seo_bool (seo_option (seo_prod seo_int64 seo_hashval)))
			     seosb
			     ((minvers,0L,mytm,addr_from,myaddr(),!this_nodes_nonce),
			      (Version.useragent,0L,0L,0L,true,None))
			     (vm,None));
			queue_msg cs Version (Buffer.contents vm);
			cs.handshakestep <- 3;
			cs.peertimeskew <- tmskew;
			cs.useragent <- ua;
			cs.protvers <- minvers;
			cs.addrfrom <- addr_from;
			cs.first_header_height <- fhh;
			cs.first_full_height <- ffh;
			cs.last_height <- lh
		      end
		    else if cs.handshakestep = 4 then
		      begin
			queue_msg cs Verack "";
			cs.handshakestep <- 5;
			cs.peertimeskew <- tmskew;
			cs.useragent <- ua;
			cs.protvers <- minvers;
			cs.addrfrom <- addr_from;
			cs.first_header_height <- fhh;
			cs.first_full_height <- ffh;
			cs.last_height <- lh;
			addknownpeer mytm addr_from;
			!send_inv_fn 5000 sout cs
		      end
		    else
		      raise (ProtocolViolation "Handshake failed")
	      end
	  | Verack ->
	      begin
		if cs.handshakestep = 2 then
		  cs.handshakestep <- 4
		else if cs.handshakestep = 3 then
		  begin
		    let mytm = Int64.of_float (Unix.time()) in
		    cs.handshakestep <- 5;
		    addknownpeer mytm cs.addrfrom;
		    !send_inv_fn 5000 sout cs
		  end
		else
		  raise (ProtocolViolation("Unexpected Verack"))
	      end
	  | _ -> raise (ProtocolViolation "Handshake failed")
	end
      else
      try
	let f = Hashtbl.find msgtype_handler mt in
	try
	  f(sin,sout,cs,m)
	with e -> log_string (Printf.sprintf "Call to handler for message type %s raised %s\n" (string_of_msgtype mt) (Printexc.to_string e))
      with Not_found ->
	match mt with
	| Version -> raise (ProtocolViolation "Version message after handshake")
	| Verack -> raise (ProtocolViolation "Verack message after handshake")
	| _ -> raise (Failure ("No handler found for message type " ^ (string_of_msgtype mt)))

let connlistener (s,sin,sout,gcs) =
  try
    while true do
      try
	let (replyto,mh,mt,m) = rec_msg !netblkh sin in
	match !gcs with
	| Some(cs) ->
	    let tm = Unix.time() in
(*	    log_string (Printf.sprintf "got msg %s from %s at time %f\n" (string_of_msgtype mt) cs.realaddr tm); *)
            let f = open_out_gen [Open_wronly;Open_creat;Open_append] 0o644
                            (!Config.datadir ^ (if !Config.testnet then "/testnet/reclog_" else "/reclog_") ^ (string_hexstring cs.addrfrom)) in
            output_value f tm;
            output_value f (replyto,mh,mt,m);
            close_out f;
	    cs.lastmsgtm <- tm;
	    if Hashtbl.mem knownpeers cs.addrfrom then Hashtbl.replace knownpeers cs.addrfrom (Int64.of_float tm);
	    handle_msg replyto mt sin sout cs mh m;
	    if cs.banned then raise (ProtocolViolation("banned"))
	| None -> raise End_of_file (*** connection died; this probably shouldn't happen, as we should have left this thread when it died ***)
      with
      | Unix.Unix_error(c,x,y) -> (*** close connection ***)
	  log_string (Printf.sprintf "Unix error exception raised in connection listener for %s:\n%s %s %s\nClosing connection\n" (peeraddr !gcs) (Unix.error_message c) x y);
	  shutdown_close s;
	  close_in sin;
	  close_out sout;
	  raise Exit
      | End_of_file -> (*** close connection ***)
	  log_string (Printf.sprintf "Channel for connection %s raised End_of_file. Closing connection\n" (peeraddr !gcs));
	  shutdown_close s;
	  close_in sin;
	  close_out sout;
	  raise Exit
      | ProtocolViolation(x) -> (*** close connection ***)
	  log_string (Printf.sprintf "Protocol violation by connection %s: %s\nClosing connection\n" (peeraddr !gcs) x);
	  shutdown_close s;
	  close_in sin;
	  close_out sout;
	  raise Exit
      | SelfConnection -> (*** detected a self-connection attempt, close ***)
	  log_string (Printf.sprintf "Stopping potential self-connection\n");
	  shutdown_close s;
	  close_in sin;
	  close_out sout;
	  raise Exit
      | DupConnection -> (*** detected a duplicate connection attempt, close ***)
	  log_string (Printf.sprintf "Stopping potential duplicate connection\n");
	  shutdown_close s;
	  close_in sin;
	  close_out sout;
	  raise Exit
      | exc -> (*** report but ignore all other exceptions ***)
	  log_string (Printf.sprintf "Ignoring exception raised in connection listener for %s:\n%s\n" (peeraddr !gcs) (Printexc.to_string exc))
    done
  with _ -> gcs := None (*** indicate that the connection is dead; it will be removed from netaddr by the netlistener or netseeker ***)

let connsender (s,sin,sout,gcs) =
  match !gcs with
  | None ->
      log_string (Printf.sprintf "connsender was called without gcs being set to a connection state already.\nThis should never happen.\nKilling connection immediately.\n");
      shutdown_close s
  | Some(cs) ->
      let connsender_end () =
	Mutex.unlock cs.connmutex;
	gcs := None;
	shutdown_close s
      in
      try
	Mutex.lock cs.connmutex;
	while true do
	  try
	    while true do
	      let (mh,replyto,mt,m) = Queue.take cs.sendqueue in
	      send_msg sout mh replyto mt m
	    done;
	  with
	  | Queue.Empty -> Condition.wait cs.sendqueuenonempty cs.connmutex
	done
      with
      | Unix.Unix_error(c,x,y) -> (*** close connection ***)
	  log_string (Printf.sprintf "Unix error exception raised in connection listener for %s:\n%s %s %s\nClosing connection\n" (peeraddr !gcs) (Unix.error_message c) x y);
	  connsender_end()
      | End_of_file -> (*** close connection ***)
	  log_string (Printf.sprintf "Channel for connection %s raised End_of_file. Closing connection\n" (peeraddr !gcs));
	  connsender_end()
      | ProtocolViolation(x) -> (*** close connection ***)
	  log_string (Printf.sprintf "Protocol violation by connection %s: %s\nClosing connection\n" (peeraddr !gcs) x);
	  connsender_end()
      | SelfConnection -> (*** detected a self-connection attempt, close ***)
	  log_string (Printf.sprintf "Stopping potential self-connection\n");
	  connsender_end()
      | exc -> (*** report all other exceptions and close connection ***)
	  log_string (Printf.sprintf "Ignoring exception raised in connection listener for %s:\n%s\n" (peeraddr !gcs) (Printexc.to_string exc));
	  connsender_end()

let remove_dead_conns () =
  let tmminus1hr = Unix.time() -. 3600.0 in
  List.iter
    (fun (_,_,(s,sin,sout,gcs)) ->
      match !gcs with
      | Some(cs) ->
	  if cs.handshakestep < 5 && cs.lastmsgtm < tmminus1hr then (*** if the handshake has not completed in 1 hour, then kill conn ***)
	    begin
	      try
		shutdown_close s;
		close_in sin;
		close_out sout;
		gcs := None
	      with _ ->
		gcs := None
	    end
      | _ -> ())
    !netconns;
  Mutex.lock netconnsmutex;
  netconns :=
    List.filter
      (fun (_,_,(_,_,_,gcs)) ->
	match !gcs with
	| None -> false
	| Some(cs) -> true)
      !netconns;
  Mutex.unlock netconnsmutex

exception EnoughConnections

let initialize_conn_accept ra s =
  if List.length !netconns < !Config.maxconns then
    begin
      let sin = Unix.in_channel_of_descr s in
      let sout = Unix.out_channel_of_descr s in
      set_binary_mode_in sin true;
      set_binary_mode_out sout true;
      let tm = Unix.time() in
      let cs = { conntime = tm; realaddr = ra; connmutex = Mutex.create(); sendqueue = Queue.create(); sendqueuenonempty = Condition.create(); nonce = None; handshakestep = 1; peertimeskew = 0; protvers = Version.protocolversion; useragent = ""; addrfrom = ""; banned = false; lastmsgtm = tm; pending = []; sentinv = []; rinv = []; invreq = []; first_header_height = 0L; first_full_height = 0L; last_height = 0L } in
      let sgcs = (s,sin,sout,ref (Some(cs))) in
      let clth = Thread.create connlistener sgcs in
      let csth = Thread.create connsender sgcs in
      Mutex.lock netconnsmutex;
      netconns := (clth,csth,sgcs)::!netconns;
      Mutex.unlock netconnsmutex
    end
  else
    begin
      shutdown_close s;
      raise EnoughConnections
    end

let initialize_conn_2 n s sin sout =
  (*** initiate handshake ***)
  let vers = 1l in
  let srvs = 1L in
  let tm = Unix.time() in
  let fhh = 0L in
  let ffh = 0L in
  let lh = 0L in
  let relay = true in
  let lastchkpt = None in
  let vm = Buffer.create 100 in
  seosbf
    (seo_prod
       (seo_prod6 seo_int32 seo_int64 seo_int64 seo_string seo_string seo_int64)
       (seo_prod6 seo_string seo_int64 seo_int64 seo_int64 seo_bool (seo_option (seo_prod seo_int64 seo_hashval)))
       seosb
       ((vers,srvs,Int64.of_float tm,n,myaddr(),!this_nodes_nonce),
	(Version.useragent,fhh,ffh,lh,relay,lastchkpt))
       (vm,None));
  let cs = { conntime = tm; realaddr = n; connmutex = Mutex.create(); sendqueue = Queue.create(); sendqueuenonempty = Condition.create(); nonce = None; handshakestep = 2; peertimeskew = 0; protvers = Version.protocolversion; useragent = ""; addrfrom = ""; banned = false; lastmsgtm = tm; pending = []; sentinv = []; rinv = []; invreq = []; first_header_height = fhh; first_full_height = ffh; last_height = lh } in
  queue_msg cs Version (Buffer.contents vm);
  let sgcs = (s,sin,sout,ref (Some(cs))) in
  let clth = Thread.create connlistener sgcs in
  let csth = Thread.create connsender sgcs in
  Mutex.lock netconnsmutex;
  netconns := (clth,csth,sgcs)::!netconns;
  Mutex.unlock netconnsmutex;
  (clth,csth,sgcs)

let initialize_conn n s =
  let sin = Unix.in_channel_of_descr s in
  let sout = Unix.out_channel_of_descr s in
  set_binary_mode_in sin true;
  set_binary_mode_out sout true;
  initialize_conn_2 n s sin sout

let tryconnectpeer n =
  if List.length !netconns >= !Config.maxconns then raise EnoughConnections;
  if Hashtbl.mem bannedpeers n then raise BannedPeer;
  try
    Some(List.find (fun (_,_,(_,_,_,gcs)) -> n = peeraddr !gcs) !netconns);
  with Not_found ->
    let (ip,port,v6) = extract_ip_and_port n in
    begin
      try
	match !Config.socks with
	| None ->
	    let s = connectpeer ip port in
	    Some (initialize_conn n s)
	| Some(4) ->
	    let (s,sin,sout) = connectpeer_socks4 !Config.socksport ip port in
	    Some (initialize_conn_2 n s sin sout)
	| Some(5) ->
	    raise (Failure "socks5 is not yet supported")
	| Some(z) ->
	    raise (Failure ("do not know what socks" ^ (string_of_int z) ^ " means"))
      with
      | RequestRejected ->
	  log_string (Printf.sprintf "RequestRejected\n");
	  None
      | _ ->
	  None
    end

let netlistener l =
  while true do
    try
      let (s,a) = Unix.accept l in
      let ra =
	begin
	  match a with
	  | Unix.ADDR_UNIX(x) ->
	      log_string (Printf.sprintf "got local connection %s\n" x);
	      "local " ^ x
	  | Unix.ADDR_INET(x,y) ->
	      log_string (Printf.sprintf "got remote connection %s %d\n" (Unix.string_of_inet_addr x) y);
	      (Unix.string_of_inet_addr x) ^ " " ^ (string_of_int y)
	end
      in
      remove_dead_conns();
      initialize_conn_accept ra s
    with
    | EnoughConnections -> log_string (Printf.sprintf "Rejecting connection because of maxconns.\n");
    | _ -> ()
  done

let recently_requested (i,h) nw ir =
  try
    ignore (List.find (fun (j,k,tm) -> i = j && h = k && nw -. tm < 991.0) ir);
    true
  with Not_found -> false

let recently_sent (i,h) nw isnt =
  try
    ignore (List.find (fun (j,k,tm) -> i = j && h = k && nw -. tm < 353.0) isnt);
    true
  with Not_found -> false
  
let netseeker_loop () =
  while true do
    try
      remove_dead_conns();
      if List.length !netconns < max 1 (!Config.maxconns lsr 1) then
	begin
	  Hashtbl.iter
	    (fun n oldtm ->
	      try (*** don't try to connect to the same peer twice ***)
		ignore (List.find
			  (fun (_,_,(_,_,_,gcs)) -> peeraddr !gcs = n)
			  !netconns)
	      with Not_found -> ignore (tryconnectpeer n)
	      )
	    knownpeers;
	  match !newpeers with
	  | [] -> ()
	  | (n::r) ->
	      newpeers := r;
	      if not (Hashtbl.mem knownpeers n) then
		begin
		  try (*** don't try to connect to the same peer twice ***)
		    ignore (List.find
			      (fun (_,_,(_,_,_,gcs)) -> peeraddr !gcs = n)
			      !netconns)
		  with Not_found ->
		    ignore (tryconnectpeer n)
		end
	end;
      if !netconns = [] then
	begin
	  List.iter
	    (fun n -> ignore (tryconnectpeer n))
	    (if !Config.testnet then testnetfallbacknodes else fallbacknodes)
	end;
      (*** occasionally send a GetAddr request ***)
      let i = int_of_msgtype GetAddr in
      let h0 = (0l,0l,0l,0l,0l,0l,0l,0l) in
      let tm = Unix.time() in
      List.iter
	(fun (_,_,(_,_,_,gcs)) ->
	  match !gcs with
	    None -> ()
	  | Some(cs) ->
	      if cs.handshakestep = 5 && not (recently_requested (i,h0) tm cs.invreq) then
		begin
		  ignore (queue_msg cs GetAddr "");
		  cs.invreq <- (i,h0,tm)::List.filter (fun (j,k,tm0) -> tm -. tm0 < 3600.0) cs.invreq
		end
	  )
	!netconns;
      if !newpeers = [] || List.length !netconns = !Config.maxconns then
	Thread.delay 600.
      else
	Thread.delay 20.
    with
    | _ -> ()
  done

let netseeker () =
  loadknownpeers();
  netseekerth := Some(Thread.create netseeker_loop ())

let broadcast_requestdata mt h =
  let i = int_of_msgtype mt in
  let msb = Buffer.create 20 in
  seosbf (seo_hashval seosb h (msb,None));
  let ms = Buffer.contents msb in
  let tm = Unix.time() in
  List.iter
    (fun (lth,sth,(fd,sin,sout,gcs)) ->
       match !gcs with
       | Some(cs) ->
           if not (recently_requested (i,h) tm cs.invreq) &&
	     (List.mem (inv_of_msgtype mt,h) cs.rinv
	    || mt = GetCTreeElement || mt = GetHConsElement || mt = GetAsset)
	   then
             begin
               queue_msg cs mt ms;
               cs.invreq <- (i,h,tm)::List.filter (fun (j,k,tm0) -> tm -. tm0 < 3600.0) cs.invreq
             end
       | None -> ())
    !netconns;;

let find_and_send_requestdata mt h =
  let i = int_of_msgtype mt in
  let msb = Buffer.create 20 in
  seosbf (seo_hashval seosb h (msb,None));
  let ms = Buffer.contents msb in
  let tm = Unix.time() in
  let alrreq = ref false in
  try
    List.iter
      (fun (lth,sth,(fd,sin,sout,gcs)) ->
	match !gcs with
	| Some(cs) ->
            if not cs.banned && List.mem (inv_of_msgtype mt,h) cs.rinv then
	      if recently_requested (i,h) tm cs.invreq then
		begin
		  log_string (Printf.sprintf "already recently sent request %s %s from %s\n" (string_of_msgtype mt) (hashval_hexstring h) cs.addrfrom);
		  alrreq := true
		end
	      else
		begin
		  log_string (Printf.sprintf "sending request %s %s to %s\n" (string_of_msgtype mt) (hashval_hexstring h) cs.addrfrom);
		  let mh = queue_msg cs mt ms in
		  cs.invreq <- (i,h,tm)::List.filter (fun (j,k,tm0) -> tm -. tm0 < 3600.0) cs.invreq;
		  raise Exit
		end
	| None -> ())
      !netconns;
    if not !alrreq then raise Not_found
  with Exit ->
    ();;

let find_and_send_requestmissingheaders () =
  let i = int_of_msgtype GetHeaders in
  let ii = int_of_msgtype Headers in
  let tm = Unix.time() in
  try
    List.iter
      (fun (lth,sth,(fd,sin,sout,gcs)) ->
	match !gcs with
	| Some(cs) ->
	    if not cs.banned then
	      begin
		let rhl = ref [] in
		let mhl = ref !missingheaders in
		let j = ref 0 in
		while (!j < 256 && not (!mhl = [])) do
		  match !mhl with
		  | [] -> raise Exit (*** impossible ***)
		  | h::mhr ->
		      mhl := mhr;
		      if List.mem (ii,h) cs.rinv && not (recently_requested (i,h) tm cs.invreq) then
			begin
			  incr j;
			  rhl := h::!rhl
			end
		done;
		if not (!rhl = []) then
		  begin
		    let msb = Buffer.create 100 in
		    seosbf (seo_int8 seosb !j (msb,None));
		    List.iter
		      (fun h ->
			cs.invreq <- (i,h,tm)::List.filter (fun (j,k,tm0) -> tm -. tm0 < 3600.0) cs.invreq;
			seosbf (seo_hashval seosb h (msb,None)))
		      !rhl;
		    let ms = Buffer.contents msb in
		    let mh = queue_msg cs GetHeaders ms in
		    ()
		  end
	      end
	| None -> ())
      !netconns
  with Exit -> ();;

let broadcast_inv tosend =
  let invmsg = Buffer.create 10000 in
  let c = ref (seo_int32 seosb (Int32.of_int (List.length tosend)) (invmsg,None)) in
  List.iter
    (fun (i,h) ->
      c := seo_prod seo_int8 seo_hashval seosb (i,h) !c)
    tosend;
  let invmsgstr = Buffer.contents invmsg in
  log_string (Printf.sprintf "broadcast_inv Created invmsgstr %s\n" (string_hexstring invmsgstr));
  List.iter
    (fun (lth,sth,(fd,sin,sout,gcs)) ->
      match !gcs with
      | Some(cs) ->
	  log_string (Printf.sprintf "broadcast_inv sending to %s\n" cs.addrfrom);
	  ignore (queue_msg cs Inv invmsgstr)
      | None -> ())
    !netconns;;

Hashtbl.add msgtype_handler GetAddr
    (fun (sin,sout,cs,ms) ->
      let i = int_of_msgtype Addr in
      let tm = Unix.time() in
      if not (recently_sent (i,(0l,0l,0l,0l,0l,0l,0l,0l)) tm cs.sentinv) then (*** ignore GetAddr message if we recently sent addresses ***)
	begin
	  let pc = ref 0 in
	  cs.sentinv <- (i,(0l,0l,0l,0l,0l,0l,0l,0l),tm)::cs.sentinv;
	  let tm64 = Int64.of_float tm in
	  let yesterday = Int64.sub tm64 86400L in
	  let currpeers = ref [] in
	  let oldpeers = ref [] in
	  Hashtbl.iter
	    (fun nodeaddr lasttm ->
	      if not (nodeaddr = "") then
		if lasttm > yesterday then
		  currpeers := nodeaddr::!currpeers
		else
		  oldpeers := nodeaddr::!oldpeers)
	    knownpeers;
	  let cpl = List.length !currpeers in
	  let opl = List.length !oldpeers in
	  if cpl > 65535 then
	    begin
	      oldpeers := [];
	      for j = 65535 to cpl do
		match !currpeers with
		| (_::r) -> currpeers := r
		| [] -> ()
	      done
	    end;
	  let cpl = List.length !currpeers in
	  let opl = List.length !oldpeers in
	  for j = 65535 to cpl + opl do
	    match !oldpeers with
	    | (_::r) -> oldpeers := r
	    | [] -> ()
	  done;
	  let opl = List.length !oldpeers in
	  let l = !currpeers @ !oldpeers in
	  let ll = cpl + opl in
	  let addrmsg = Buffer.create 10000 in
	  let c = ref (seo_varintb seosb ll (addrmsg,None)) in
	  List.iter
	    (fun s ->
	      let cn = seo_string seosb s !c in
	      c := cn)
	    l;
	  seosbf !c;
	  ignore (queue_msg cs Addr (Buffer.contents addrmsg))
	end);;

Hashtbl.add msgtype_handler Addr
    (fun (sin,sout,cs,ms) ->
      let i = int_of_msgtype GetAddr in
      let tm = Unix.time() in
      if recently_requested (i,(0l,0l,0l,0l,0l,0l,0l,0l)) tm cs.invreq then (*** ignore Addr message unless it was recently requested ***)
	let c = ref (ms,String.length ms,None,0,0) in
	let m = ref 0 in
	let bhl = ref [] in
	let (n,cn) = sei_varintb seis !c in (*** < 65536 other addresses ***)
	c := cn;
	let numc = ref (List.length !netconns) in
	for j = 1 to n do
	  let (nodeaddr,cn) = sei_string seis !c in
	  if not (Hashtbl.mem knownpeers nodeaddr) then newpeers := nodeaddr::!newpeers
	done);;

Hashtbl.add msgtype_handler Ping
  (fun (sin,sout,cs,ms) ->
    let tm = Unix.time() in
    if tm -. cs.lastmsgtm >= 3600.0 then
      ignore (queue_msg cs Pong ""));;

Hashtbl.add msgtype_handler Pong (fun _ -> ());;
