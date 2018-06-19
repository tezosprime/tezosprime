(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Copyright (c) 2018 The Tezos' (Tezos Prime) developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Json
open Ser
open Hashaux
open Hash
open Net
open Db
open Mathdata
open Checking
open Assets
open Cryptocurr
open Tx
open Config

let datadir () = if !testnet then (Filename.concat !datadir "testnet") else !datadir

let intention_minage = 4L (** one day, at 6 hour block times **)

let maximum_age_steps = 32L (*** aging happens in n steps, with max n = 32 ***)

let sqrfun x = let y = big_int_of_int64 (Int64.add 1L x) in mult_big_int y y
let sqr16 x = sqrfun (min maximum_age_steps (Int64.shift_right x 4))
let sqr512 x = sqrfun (min maximum_age_steps (Int64.shift_right x 9))

let maximum_age_sqr = sqrfun maximum_age_steps
let reward_maturation = 32L (*** rewards become stakable after 32 blocks, about 8 days ***)
let close_to_unlocked = 8L

(***
  rewards must be locked for at least 512 blocks (about 4 months);
  otherwise locking rewards after their initial locktime can result in an unintentional maximization of coinage.
 ***)
let reward_locktime = 512L

let max_assets_at_address = 32 (** preventing having long lists of assets with a small hard limit **)
exception MaxAssetsAtAddress

let coinagefactor blkh bday obl =
  if bday = 0L then (*** coins in the initial distribution start out at maximum age ***)
    maximum_age_sqr
  else
    let prelock_age a = (*** ages more quickly at a rate of n^2 with n increasing roughly every 4 days, reaching maximum age in about 4 months ***)
      sqr16 a (*** multiply the currency units by [a/16]^2 with [a/16] int max 32 ***)
    in
    let postlock_age a = (*** ages slowly at a rate of n^2 with n increasing roughly every 4 months, reaching maximum age in about 12 years ***)
      sqr512 a (*** multiply the currency units by [a/512]^2 with [a/512] int max 32 ***)
    in
    match obl with
    | None -> (*** unlocked ***)
	if bday >= Int64.sub blkh 1L then (*** considered mature for staking after there has been at least one proof of burn ***)
	  zero_big_int
	else
	  prelock_age(Int64.sub blkh bday) (*** how many blocks since the output became born (changed from 'mature' to avoid needing to know first pob block after bday) ***)
    | Some(_,n,r) when r -> (*** in this case it's locked until block height n and is a reward ***)
	let mday = Int64.add bday reward_maturation in (*** insist on being age mature here, not just proof of burn maturity ***)
	if mday > blkh then (*** only start aging after it is mature ***)
	  zero_big_int
	else if blkh >= n then (*** after unlocked, start over aging as unlocked from the time it was unlocked ***)
	  postlock_age(Int64.sub blkh (max mday n)) (*** after unlocking, rewards restart aging, using the slower formula ***)
	else
	  prelock_age(Int64.sub blkh mday) (*** rewards must age, but age more quickly the longer it is locked ***)
    | Some(_,n,_) -> (*** in this case it's locked until block height n and is not a reward ***)
	if bday >= Int64.sub blkh 1L then (*** only start aging after it is mature ***)
	  zero_big_int
	else if blkh >= n then (*** after unlocked, start over aging as unlocked from the time it was unlocked ***)
	  postlock_age(Int64.sub blkh (max bday n))
	else
	  maximum_age_sqr (*** always at maximum age during after it is mature and until it is close to unlocked ***)

let coinage blkh bday obl v = mult_big_int (coinagefactor blkh bday obl) (big_int_of_int64 v)

type hlist = HHash of hashval * int | HNil | HCons of asset * hlist | HConsH of hashval * hlist

let rec hlist_len hl =
  match hl with
  | HHash(_,l) -> l
  | HNil -> 0
  | HCons(_,hr) -> hlist_len hr
  | HConsH(_,hr) -> hlist_len hr

let rec hlist_hashroot hl =
  match hl with
  | HHash(h,l) -> Some(h,l)
  | HNil -> None
  | HCons(a,hr) ->
      begin
	match hlist_hashroot hr with
	| None -> Some(hashtag (hashasset a) 3l,1)
	| Some(k,l) -> Some(hashtag (hashpair (hashasset a) k) (Int32.of_int (4096 + l)),1+l)
      end
  | HConsH(h,hr) ->
      begin
	match hlist_hashroot hr with
	| None -> Some(hashtag h 3l,1)
	| Some(k,l) -> Some(hashtag (hashpair h k) (Int32.of_int (4096 + l)),1+l)
      end

type nehlist = NehHash of hashval * int | NehCons of asset * hlist | NehConsH of hashval * hlist

let nehlist_len hl =
  match hl with
  | NehHash(_,l) -> l
  | NehCons(_,hr) -> 1+hlist_len hr
  | NehConsH(_,hr) -> 1+hlist_len hr

let nehlist_hlist hl =
  match hl with
  | NehHash(h,l) -> HHash(h,l)
  | NehCons(a,hr) -> HCons(a,hr)
  | NehConsH(h,hr) -> HConsH(h,hr)

let nehlist_hashroot hl =
  match hl with
  | NehHash(h,l) -> (h,l)
  | NehCons(a,hr) ->
      begin
	match hlist_hashroot hr with
	| None -> (hashtag (hashasset a) 3l,1)
	| Some(k,l) -> (hashtag (hashpair (hashasset a) k) (Int32.of_int (4096+l)),1+l)
      end
  | NehConsH(h,hr) ->
      begin
	match hlist_hashroot hr with
	| None -> (hashtag h 3l,1)
	| Some(k,l) -> (hashtag (hashpair h k) (Int32.of_int (4096+l)),1+l)
      end

let rec in_hlist a hl =
  match hl with
  | HCons(b,hr) when a = b -> true
  | HCons(_,hr) -> in_hlist a hr
  | HConsH(_,hr) -> in_hlist a hr
  | _ -> false

let in_nehlist a hl =
  match hl with
  | NehCons(b,hr) when a = b -> true
  | NehCons(_,hr) -> in_hlist a hr
  | NehConsH(_,hr) -> in_hlist a hr
  | _ -> false

type ctree =
  | CLeaf of bool list * nehlist
  | CHash of hashval
  | CLeft of ctree
  | CRight of ctree
  | CBin of ctree * ctree

let rec print_ctree_r c n =
  for i = 1 to n do Printf.printf " " done;
  match c with
  | CLeaf(bl,NehHash(h,l)) -> Printf.printf "Leaf %s[%d]\n" (hashval_hexstring h) l
  | CLeaf(bl,hl) -> Printf.printf "Leaf ...[%d]\n" (nehlist_len hl)
  | CHash(h) -> Printf.printf "H %s\n" (hashval_hexstring h)
  | CLeft(c0) -> Printf.printf "L\n"; print_ctree_r c0 (n+1)
  | CRight(c1) -> Printf.printf "R\n"; print_ctree_r c1 (n+1)
  | CBin(c0,c1) -> Printf.printf "B\n"; print_ctree_r c0 (n+1); print_ctree_r c1 (n+1)

let print_ctree c = print_ctree_r c 0

let rec print_hlist_gen f hl g =
  match hl with
  | HHash(h,l) -> Printf.fprintf f "...%s[%d]...\n" (hashval_hexstring h) l
  | HNil -> ()
  | HCons((aid,bday,obl,Currency(v)) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] Currency %s tezzie%s (%Ld cant%s)\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (tezzies_of_cants v) (if v = 100000000000L then "" else "s") v (if v = 1L then "" else "s");
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,Bounty(v)) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] Bounty %s tezzie%s (%Ld cant%s)\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (tezzies_of_cants v) (if v = 100000000000L then "" else "s") v (if v = 1L then "" else "s");
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,OwnsObj(k,gamma,Some(r))) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] OwnsObj %s %s royalty fee %s tezzie%s\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (hashval_hexstring k) (addr_daliladdrstr (payaddr_addr gamma)) (tezzies_of_cants r) (if r = 100000000000L then "" else "s");
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,OwnsObj(k,gamma,None)) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] OwnsObj %s %s None\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (hashval_hexstring k) (addr_daliladdrstr (payaddr_addr gamma));
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,OwnsProp(k,gamma,Some(r))) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] OwnsProp %s %s royalty fee %s tezzie%s\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (hashval_hexstring k) (addr_daliladdrstr (payaddr_addr gamma)) (tezzies_of_cants r) (if r = 100000000000L then "" else "s");
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,OwnsProp(k,gamma,None)) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] OwnsProp %s %s None\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (hashval_hexstring k) (addr_daliladdrstr (payaddr_addr gamma));
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,OwnsNegProp) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] OwnsNegProp\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday;
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,RightsObj(k,r)) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] RightsObj %s %Ld\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (hashval_hexstring k) r;
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,RightsProp(k,r)) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld] RightsProp %s %Ld\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday (hashval_hexstring k) r;
	g a;
	print_hlist_gen f hr g
      end
  | HCons((aid,bday,obl,v) as a,hr) ->
      begin
	Printf.fprintf f "%s: (id %s) [%Ld]\n" (hashval_hexstring (hashasset a)) (hashval_hexstring aid) bday;
	g a;
	print_hlist_gen f hr g
      end
  | HConsH(ah,hr) ->
      begin
	Printf.fprintf f "%s: *\n" (hashval_hexstring ah);
	print_hlist_gen f hr g
      end

let print_hlist f hl = print_hlist_gen f hl (fun _ -> ())

let right_trim c s =
  let l = ref ((String.length s) - 1) in
  while (!l > 0 && s.[!l] = c) do
    decr l
  done;
  String.sub s 0 (!l+1)

let rec print_hlist_to_buffer_gen sb blkh hl g =
  match hl with
  | HHash(h,l) ->
      Buffer.add_string sb "...";
      Buffer.add_string sb (hashval_hexstring h);
      Buffer.add_string sb "...\n"
  | HNil -> ()
  | HCons((aid,bday,None,Currency(v)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] Currency ";
	Buffer.add_string sb (tezzies_of_cants v);
	Buffer.add_string sb " tezzies; coinage ";
	Buffer.add_string sb (string_of_big_int (coinage blkh bday None v));
	Buffer.add_char sb '\n';
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,((Some(delta,locktime,b)) as obl),Currency(v)) as a,hr) when b ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	if locktime > blkh then
	  Buffer.add_string sb "] Currency (Reward, Locked) "
	else
	  Buffer.add_string sb "] Currency (Reward) ";
	Buffer.add_string sb (tezzies_of_cants v);
	Buffer.add_string sb " tezzies spendable by ";
	Buffer.add_string sb (addr_daliladdrstr (payaddr_addr delta));
	if locktime > blkh then
	  begin
	    Buffer.add_string sb " unlocks at height ";
	    Buffer.add_string sb (Int64.to_string locktime);
	    Buffer.add_string sb " in ";
	    Buffer.add_string sb (Int64.to_string (Int64.sub locktime blkh));
	    Buffer.add_string sb " blocks ";
	  end;
	Buffer.add_string sb "; coinage ";
	Buffer.add_string sb (string_of_big_int (coinage blkh bday obl v));
	Buffer.add_char sb '\n';
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,((Some(delta,locktime,b)) as obl),Currency(v)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	if locktime > blkh then
	  Buffer.add_string sb "] Currency (Locked) "
	else
	  Buffer.add_string sb "] Currency ";
	Buffer.add_string sb (tezzies_of_cants v);
	if v = 100000000000L then
	  Buffer.add_string sb " tezzie spendable by "
	else
	  Buffer.add_string sb " tezzies spendable by ";
	Buffer.add_string sb (addr_daliladdrstr (payaddr_addr delta));
	if locktime > blkh then
	  begin
	    Buffer.add_string sb " unlocks at height ";
	    Buffer.add_string sb (Int64.to_string locktime);
	    Buffer.add_string sb " in ";
	    Buffer.add_string sb (Int64.to_string (Int64.sub locktime blkh));
	    Buffer.add_string sb " blocks ";
	  end;
	Buffer.add_string sb "; coinage ";
	Buffer.add_string sb (string_of_big_int (coinage blkh bday obl v));
	Buffer.add_char sb '\n';
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,Bounty(v)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] Bounty ";
	Buffer.add_string sb (tezzies_of_cants v);
	if v = 100000000000L then
	  Buffer.add_string sb " tezzie\n"
	else
	  Buffer.add_string sb " tezzies\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,OwnsObj(k,gamma,Some(r))) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] owned object ";
	Buffer.add_string sb (hashval_hexstring k);
	Buffer.add_string sb " by ";
	Buffer.add_string sb (addr_daliladdrstr (payaddr_addr gamma));
	Buffer.add_string sb " each right costs ";
	Buffer.add_string sb (tezzies_of_cants r);
	if r = 100000000000L then
	  Buffer.add_string sb " tezzie\n"
	else
	  Buffer.add_string sb " tezzies\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,OwnsObj(k,gamma,None)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] owned object ";
	Buffer.add_string sb (hashval_hexstring k);
	Buffer.add_string sb " by ";
	Buffer.add_string sb (addr_daliladdrstr (payaddr_addr gamma));
	Buffer.add_string sb " rights cannot be purchased\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,OwnsProp(k,gamma,Some(r))) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] owned prop ";
	Buffer.add_string sb (hashval_hexstring k);
	Buffer.add_string sb " by ";
	Buffer.add_string sb (addr_daliladdrstr (payaddr_addr gamma));
	Buffer.add_string sb " each right costs ";
	Buffer.add_string sb (tezzies_of_cants r);
	if r = 100000000000L then
	  Buffer.add_string sb " tezzie\n"
	else
	  Buffer.add_string sb " tezzies\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,OwnsProp(k,gamma,None)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] owned prop ";
	Buffer.add_string sb (hashval_hexstring k);
	Buffer.add_string sb " by ";
	Buffer.add_string sb (addr_daliladdrstr (payaddr_addr gamma));
	Buffer.add_string sb " rights cannot be purchased\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,OwnsNegProp) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] owned negation of prop\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,RightsObj(k,r)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] ";
	Buffer.add_string sb (Int64.to_string r);
	Buffer.add_string sb " rights to use object ";
	Buffer.add_string sb (hashval_hexstring k);
	Buffer.add_char sb '\n';
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,RightsProp(k,r)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] ";
	Buffer.add_string sb (Int64.to_string r);
	Buffer.add_string sb " rights to use prop ";
	Buffer.add_string sb (hashval_hexstring k);
	Buffer.add_char sb '\n';
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,Marker) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] Marker\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,TheoryPublication(gamma,nonce,d)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] Theory\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,SignaPublication(gamma,nonce,th,d)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] Signature\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HCons((aid,bday,obl,DocPublication(gamma,nonce,th,d)) as a,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring aid);
	Buffer.add_string sb " [";
	Buffer.add_string sb (Int64.to_string bday);
	Buffer.add_string sb "] Document\n";
	g a;
	print_hlist_to_buffer_gen sb blkh hr g
      end
  | HConsH(ah,hr) ->
      begin
	Buffer.add_string sb (hashval_hexstring ah);
	Buffer.add_string sb " *\n";
	print_hlist_to_buffer_gen sb blkh hr g
      end

let print_hlist_to_buffer sb blkh hl = print_hlist_to_buffer_gen sb blkh hl (fun _ -> ())

let rec print_ctree_all_r f c n br =
  for i = 1 to n do Printf.fprintf f " " done;
  match c with
  | CLeaf(bl,hl) -> Printf.fprintf f "Leaf %s\n" (addr_daliladdrstr (bitseq_addr ((List.rev br) @ bl))); print_hlist f (nehlist_hlist hl)
  | CHash(h) -> Printf.fprintf f "H %s\n" (hashval_hexstring h)
  | CLeft(c0) -> Printf.fprintf f "L\n"; print_ctree_all_r f c0 (n+1) (false::br)
  | CRight(c1) -> Printf.fprintf f "R\n"; print_ctree_all_r f c1 (n+1) (true::br)
  | CBin(c0,c1) -> Printf.fprintf f "B\n"; print_ctree_all_r f c0 (n+1) (false::br); print_ctree_all_r f c1 (n+1) (true::br)

let print_ctree_all f c = print_ctree_all_r f c 0 []

let rec ctree_hashroot c =
  match c with
  | CLeaf(bl,hl) ->
      List.fold_right
	(fun b h ->
	  if b then
	    hashopair2 None h
	  else
	    hashopair1 h None
	)
	bl (let (h,l) = nehlist_hashroot hl in if l = 1 then h else (hashtag h (Int32.of_int (4224+l)))) (*** commit to the number of assets held, but treating 1 in a special way to maintain compatibility with the initial ledger ***)
  | CHash(h) -> h
  | CLeft(c0) -> hashopair1 (ctree_hashroot c0) None
  | CRight(c1) -> hashopair2 None (ctree_hashroot c1)
  | CBin(c0,c1) -> hashopair1 (ctree_hashroot c0) (Some (ctree_hashroot c1))

let rec ctree_numnodes c =
  match c with
  | CLeaf(_,_) -> 1
  | CHash(_) -> 1
  | CLeft(c) -> 1 + ctree_numnodes c
  | CRight(c) -> 1 + ctree_numnodes c
  | CBin(c0,c1) -> 1 + ctree_numnodes c0 + ctree_numnodes c1

let octree_numnodes oc =
  match oc with
  | None -> 0
  | Some(c) -> ctree_numnodes c

let octree_hashroot c =
  match c with
  | Some(c) -> Some(ctree_hashroot c)
  | None -> None

let rec strip_bitseq_false l =
  match l with
  | [] -> []
  | ((false::bl),x)::r -> (bl,x)::strip_bitseq_false r
  | ((true::bl),x)::r -> strip_bitseq_false r
  | _ -> raise (Failure "bitseq length error")
  
let rec strip_bitseq_true l =
  match l with
  | [] -> []
  | ((true::bl),x)::r -> (bl,x)::strip_bitseq_true r
  | ((false::bl),x)::r -> strip_bitseq_true r
  | _ -> raise (Failure "bitseq length error")

let rec strip_bitseq_false0 l =
  match l with
  | [] -> []
  | (false::bl)::r -> bl::strip_bitseq_false0 r
  | (true::bl)::r -> strip_bitseq_false0 r
  | _ -> raise (Failure "bitseq length error")
  
let rec strip_bitseq_true0 l =
  match l with
  | [] -> []
  | (true::bl)::r -> bl::strip_bitseq_true0 r
  | (false::bl)::r -> strip_bitseq_true0 r
  | _ -> raise (Failure "bitseq length error")

let rec hlist_new_assets nw old =
  match nw with
  | [] -> old
  | a::nwr -> HCons(a,hlist_new_assets nwr old)

(** * serialization **)
let rec seo_hlist o hl c =
  match hl with
  | HHash(h,l) -> (* 00 *)
      let c = o 2 0 c in
      let c = seo_hashval o h c in
      seo_int8 o l c
  | HNil -> (* 01 *)
      let c = o 2 1 c in
      c
  | HCons(a,hr) -> (* 10 *)
      let c = o 2 2 c in
      let c = seo_asset o a c in
      seo_hlist o hr c
  | HConsH(ah,hr) -> (* 11 *)
      let c = o 2 3 c in
      let c = seo_hashval o ah c in
      seo_hlist o hr c

let rec sei_hlist i c =
  let (x,c) = i 2 c in
  if x = 0 then
    let (h,c) = sei_hashval i c in
    let (l,c) = sei_int8 i c in
    (HHash(h,l),c)
  else if x = 1 then
      (HNil,c)
  else if x = 2 then
    let (a,c) = sei_asset i c in
    let (hr,c) = sei_hlist i c in
    (HCons(a,hr),c)
  else
    let (ah,c) = sei_hashval i c in
    let (hr,c) = sei_hlist i c in
    (HConsH(ah,hr),c)

let seo_nehlist o hl c =
  match hl with
  | NehHash(h,l) when l=1 -> (* 0 *) (*** treat l=1 the old way for compatibility ***)
      let c = o 1 0 c in
      seo_hashval o h c
  | NehHash(h,l) -> (* 1 0 *)
      let c = o 2 1 c in
      let c = seo_hashval o h c in
      seo_int8 o l c
  | NehCons(a,hr) -> (* 1 1 0 *)
      let c = o 3 3 c in
      let c = seo_asset o a c in
      seo_hlist o hr c
  | NehConsH(ah,hr) -> (* 1 1 1 *)
      let c = o 3 7 c in
      let c = seo_hashval o ah c in
      seo_hlist o hr c

let sei_nehlist i c =
  let (x,c) = i 1 c in
  if x = 0 then (*** default of l=1 for compatibility ***)
    let (h,c) = sei_hashval i c in
    (NehHash(h,1),c)
  else
    let (y,c) = i 1 c in
    if y = 0 then
      let (h,c) = sei_hashval i c in
      let (l,c) = sei_int8 i c in
      (NehHash(h,l),c)
    else
      let (z,c) = i 1 c in
      if z = 0 then
	let (a,c) = sei_asset i c in
	let (hr,c) = sei_hlist i c in
	(NehCons(a,hr),c)
      else
	let (ah,c) = sei_hashval i c in
	let (hr,c) = sei_hlist i c in
	(NehConsH(ah,hr),c)

let rec seo_ctree o tr c =
  match tr with
  | CLeaf(bl,hl) -> (* 00 *)
      let c = o 2 0 c in
      let c = seo_list seo_bool o bl c in
      seo_nehlist o hl c
  | CHash(h) -> (* 01 *)
      let c = o 2 1 c in
      seo_hashval o h c
  | CLeft(trl) -> (* 10 0 *)
      let c = o 3 2 c in
      let c = seo_ctree o trl c in
      c
  | CRight(trr) -> (* 10 1 *)
      let c = o 3 6 c in
      let c = seo_ctree o trr c in
      c
  | CBin(trl,trr) -> (* 11 *)
      let c = o 2 3 c in
      let c = seo_ctree o trl c in
      let c = seo_ctree o trr c in
      c

let rec sei_ctree i c =
  let (x,c) = i 2 c in
  if x = 0 then
    let (bl,c) = sei_list sei_bool i c in
    let (hl,c) = sei_nehlist i c in
    (CLeaf(bl,hl),c)
  else if x = 1 then
    let (h,c) = sei_hashval i c in
    (CHash(h),c)
  else if x = 2 then
    let (y,c) = i 1 c in
    let (tr,c) = sei_ctree i c in
    if y = 0 then
      (CLeft(tr),c)
    else
      (CRight(tr),c)
  else
    let (trl,c) = sei_ctree i c in
    let (trr,c) = sei_ctree i c in
    (CBin(trl,trr),c)

let rec reduce_hlist_to_approx al hl =
  match hl with
  | HNil -> HNil
  | HHash(h,l) -> HHash(h,l)
  | HCons((h1,bh1,o1,u1),hr) ->
      if al = [] then
	begin
	  match hlist_hashroot hl with
	  | Some(h,l) -> HHash(h,l)
	  | None -> raise (Failure("Impossible"))
	end
      else
	if List.mem h1 al then
	  HCons((h1,bh1,o1,u1),reduce_hlist_to_approx (List.filter (fun z -> not (z = h1)) al) hr)
	else
	  HConsH(h1,reduce_hlist_to_approx al hr)
  | HConsH(h1,hr) ->
      HConsH(h1,reduce_hlist_to_approx al hr)

let save_ctree f tr =
  let ch = open_out_bin f in
  let c = seo_ctree seoc tr (ch,None) in
  seocf c;
  close_out ch

let save_octree f tr =
  let ch = open_out_bin f in
  let c = seo_option seo_ctree seoc tr (ch,None) in
  seocf c;
  close_out ch

let load_ctree f =
  let ch = open_in_bin f in
  let (tr,_) = sei_ctree seic (ch,None) in
  close_in ch;
  tr

let load_octree f =
  let ch = open_in_bin f in
  let (tr,_) = sei_option sei_ctree seic (ch,None) in
  close_in ch;
  tr

let ensure_dir_exists d =
  try
    let s = Unix.stat d in
    if not (s.Unix.st_kind = Unix.S_DIR) then
      raise (Failure (d ^ " is not a directory"))
  with
  | Unix.Unix_error(Unix.ENOENT,_,_) -> raise (Failure(d ^ " directory does not exist"))
  | _ -> raise (Failure("Problem with " ^ d))

exception FoundHashval of hashval

module DbHConsElt =
  Dbbasic
    (struct
      type t = hashval * (hashval * int) option
      let basedir = "hconselt"
      let seival = sei_prod sei_hashval (sei_option (sei_prod sei_hashval sei_int8)) seic
      let seoval = seo_prod seo_hashval (seo_option (seo_prod seo_hashval seo_int8)) seoc
    end)

module DbHConsEltAt =
  Dbbasic
    (struct
      type t = addr
      let basedir = "hconseltat"
      let seival = sei_addr seic
      let seoval = seo_addr seoc
    end)

let get_hcons_element h =
  try
    DbHConsElt.dbget h
  with Not_found -> (*** request it and fail ***)
    broadcast_requestdata GetHConsElement h;
    raise GettingRemoteData

let rec save_hlist_elements hl alpha =
  match hl with
  | HCons(a,hr) ->
      let ah = hashasset a in
      DbAsset.dbput ah a;
      if !Config.extraindex then DbAssetIdAt.dbput (assetid a) alpha;
      let h = save_hlist_elements hr alpha in
      let (r,l) =
	match h with
	| None -> (hashtag ah 3l,1)
	| Some(k,l) -> (hashtag (hashpair ah k) (Int32.of_int (4096+l)),1+l)
      in
      if !Config.extraindex then DbHConsEltAt.dbput r alpha;
      DbHConsElt.dbput r (ah,h);
      Some(r,l)
  | HConsH(ah,hr) ->
      let h = save_hlist_elements hr alpha in
      let (r,l) =
	match h with
	| None -> (hashtag ah 3l,1)
	| Some(k,l) -> (hashtag (hashpair ah k) (Int32.of_int (4096+l)),1+l)
      in
      if !Config.extraindex then DbHConsEltAt.dbput r alpha;
      DbHConsElt.dbput r (ah,h);
      Some(r,l)
  | HNil -> None
  | HHash(r,l) -> Some(r,l)

let save_nehlist_elements hl alpha =
  match hl with
  | NehCons(a,hr) ->
      let ah = hashasset a in
      DbAsset.dbput ah a;
      if !Config.extraindex then DbAssetIdAt.dbput (assetid a) alpha;
      let h = save_hlist_elements hr alpha in
      let (r,l) = 
	match h with
	| None -> (hashtag ah 3l,1)
	| Some(k,l) -> (hashtag (hashpair ah k) (Int32.of_int (4096+l)),1+l)
      in
      DbHConsElt.dbput r (ah,h);
      if !Config.extraindex then DbHConsEltAt.dbput r alpha;
      (r,l)
  | NehConsH(ah,hr) ->
      let h = save_hlist_elements hr alpha in
      let (r,l) = 
	match h with
	| None -> (hashtag ah 3l,1)
	| Some(k,l) -> (hashtag (hashpair ah k) (Int32.of_int (4096+l)),1+l)
      in
      DbHConsElt.dbput r (ah,h);
      if !Config.extraindex then DbHConsEltAt.dbput r alpha;
      (r,l)
  | NehHash(r,l) -> (r,l)

(** exp: bool indicating if hashes should be expanded, req: bool indicating if missing hashes should be requested from peers;
  raises Not_found if exp was true but a hash was not in the database;
  raises GettingRemoteData if exp and req were true, a hash was not in the database and it is being requested from peers
 **)
let rec hlist_lookup_asset_gen exp req p hl =
  match hl with
  | HCons(a,hr) when p a -> Some(a)
  | HConsH(h,hr) ->
      if exp then
	let a = if req then get_asset h else DbAsset.dbget h in
	hlist_lookup_asset_gen exp req p (HCons(a,hr))
      else
	hlist_lookup_asset_gen exp req p hr (* Skip this one and search for another asset satisfying p. Note: This means that if the asset with id h satisfies p, we will miss it. This is even the case when p a means assetid a = h *)
  | HHash(h,l) ->
      if exp then
	begin
	  let (h1,h2) = if req then get_hcons_element h else DbHConsElt.dbget h in
	  match h2 with
	  | Some(h2,l2) -> hlist_lookup_asset_gen exp req p (HConsH(h1,HHash(h2,l2)))
	  | None -> hlist_lookup_asset_gen exp req p (HConsH(h1,HNil))
	end
      else
	None
  | HCons(a,hr) -> hlist_lookup_asset_gen exp req p hr
  | _ -> None

let nehlist_lookup_asset_gen exp req p hl = hlist_lookup_asset_gen exp req p (nehlist_hlist hl)

let hlist_lookup_asset exp req k hl = hlist_lookup_asset_gen exp req (fun a -> assetid a = k) hl
let nehlist_lookup_asset exp req k hl = nehlist_lookup_asset_gen exp req (fun a -> assetid a = k) hl

let hlist_lookup_marker exp req hl = hlist_lookup_asset_gen exp req (fun a -> assetpre a = Marker) hl
let nehlist_lookup_marker exp req hl = nehlist_lookup_asset_gen exp req (fun a -> assetpre a = Marker) hl

let hlist_lookup_obj_owner exp req oid hl =
  match hlist_lookup_asset_gen exp req (fun a -> match a with (_,_,_,OwnsObj(oid2,_,_)) when oid = oid2 -> true | _ -> false) hl with
  | Some(_,_,_,OwnsObj(_,beta,r)) -> Some(beta,r)
  | _ -> None

let nehlist_lookup_obj_owner exp req oid hl =
  match nehlist_lookup_asset_gen exp req (fun a -> match a with (_,_,_,OwnsObj(oid2,_,_)) when oid = oid2 -> true | _ -> false) hl with
  | Some(_,_,_,OwnsObj(_,beta,r)) -> Some(beta,r)
  | _ -> None

let rec hlist_lookup_prop_owner exp req pid hl =
  match hlist_lookup_asset_gen exp req (fun a -> match a with (_,_,_,OwnsProp(pid2,_,_)) when pid = pid2 -> true | _ -> false) hl with
  | Some(_,_,_,OwnsProp(_,beta,r)) -> Some(beta,r)
  | _ -> None

let nehlist_lookup_prop_owner exp req pid hl =
  match nehlist_lookup_asset_gen exp req (fun a -> match a with (_,_,_,OwnsProp(pid2,_,_)) when pid = pid2 -> true | _ -> false) hl with
  | Some(_,_,_,OwnsProp(_,beta,r)) -> Some(beta,r)
  | _ -> None

let hlist_lookup_neg_prop_owner exp req hl =
  match hlist_lookup_asset_gen exp req (fun a -> assetpre a = OwnsNegProp) hl with
  | Some(_) -> true
  | None -> false

let nehlist_lookup_neg_prop_owner exp req hl =
  match nehlist_lookup_asset_gen exp req (fun a -> assetpre a = OwnsNegProp) hl with
  | Some(_) -> true
  | None -> false

module DbCTreeElt =
  Dbbasic
    (struct
      type t = ctree
      let basedir = "ctreeelt"
      let seival = sei_ctree seic
      let seoval = seo_ctree seoc
    end)

module DbCTreeEltAt =
  Dbbasic
    (struct
      type t = bool list
      let basedir = "ctreeeltat"
      let seival = sei_list sei_bool seic
      let seoval = seo_list seo_bool seoc
    end)

let rec ctree_element_a tr i =
  if i > 0 then
    begin
      match tr with
      | CLeaf(_,NehHash(_,_)) -> true
      | CLeft(tr0) -> ctree_element_a tr0 (i-1)
      | CRight(tr1) -> ctree_element_a tr1 (i-1)
      | CBin(tr0,tr1) -> ctree_element_a tr0 (i-1) && ctree_element_a tr1 (i-1)
      | _ -> false
    end
  else
    match tr with
    | CHash(_) -> true
    | _ -> false

let ctree_element_p tr =
  ctree_element_a tr 9

let rec save_ctree_elements_a tr i pl =
  if i > 0 then
    match tr with
    | CLeaf(bl,hl) ->
	let (h,l) = save_nehlist_elements hl (bitseq_addr (((List.rev pl) @ bl))) in
	let h2 = if l = 1 then h else (hashtag h (Int32.of_int (4224+l))) in (*** commit to the number of assets held, but treating 1 in a special way to maintain compatibility with the initial ledger ***)
	let r = List.fold_right
	    (fun b h ->
	      if b then
		hashopair2 None h
	      else
		hashopair1 h None
	    )
	    bl h2 (*** the h2 is the combination of h with the commitment to the length of l ***)
	in
	let tr2 = CLeaf(bl,NehHash(h,l)) in (*** the h is the key to the first hcons element, without commitment to l ***)
	(tr2,r)
    | CLeft(trl) ->
	let (trl2,hl) = save_ctree_elements_a trl (i-1) (false::pl) in
	let r = hashopair1 hl None in
	(CLeft(trl2),r)
    | CRight(trr) ->
	let (trr2,hr) = save_ctree_elements_a trr (i-1) (true::pl) in
	let r = hashopair2 None hr in
	(CRight(trr2),r)
    | CBin(trl,trr) ->
	let (trl2,hl) = save_ctree_elements_a trl (i-1) (false::pl) in
	let (trr2,hr) = save_ctree_elements_a trr (i-1) (true::pl) in
	let r = hashopair1 hl (Some(hr)) in
	(CBin(trl2,trr2),r)
    | CHash(r) -> (tr,r)
  else
    let (tre,r) = save_ctree_elements_a tr 9 pl in
    if ctree_element_p tre then (*** make sure it's an element before saving it ***)
      begin
	DbCTreeElt.dbput r tre;
	if !Config.extraindex then DbCTreeEltAt.dbput r (List.rev pl);
	(CHash(r),r)
      end
    else (*** if it isn't an element (presumably because it's only approximating an element) then return the hash root only ***)
      (CHash(r),r)
    
let save_ctree_elements tr =
  let (tre,r) = save_ctree_elements_a tr 0 [] in
  r

let load_hlist_element h =
  match DbHConsElt.dbget h with
  | (ah,Some(k,l)) -> HConsH(ah,HHash(k,l))
  | (ah,None) -> HConsH(ah,HNil)

let load_nehlist_element h =
  match DbHConsElt.dbget h with
  | (ah,Some(k,l)) -> NehConsH(ah,HHash(k,l))
  | (ah,None) -> NehConsH(ah,HNil)

let get_hlist_element h =
  match get_hcons_element h with
  | (ah,Some(k,l)) -> HConsH(ah,HHash(k,l))
  | (ah,None) -> HConsH(ah,HNil)

let get_nehlist_element h =
  match get_hcons_element h with
  | (ah,Some(k,l)) -> NehConsH(ah,HHash(k,l))
  | (ah,None) -> NehConsH(ah,HNil)

(**
  if exp is true, then allow loading from database.
  if req is true, then allow remote requests.
  if exp and req are false (as should be the case when validating blocks): this should never request information from the database or remote nodes; if an asset to spend is not found, raise Not_found
**)
let rec remove_assets_hlist exp req hl spent =
  if spent = [] then (** if spent is empty, then we have finished removing (we assume asset ids are unique so one removal is enough) **)
    hl
  else
    match hl with
    | HCons((h,bh,obl,u) as a,hr) ->
	if List.mem h spent then
	  remove_assets_hlist exp req hr (List.filter (fun k -> not (k = h)) spent) (** remember it has been removed **)
	else
	  HCons(a,remove_assets_hlist exp req hr spent)
    | HConsH(h,hr) ->
	if exp then
	  if req then
	    let a = get_asset h in
	    remove_assets_hlist exp req (HCons(a,hr)) spent
	  else
	    let a = DbAsset.dbget h in
	    remove_assets_hlist exp req (HCons(a,hr)) spent
	else (** assume h is not an asset to be removed (if it is, spent will be nonempty when we get to the end) **)
	  HConsH(h,remove_assets_hlist exp req hr spent)
    | HHash(h,l) ->
	if exp then
	  if req then
	    let (h1,h2) = get_hcons_element h in
	    remove_assets_hlist exp req (HConsH(h1,match h2 with Some(hr,l) -> HHash(hr,l) | None -> HNil)) spent
	  else
	    let (h1,h2) = DbHConsElt.dbget h in
	    remove_assets_hlist exp req (HConsH(h1,match h2 with Some(hr,l) -> HHash(hr,l) | None -> HNil)) spent
	else
	  raise Not_found (*** spent is nonempty, but we cannot continue, so not enough information is on the hl ***)
    | _ ->
	raise Not_found (*** spent is nonempty, but we cannot continue, so not enough information is on the hl ***)

let rec ctree_super_element_a tr i =
  if i > 0 then
    begin
      match tr with
      | CLeaf(_,_) -> true
      | CLeft(tr0) -> ctree_super_element_a tr0 (i-1)
      | CRight(tr1) -> ctree_super_element_a tr1 (i-1)
      | CBin(tr0,tr1) -> ctree_super_element_a tr0 (i-1) && ctree_super_element_a tr1 (i-1)
      | _ -> false
    end
  else
    true

(*** A 'superelement' is a ctree with enough information to reduce to an element. ***)
let ctree_super_element_p tr =
  ctree_super_element_a tr 9

let rec super_element_to_element_a tr i =
  if i > 0 then
    begin
      match tr with
      | CLeaf(bl,hl) ->
	  let (h,l) = nehlist_hashroot hl in
	  CLeaf(bl,NehHash(h,l))
      | CLeft(tr0) -> CLeft(super_element_to_element_a tr0 (i-1))
      | CRight(tr1) -> CRight(super_element_to_element_a tr1 (i-1))
      | CBin(tr0,tr1) -> CBin(super_element_to_element_a tr0 (i-1),super_element_to_element_a tr1 (i-1))
      | _ -> raise (Failure("not a super-element"))
    end
  else
    CHash(ctree_hashroot tr)

let super_element_to_element tr =
  super_element_to_element_a tr 9

let get_ctree_element h =
  try
    DbCTreeElt.dbget h
  with Not_found -> (*** request it and fail ***)
    broadcast_requestdata GetCTreeElement h;
    raise GettingRemoteData

let expand_ctree_element req h =
  if req then
    get_ctree_element h
  else
    DbCTreeElt.dbget h

let rec octree_S_inv exp req c =
  match c with
  | None -> (None,None)
  | Some(CHash(h)) ->
      if exp then
	if req then
	  octree_S_inv exp req (Some(get_ctree_element h))
	else
	  octree_S_inv exp req (Some(DbCTreeElt.dbget h))
      else
	raise Not_found
  | Some(CLeaf([],hl)) ->
      raise Not_found
  | Some(CLeaf(false::bl,hl)) -> (Some(CLeaf(bl,hl)),None)
  | Some(CLeaf(true::bl,hl)) -> (None,Some(CLeaf(bl,hl)))
  | Some(CLeft(c0)) -> (Some(c0),None)
  | Some(CRight(c1)) -> (None,Some(c1))
  | Some(CBin(c0,c1)) -> (Some(c0),Some(c1))

let rec tx_octree_trans_ exp req n inpl outpl c =
  if inpl = [] && outpl = [] then
    c
  else if n > 0 then
    begin
      match octree_S_inv exp req c with
      | (c0,c1) ->
	  match
	    tx_octree_trans_ exp req (n-1) (strip_bitseq_false inpl) (strip_bitseq_false outpl) c0,
	    tx_octree_trans_ exp req (n-1) (strip_bitseq_true inpl) (strip_bitseq_true outpl) c1
	  with
	  | None,None -> None
	  | Some(CLeaf(bl,hl)),None -> Some(CLeaf(false::bl,hl))
	  | Some(c0r),None -> Some(CLeft(c0r))
	  | None,Some(CLeaf(bl,hl)) -> Some(CLeaf(true::bl,hl))
	  | None,Some(c1r) -> Some(CRight(c1r))
	  | Some(c0r),Some(c1r) -> Some(CBin(c0r,c1r))
    end
  else
    begin
      let hl =
	begin
	  match c with
	  | Some(CLeaf([],hl)) -> nehlist_hlist hl
	  | None -> HNil
	  | _ -> raise (Failure "not a ctree 0")
	end
      in
      let hl2 = remove_assets_hlist exp req hl (List.map (fun (x,y) -> y) inpl) in
      if List.length outpl + hlist_len hl2 > max_assets_at_address then raise MaxAssetsAtAddress;
      let hl3 = hlist_new_assets (List.map (fun (x,y) -> y) outpl) hl2 in
      match hl3 with
      | HNil -> None
      | HHash(h,l) -> Some(CLeaf([],NehHash(h,l)))
      | HCons(a,hr) -> Some(CLeaf([],NehCons(a,hr)))
      | HConsH(h,hr) -> Some(CLeaf([],NehConsH(h,hr)))
    end

let add_vout bh txh outpl =
  let i = ref 0 in
  let r = ref [] in
  List.iter
    (fun (alpha,(obl,u)) ->
      r := (addr_bitseq alpha,(hashpair txh (hashint32 (Int32.of_int !i)),bh,obl,u))::!r;
      incr i;
    )
    outpl;
  !r

let tx_octree_trans exp req bh tx c =
  let (inpl,outpl) = tx in
  tx_octree_trans_ exp req 162
    (List.map (fun (alpha,h) -> (addr_bitseq alpha,h)) inpl)
    (add_vout bh (hashtx tx) outpl)
    c

let rec txl_octree_trans exp req bh txl c =
  match txl with
  | (tx::txr) -> txl_octree_trans exp req bh txr (tx_octree_trans exp req bh tx c)
  | [] -> c

let rec expand_hlist req hl z =
  match hl,z with
  | _,Some(i) when i <= 0 ->
      begin
	match hlist_hashroot hl with
	| Some(h,l) -> HHash(h,l)
	| None -> HNil
      end
  | HNil,_ -> HNil
  | HHash(h,l),_ ->
      begin
	match if req then get_hcons_element h else DbHConsElt.dbget h with
	| (h1,Some(h2,l2)) -> expand_hlist req (HConsH(h1,HHash(h2,l2))) z
	| (h1,None) -> expand_hlist req (HConsH(h1,HNil)) z
      end
  | HCons(a,hr),None -> HCons(a,expand_hlist req hr None)
  | HCons(a,hr),Some(i) -> HCons(a,expand_hlist req hr (Some(i-1)))
  | HConsH(h,hr),None ->
      let a = if req then get_asset h else DbAsset.dbget h in
      HCons(a,expand_hlist req hr None)
  | HConsH(h,hr),Some(i) ->
      let a = if req then get_asset h else DbAsset.dbget h in
      HCons(a,expand_hlist req hr (Some(i-1)))

let rec expand_nehlist req hl z =
  match hl,z with
  | _,Some(i) when i <= 0 ->
      let (h,l) = nehlist_hashroot hl in
      NehHash(h,l)
  | NehHash(h,l),_ ->
      begin
	match if req then get_hcons_element h else DbHConsElt.dbget h with
	| (h1,Some(h2,l2)) -> expand_nehlist req (NehConsH(h1,HHash(h2,l2))) z
	| (h1,None) -> expand_nehlist req (NehConsH(h1,HNil)) z
      end
  | NehCons(a,hr),None -> NehCons(a,expand_hlist req hr None)
  | NehCons(a,hr),Some(i) -> NehCons(a,expand_hlist req hr (Some(i-1)))
  | NehConsH(h,hr),None -> NehCons(get_asset h,expand_hlist req hr None)
  | NehConsH(h,hr),Some(i) -> NehCons(get_asset h,expand_hlist req hr (Some(i-1)))

let rec truncate_hlist hl i =
  if i <= 0 then
    match hlist_hashroot hl with
    | Some(h,l) -> HHash(h,l)
    | None -> HNil
  else
    match hl with
    | HCons(a,hr) -> HCons(a,truncate_hlist hr (i-1))
    | HConsH(h,hr) -> HConsH(h,truncate_hlist hr (i-1))
    | _ -> hl

let truncate_nehlist hl i =
  if i <= 0 then
    let (h,l) = nehlist_hashroot hl in
    NehHash(h,l)
  else
    match hl with
    | NehCons(a,hr) -> NehCons(a,truncate_hlist hr (i-1))
    | NehConsH(h,hr) -> NehConsH(h,truncate_hlist hr (i-1))
    | _ -> hl

let rec ctree_pre ocache exp req bl c d z =
  match bl with
  | [] ->
      begin
	match c with
	| CLeaf([],hl) ->
	    if exp then
	      (Some(expand_nehlist req hl z),d)
	    else
	      begin
		match z with
		| Some(i) -> (Some(truncate_nehlist hl i),d)
		| None -> (Some(hl),d)
	      end
	| _ -> (None,d)
      end
  | (b::br) ->
      match c with
      | CLeaf(bl2,hl) ->
	  if bl = bl2 then
	    if exp then
	      (Some(expand_nehlist req hl z),d)
	    else
	      (Some(hl),d)
	  else
	    (None,d)
      | CLeft(c0) -> if b then (None,d) else ctree_pre ocache exp req br c0 (d+1) z
      | CRight(c1) -> if b then ctree_pre ocache exp req br c1 (d+1) z else (None,d)
      | CBin(c0,c1) -> if b then ctree_pre ocache exp req br c1 (d+1) z else ctree_pre ocache exp req br c0 (d+1) z
      | CHash(h) ->
	  match ocache with
	  | None -> ctree_pre None exp req bl (expand_ctree_element req h) d z
	  | Some(cache) ->
	      begin
		try
		  Hashtbl.find cache h
		with Not_found ->
		  let r = ctree_pre ocache exp req bl (expand_ctree_element req h) d z in
		  Hashtbl.add cache h r;
		  r
	      end

let ctree_addr exp req alpha c z =
  ctree_pre None exp req (addr_bitseq alpha) c 0 z

let ctree_addr_cache h exp req alpha c z =
  ctree_pre (Some(h)) exp req (addr_bitseq alpha) c 0 z

exception InsufficientInformation

let rec process_unused_ctrees_1 a c =
   match c with
   | CLeft(cl) ->
     process_unused_ctrees_1 a cl
   | CRight(cr) ->
     process_unused_ctrees_1 a cr
   | CBin(cl,cr) ->
     process_unused_ctrees_1 a cl;
     process_unused_ctrees_1 a cr
   | _ -> ()

let rec process_unused_ctrees_2 a c1 c2 =
   match c1 with
   | CLeft(c1l) ->
     begin
       match c2 with
       | CLeft(c2l) -> process_unused_ctrees_2 a c1l c2l
       | CBin(c2l,c2r) -> process_unused_ctrees_2 a c1l c2l
       | CLeaf((b::bl),hl) when not b -> process_unused_ctrees_2 a c1l (CLeaf(bl,hl))
       | _ -> process_unused_ctrees_1 a c1l
     end
   | CRight(c1r) ->
     begin
       match c2 with
       | CRight(c2r) -> process_unused_ctrees_2 a c1r c2r
       | CBin(c2l,c2r) -> process_unused_ctrees_2 a c1r c2r
       | CLeaf((b::bl),hl) when b -> process_unused_ctrees_2 a c1r (CLeaf(bl,hl))
       | _ -> process_unused_ctrees_1 a c1r
     end
   | CBin(c1l,c1r) ->
     begin
       match c2 with
       | CLeft(c2l) ->
         process_unused_ctrees_2 a c1l c2l;
         process_unused_ctrees_1 a c1r
       | CRight(c2r) ->
         process_unused_ctrees_1 a c1l;
         process_unused_ctrees_2 a c1r c2r
       | CBin(c2l,c2r) ->
         process_unused_ctrees_2 a c1l c2l;
         process_unused_ctrees_2 a c1r c2r
       | CLeaf((b::bl),hl) when not b ->
         process_unused_ctrees_2 a c1l (CLeaf(bl,hl));
         process_unused_ctrees_1 a c1r
       | CLeaf((b::bl),hl) when b ->
         process_unused_ctrees_1 a c1l;
         process_unused_ctrees_2 a c1r (CLeaf(bl,hl))
       | _ ->
         process_unused_ctrees_1 a c1l;
         process_unused_ctrees_1 a c1r
     end
   | _ -> ()

let ctree_rights_balanced tr ownr rtot1 rtot2 rtot3 outpl =
  match ownr with
  | Some(beta,None) -> (*** Owner does not allow right to use. Rights may have been obtained in the past. ***)
      Int64.add rtot1 rtot2 = rtot3
  | Some(beta,Some(r)) -> (*** Owner possibly requiring royalties (r = 0L if it is free to use) ***)
      if r > 0L then
	let rtot4 = Int64.div (units_sent_to_addr (payaddr_addr beta) outpl) r in
	Int64.add rtot1 rtot2 = Int64.add rtot3 rtot4
      else
	true (*** If it's free to use, people are free to use or create rights as they please. ***)
  | None -> false (*** No owner, in this case we shouldn't even be here ***)

let rec hlist_full_approx exp req hl =
  match hl with
  | HNil -> true
  | HCons(a,hr) -> hlist_full_approx exp req hr
  | HConsH(h,hr) ->
      if exp then
	if req then
	  begin
	    ignore (get_asset h);
	    hlist_full_approx exp req hr
	  end
	else
	  begin
	    if DbAsset.dbexists h then
	      hlist_full_approx exp req hr
	    else
	      raise Not_found	      
	  end
      else
	false
  | HHash(h,l) ->
      if exp then
	if req then
	  begin
	    match get_hcons_element h with
	    | (h1,Some(h2,l2)) -> hlist_full_approx exp req (HHash(h2,l2))
	    | (h1,None) -> true
	  end
	else
	  begin
	    match DbHConsElt.dbget h with
	    | (h1,Some(h2,l2)) -> hlist_full_approx exp req (HHash(h2,l2))
	    | (h1,None) -> true
	  end
      else
	false

(** exp: bool indicating if hashes should be expanded, req: bool indicating if missing hashes should be requested from peers;
  raises Not_found if exp was true but a hash was not in the database;
  raises GettingRemoteData if exp and req were true, a hash was not in the database and it is being requested from peers
 **)
let nehlist_full_approx exp req hl =
  match hl with
  | NehCons(a,hr) -> hlist_full_approx exp req hr
  | NehConsH(h,hr) ->
      if exp then
	if req then
	  begin
	    ignore (get_asset h);
	    hlist_full_approx exp req hr
	  end
	else
	  begin
	    if DbAsset.dbexists h then
	      hlist_full_approx exp req hr
	    else
	      raise Not_found	      
	  end
      else
	false
  | NehHash(h,l) ->
      if exp then
	if req then
	  begin
	    match get_hcons_element h with
	    | (h1,Some(h2,l2)) -> hlist_full_approx exp req (HHash(h2,l2))
	    | (h1,None) -> true
	  end
	else
	  begin
	    match DbHConsElt.dbget h with
	    | (h1,Some(h2,l2)) -> hlist_full_approx exp req (HHash(h2,l2))
	    | (h1,None) -> true
	  end
      else
	false

let rec ctree_full_approx_addr exp req tr bl =
  match tr with
  | CLeaf(br,hl) when br = bl -> nehlist_full_approx exp req hl
  | CLeaf(_,_) -> true (*** fully approximates because we know it's empty ***)
  | CHash(h) ->
      if exp then
	if req then
	  ctree_full_approx_addr exp req (get_ctree_element h) bl
	else
	  ctree_full_approx_addr exp req (DbCTreeElt.dbget h) bl
      else
	false
  | CLeft(trl) ->
      begin
	match bl with
	| (false::br) -> ctree_full_approx_addr exp req trl br
	| _ -> true (*** fully approximates because we know it's empty ***)
      end
  | CRight(trr) ->
      begin
	match bl with
	| (true::br) -> ctree_full_approx_addr exp req trr br
	| _ -> true (*** fully approximates because we know it's empty ***)
      end
  | CBin(trl,trr) ->
      begin
	match bl with
	| (false::br) -> ctree_full_approx_addr exp req trl br
	| (true::br) -> ctree_full_approx_addr exp req trr br
	| [] -> raise (Failure "Level problem") (*** should never happen ***)
      end

let rec ctree_supports_addr exp req tr bl =
  match tr with
  | CLeaf(_,_) -> true
  | CHash(h) ->
      if exp then
	if req then
	  ctree_supports_addr exp req (get_ctree_element h) bl
	else
	  ctree_supports_addr exp req (DbCTreeElt.dbget h) bl
      else
	false
  | CLeft(trl) ->
      begin
	match bl with
	| (false::br) -> ctree_supports_addr exp req trl br
	| _ -> true (*** supports since known to be empty ***)
      end
  | CRight(trr) ->
      begin
	match bl with
	| (true::br) -> ctree_supports_addr exp req trr br
	| _ -> true (*** supports since known to be empty ***)
      end
  | CBin(trl,trr) ->
      begin
	match bl with
	| (false::br) -> ctree_supports_addr exp req trl br
	| (true::br) -> ctree_supports_addr exp req trr br
	| [] -> raise (Failure "Level problem") (*** should never happen ***)
      end

let rec ctree_supports_asset exp req a tr bl =
  match tr with
  | CLeaf(br,hl) when br = bl -> in_nehlist a hl
  | CLeaf(_,_) -> false
  | CHash(h) ->
      if exp then
	if req then
	  ctree_supports_asset exp req a (get_ctree_element h) bl
	else
	  ctree_supports_asset exp req a (DbCTreeElt.dbget h) bl
      else
	false
  | CLeft(trl) ->
      begin
	match bl with
	| (false::br) -> ctree_supports_asset exp req a trl br
	| _ -> false
      end
  | CRight(trr) ->
      begin
	match bl with
	| (true::br) -> ctree_supports_asset exp req a trr br
	| _ -> false
      end
  | CBin(trl,trr) ->
      begin
	match bl with
	| (false::br) -> ctree_supports_asset exp req a trl br
	| (true::br) -> ctree_supports_asset exp req a trr br
	| [] -> raise (Failure "Level problem") (*** should never happen ***)
      end

let rec ctree_lookup_asset_gen exp req p tr bl =
  match tr with
  | CLeaf(br,hl) when br = bl ->
      nehlist_lookup_asset_gen exp req p hl
  | CLeaf(_,_) ->
      None
  | CHash(h) ->
      if exp then
	if req then
	  ctree_lookup_asset_gen exp req p (get_ctree_element h) bl
	else
	  ctree_lookup_asset_gen exp req p (DbCTreeElt.dbget h) bl
      else
	None
  | CLeft(trl) ->
      begin
	match bl with
	| (false::br) -> ctree_lookup_asset_gen exp req p trl br
	| _ -> None
      end
  | CRight(trr) ->
      begin
	match bl with
	| (true::br) -> ctree_lookup_asset_gen exp req p trr br
	| _ -> None
      end
  | CBin(trl,trr) ->
      begin
	match bl with
	| (false::br) -> ctree_lookup_asset_gen exp req p trl br
	| (true::br) -> ctree_lookup_asset_gen exp req p trr br
	| [] -> raise (Failure "Level problem") (*** should never happen ***)
      end

let ctree_lookup_asset exp req k tr bl = ctree_lookup_asset_gen exp req (fun a -> assetid a = k) tr bl
let ctree_lookup_marker exp req tr bl = ctree_lookup_asset_gen exp req (fun a -> assetpre a = Marker) tr bl

let rec ctree_lookup_addr_assets exp req tr bl =
  match tr with
  | CLeaf(br,hl) when br = bl -> nehlist_hlist hl
  | CLeaf(_,_) -> HNil
  | CHash(h) ->
      if exp then
	if req then
	  ctree_lookup_addr_assets exp req (get_ctree_element h) bl
	else
	  ctree_lookup_addr_assets exp req (DbCTreeElt.dbget h) bl
      else
	HNil
  | CLeft(trl) ->
      begin
	match bl with
	| (false::br) -> ctree_lookup_addr_assets exp req trl br
	| _ -> HNil
      end
  | CRight(trr) ->
      begin
	match bl with
	| (true::br) -> ctree_lookup_addr_assets exp req trr br
	| _ -> HNil
      end
  | CBin(trl,trr) ->
      begin
	match bl with
	| (false::br) -> ctree_lookup_addr_assets exp req trl br
	| (true::br) -> ctree_lookup_addr_assets exp req trr br
	| [] -> raise (Failure "Level problem") (*** should never happen ***)
      end

exception NotSupported

let verbose_supportedcheck = ref None

let vmsg f =
  match !verbose_supportedcheck with
  | None -> ()
  | Some(oc) -> f oc

(*** exp is a boolean indicating whether expanding hash abbrevs should be tried ***)
(*** req is a boolean indicating whether or not missing data should be requested of peers ***)
let rec ctree_lookup_input_assets exp req inpl tr nsf =
  match inpl with
  | [] -> []
  | (alpha,k)::inpr ->
      match ctree_lookup_asset exp req k tr (addr_bitseq alpha) with
      | Some(a) -> (alpha,a)::ctree_lookup_input_assets exp req inpr tr nsf
      | None ->
	  nsf alpha k;
	  raise NotSupported

(*** exp is a boolean indicating whether expanding hash abbrevs should be tried ***)
(*** req is a boolean indicating whether or not missing data should be requested of peers ***)
let rec ctree_supports_output_addrs exp req outpl tr =
  match outpl with
  | (alpha,_)::outpr ->
      if ctree_supports_addr exp req tr (addr_bitseq alpha) then
	ctree_supports_output_addrs exp req outpr tr
      else
	raise NotSupported
  | [] -> ()

(*** return the fee (negative) or reward (positive) if supports tx, otherwise raise NotSupported ***)
(*** this does not request remote data and does not allow local expansions of hash abbrevs ***)
let ctree_supports_tx_2 exp req tht sigt blkh tx aal al tr =
  let (inpl,outpl) = tx in
  (*** Each output address must be supported. ***)
  ctree_supports_output_addrs exp req outpl tr;
  let objids = obj_rights_mentioned outpl in
  let propids = prop_rights_mentioned outpl in
  let susesobjs = output_signaspec_uses_objs outpl in
  let susesprops = output_signaspec_uses_props outpl in
  let usesobjs = output_doc_uses_objs outpl in
  let usesprops = output_doc_uses_props outpl in
  let createsobjs = output_creates_objs outpl in
  let createsprops = output_creates_props outpl in
  let createsobjsids1 = List.map (fun (th,h,k) -> h) createsobjs in
  let createspropsids1 = List.map (fun (th,h) -> h) createsprops in
  let createsobjsids2 = List.map (fun (th,h,k) -> hashtag (hashopair2 th (hashpair h k)) 32l) createsobjs in
  let createspropsids2 = List.map (fun (th,h) -> hashtag (hashopair2 th h) 33l) createsprops in
  let createsnegpropsaddrs2 = List.map (fun (th,h) -> hashval_term_addr (hashtag (hashopair2 th h) 33l)) (output_creates_neg_props outpl) in
  (*** If an object or prop is included in a signaspec, then it must be royalty-free to use. ***)
  List.iter (fun (alphapure,alphathy) ->
    let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq (termaddr_addr (hashval_md160 alphapure))) in
    match hlist_lookup_obj_owner exp req alphapure hl with
    | Some(_,Some(r)) when r = 0L ->
	begin
	  let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq (termaddr_addr (hashval_md160 alphathy))) in
	  match hlist_lookup_obj_owner exp req alphathy hl with
	  | Some(_,Some(r)) when r = 0L -> ()
	  | _ -> raise NotSupported
	end
    | _ -> raise NotSupported
    )
    susesobjs;
  List.iter (fun (alphapure,alphathy) ->
    let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq (termaddr_addr (hashval_md160 alphapure))) in
    match hlist_lookup_prop_owner exp req alphapure hl with
    | Some(_,Some(r)) when r = 0L ->
	begin
	  let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq (termaddr_addr (hashval_md160 alphathy))) in
	  match hlist_lookup_prop_owner exp req alphathy hl with
	  | Some(_,Some(r)) when r = 0L -> ()
	  | _ -> raise NotSupported
	end
    | _ -> raise NotSupported
    )
    susesprops;
  (*** If rights are consumed in the input, then they must be mentioned in the output. ***)
  List.iter (fun a ->
    match a with
    | (_,_,_,RightsObj(h,n)) ->
	if not (List.mem h objids) then
	  raise NotSupported
    | (_,_,_,RightsProp(h,n)) ->
	if not (List.mem h propids) then
	  raise NotSupported
    | _ -> ()
	    )
    al;
  (*** ensure rights are balanced ***)
  List.iter (fun oid ->
    let alpha = hashval_md160 oid in
    let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq (termaddr_addr alpha)) in
    if hlist_full_approx exp req hl &&
      ctree_rights_balanced tr (hlist_lookup_obj_owner exp req oid hl)
	(Int64.of_int (count_rights_used usesobjs oid))
	(rights_out_obj outpl oid)
	(count_obj_rights al oid)
	outpl
    then
      ()
    else
      begin
	vmsg (fun oc -> Printf.fprintf oc "Rights for object %s are not balanced.\n" (Cryptocurr.addr_daliladdrstr (termaddr_addr alpha)));
	raise NotSupported
      end)
    objids;
  List.iter (fun pid ->
    let alpha = hashval_md160 pid in
    let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq (termaddr_addr alpha)) in
    if hlist_full_approx exp req hl &&
      ctree_rights_balanced tr (hlist_lookup_prop_owner exp req pid hl)
	(Int64.of_int (count_rights_used usesprops pid))
	(rights_out_prop outpl pid)
	(count_prop_rights al pid)
	outpl
    then
      ()
    else
      begin
	vmsg (fun oc -> Printf.fprintf oc "Rights for proposition %s are not balanced.\n" (Cryptocurr.addr_daliladdrstr (termaddr_addr alpha)));
	raise NotSupported
      end)
    propids;
  (*** publications are correct, new, and were declared in advance by placing a marker in the right pubaddr ***)
  let ensure_addr_empty alpha =
    match ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) with
    | HNil -> ()
    | _ ->
	vmsg (fun oc -> Printf.fprintf oc "Document has already been published at %s.\n" (Cryptocurr.addr_daliladdrstr alpha));
	raise NotSupported
  in
  let spentmarkersjustified = ref [] in
  List.iter
    (fun (alpha,(obl,u)) ->
      match u with
      | TheoryPublication(gamma,nonce,thy) ->
	  begin
	    ensure_addr_empty alpha; (*** make sure the publication is new because otherwise publishing it is pointless ***)
	    try
	      ignore (match check_theoryspec thy with
              | None ->
		  vmsg (fun oc -> Printf.fprintf oc "Theory does not check as correct\n");
		  raise CheckingFailure
              | _ -> ());
	      match hashtheory (theoryspec_theory thy) with
	      | Some(thyh) ->
		  let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce thyh)) in
		  begin
		    try
		      match
			List.find
			  (fun a ->
			    match a with
			    | (h,bday,obl,Marker) -> List.mem (beta,h) inpl 
			    | _ -> false
			  )
			  al
		      with (h,bday,_,_) ->
			if Int64.add bday intention_minage <= blkh then
			  spentmarkersjustified := h::!spentmarkersjustified
			else
			  begin
			    vmsg (fun oc -> Printf.fprintf oc "Marker %s at %s cannot be spent until block %Ld\n" (hashval_hexstring h) (Cryptocurr.addr_daliladdrstr beta) (Int64.add bday intention_minage));
			    raise NotSupported
			  end
		    with Not_found ->
		      vmsg (fun oc -> Printf.fprintf oc "No Spent Marker at %s to Publish Theory\n" (Cryptocurr.addr_daliladdrstr beta));
		      raise NotSupported
		  end
	      | None -> raise NotSupported
	    with
	    | CheckingFailure -> raise NotSupported
	    | NonNormalTerm -> raise NotSupported
	    | Not_found -> raise NotSupported
	  end
      | SignaPublication(gamma,nonce,th,sl) ->
	  begin
	    ensure_addr_empty alpha; (*** make sure the publication is new because otherwise publishing it is pointless ***)
	    try
	      let gvtp th h a =
		let oid = hashtag (hashopair2 th (hashpair h (hashtp a))) 32l in
		let alpha = hashval_term_addr oid in
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		match hlist_lookup_obj_owner exp req oid hl with
		| Some(beta,r) -> true
		| None -> false
	      in
	      let gvkn th k =
		let pid = hashtag (hashopair2 th k) 33l in
		let alpha = hashval_term_addr pid in
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		match hlist_lookup_prop_owner exp req pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		| Some(beta,r) -> true
		| None -> false
	      in
	      let thy = ottree_lookup tht th in
              ignore (match check_signaspec gvtp gvkn th thy sigt sl with
              | None ->
		  vmsg (fun oc -> Printf.fprintf oc "Signature does not check as correct\n");
		  raise CheckingFailure
              | _ -> ());
	      let slh = hashsigna (signaspec_signa sl) in
	      let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th slh))) in
	      begin
		try
		  match
		    List.find
		      (fun a ->
			match a with
			| (h,bday,obl,Marker) -> List.mem (beta,h) inpl
			| _ -> false
		      )
		      al
		  with (h,bday,_,_) ->
		    if Int64.add bday intention_minage <= blkh then
		      spentmarkersjustified := h::!spentmarkersjustified
		    else
		      begin
			vmsg (fun oc -> Printf.fprintf oc "Marker %s at %s cannot be spent until block %Ld\n" (hashval_hexstring h) (Cryptocurr.addr_daliladdrstr beta) (Int64.add bday intention_minage));
			raise NotSupported
		      end
		with Not_found ->
		  vmsg (fun oc -> Printf.fprintf oc "No Spent Marker at %s to Publish Signature\n" (Cryptocurr.addr_daliladdrstr beta));
		  raise NotSupported
	      end
	    with
	    | CheckingFailure -> raise NotSupported
	    | NonNormalTerm -> raise NotSupported
	  end
      | DocPublication(gamma,nonce,th,dl) ->
	  begin
	    ensure_addr_empty alpha; (*** make sure the publication is new because otherwise publishing it is pointless ***)
	    try
	      let gvtp th h a =
		let oid = hashtag (hashopair2 th (hashpair h (hashtp a))) 32l in
		let alpha = hashval_term_addr oid in
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		match hlist_lookup_obj_owner exp req oid hl with
		| Some(beta,r) -> true
		| None -> false
	      in
	      let gvkn th k =
		let pid = hashtag (hashopair2 th k) 33l in
		let alpha = hashval_term_addr pid in
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		match hlist_lookup_prop_owner exp req pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		| Some(beta,r) -> true
		| None -> false
	      in
	      let thy = ottree_lookup tht th in
              ignore (match check_doc gvtp gvkn th thy sigt dl with
              | None ->
		  vmsg (fun oc -> Printf.fprintf oc "Document does not check as correct\n");
		  raise CheckingFailure
              | _ -> ());
	      let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th (hashdoc dl)))) in
	      begin
		try
		  match
		    List.find
		      (fun a ->
			match a with
			| (h,bday,obl,Marker) -> List.mem (beta,h) inpl
			| _ -> false
		      )
		      al
		  with (h,bday,_,_) ->
		    if Int64.add bday intention_minage <= blkh then
		      spentmarkersjustified := h::!spentmarkersjustified
		    else
		      begin
			vmsg (fun oc -> Printf.fprintf oc "Marker %s at %s cannot be spent until block %Ld\n" (hashval_hexstring h) (Cryptocurr.addr_daliladdrstr beta) (Int64.add bday intention_minage));
			raise NotSupported
		      end		      
		with Not_found ->
		  vmsg (fun oc -> Printf.fprintf oc "No Spent Marker at %s to Publish Document\n" (Cryptocurr.addr_daliladdrstr beta));
		  raise NotSupported
	      end
	    with
	    | CheckingFailure -> raise NotSupported
	    | NonNormalTerm -> raise NotSupported
	  end
      | _ -> ()
    )
    outpl;
  (*** Every spent Marker corresponds to a publication in the output ***)
  List.iter
    (fun (h,bday,obl,u) ->
      match u with
      | Marker ->
	  if not (List.mem h !spentmarkersjustified) then
	    begin
	      vmsg (fun oc -> Printf.fprintf oc "Spent Marker is not being used to publish.\n");
	      raise NotSupported
	    end
      | _ -> ())
    al;
  (*** If an ownership asset is spent in the input, then it must be included as an output.
       Once a hashval at a termaddr is owned by someone, it must remain owned by someone. ***)
  List.iter
    (fun (alpha,(h,bday,obl,u)) ->
      match u with
      | OwnsObj(oid,beta,r) ->
	  begin
	    try
	      ignore (List.find
			(fun (alpha2,(obl2,u2)) ->
			  alpha = alpha2 &&
			  match u2 with
			  | OwnsObj(oid2,beta2,r2) when oid = oid2 -> true
			  | _ -> false)
			outpl)
	    with Not_found ->
	      vmsg (fun oc -> Printf.fprintf oc "OwnsObj %s not in output and so would be destroyed.\n" (Cryptocurr.addr_daliladdrstr alpha));
	      raise NotSupported
	  end
      | OwnsProp(pid,beta,r) ->
	  begin
	    try
	      ignore (List.find
			(fun (alpha2,(obl2,u2)) ->
			  alpha = alpha2 &&
			  match u2 with
			  | OwnsProp(pid2,beta2,r2) when pid = pid2 -> true
			  | _ -> false)
			outpl)
	    with Not_found ->
	      vmsg (fun oc -> Printf.fprintf oc "OwnsProp %s not in output and so would be destroyed.\n" (Cryptocurr.addr_daliladdrstr alpha));
	      raise NotSupported
	  end
      | OwnsNegProp ->
	  begin
	    try
	      ignore (List.find
			(fun (alpha2,(obl2,u2)) ->
			  alpha = alpha2 &&
			  match u2 with
			  | OwnsNegProp -> true
			  | _ -> false)
			outpl)
	    with Not_found ->
	      vmsg (fun oc -> Printf.fprintf oc "OwnsNegProp %s not in output and so would be destroyed.\n" (Cryptocurr.addr_daliladdrstr alpha));
	      raise NotSupported
	  end
      | _ -> ()
    )
    aal;
  (*** newly claimed ownership must be new and supported by a document in the tx, and must not be claimed more than once
       (Since the publisher of the document must sign the tx, the publisher agrees to this ownership declaration.)
       Also, ensure that each ownership asset has an explicit obligation for transfering it.
       The p2pkh or p2sh addr in this obligation is the owner in the sense of who can transfer it and who can collect bounties.
       The p2pkh or p2sh addr listed with the asset is the address which must be paid to buy rights to use the object or proposition.
   ***)
  let ownobjclaims = ref [] in
  let ownpropclaims = ref [] in
  let ownnegpropclaims = ref [] in
  let checkoblnonrew obl = (*** for ownership assets: insist on an obligation, or the ownership will not be transferable; also don't allow it to be indicated as a reward ***)
    match obl with
    | Some(_,_,b) when not b -> ()
    | _ ->
	vmsg (fun oc -> Printf.fprintf oc "Ownership asset must have explicit (non-reward) obligation.\n");
	raise NotSupported
  in
  List.iter
    (fun (alpha,(obl,u)) ->
      match u with
      | OwnsObj(oid,beta,r) ->
	  begin
	    if not (termaddr_addr (hashval_md160 oid) = alpha) then
	      begin
		vmsg (fun oc -> Printf.fprintf oc "OwnsObj %s should be sent to address %s not %s.\n" (hashval_hexstring oid) (Cryptocurr.addr_daliladdrstr (termaddr_addr (hashval_md160 oid))) (Cryptocurr.addr_daliladdrstr alpha));
		raise NotSupported (*** the term address holding the ownership asset must be the 160-bit digest of the object's (256 bit) id ***)
	      end;
	    checkoblnonrew obl;
	    try
	      ignore
		(List.find
		   (fun (alpha1,(_,_,_,u1)) ->
		     alpha = alpha1 &&
		     match u1 with
		     | OwnsObj(oid2,_,_) when oid = oid2 -> true
		     | _ -> false)
		   aal); (*** if the ownership is being transferred ***)
	      ownobjclaims := oid::!ownobjclaims;
	    with Not_found ->
	      (*** if the ownership is being created ***)
	      if (List.mem oid createsobjsids1 || List.mem oid createsobjsids2) && not (List.mem oid !ownobjclaims) then
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		begin
		  ownobjclaims := oid::!ownobjclaims;
		  match hlist_lookup_obj_owner exp req oid hl with
		  | Some(beta2,r2) ->
		      vmsg (fun oc -> Printf.fprintf oc "Object %s already has owner %s.\n" (hashval_hexstring oid) (Cryptocurr.addr_daliladdrstr (payaddr_addr beta2)));
		      raise NotSupported (*** already owned ***)
		  | None -> ()
		end
	      else
		begin
		  vmsg (fun oc -> Printf.fprintf oc "Creation of OwnsObj %s at %s not justified by publications in tx.\n" (hashval_hexstring oid) (Cryptocurr.addr_daliladdrstr alpha));
		  raise NotSupported
		end
	  end
      | OwnsProp(pid,beta,r) -> 
	  begin
	    if not (termaddr_addr (hashval_md160 pid) = alpha) then
	      begin
		vmsg (fun oc -> Printf.fprintf oc "OwnsProp %s should be sent to address %s not %s.\n" (hashval_hexstring pid) (Cryptocurr.addr_daliladdrstr (termaddr_addr (hashval_md160 pid))) (Cryptocurr.addr_daliladdrstr alpha));
		raise NotSupported (*** the term address holding the ownership asset must be the 160-bit digest of the proposition's (256 bit) id ***)
	      end;
	    checkoblnonrew obl;
	    try
	      ignore
		(List.find
		   (fun (alpha1,(_,_,_,u1)) ->
		     alpha = alpha1 &&
		     match u1 with
		     | OwnsProp(pid1,beta1,r1) when pid = pid1 -> true
		     | _ -> false)
		   aal); (*** if the ownership is being transferred ***)
	      ownpropclaims := pid::!ownpropclaims;
	    with Not_found ->
	      (*** if the ownership is being created ***)
	      if (List.mem pid createspropsids1 || List.mem pid createspropsids2) && not (List.mem pid !ownpropclaims) then
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		begin
		  ownpropclaims := pid::!ownpropclaims;
		  match hlist_lookup_prop_owner exp req pid hl with
		  | Some(beta2,r2) ->
		      vmsg (fun oc -> Printf.fprintf oc "Proposition %s already has owner %s.\n" (hashval_hexstring pid) (Cryptocurr.addr_daliladdrstr (payaddr_addr beta2)));
		      raise NotSupported (*** already owned ***)
		  | None -> ()
		end
	      else
		begin
		  vmsg (fun oc -> Printf.fprintf oc "Creation of OwnsProp %s at %s not justified by publications in tx.\n" (hashval_hexstring pid) (Cryptocurr.addr_daliladdrstr alpha));
		  raise NotSupported
		end
	  end
      | OwnsNegProp -> 
	  begin
	    checkoblnonrew obl; (*** note that even this one needs to be transferable in order to collect bounties ***)
	    try
	      ignore (List.find (fun (alpha1,(_,_,_,u1)) -> u1 = OwnsNegProp && alpha = alpha1) aal); (*** if the ownership is being transferred ***)
	      ownnegpropclaims := alpha::!ownnegpropclaims;
	    with Not_found ->
	      (*** if the ownership is being created ***)
	      if (List.mem alpha createsnegpropsaddrs2) && not (List.mem alpha !ownnegpropclaims) then
		let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
		begin
		  ownnegpropclaims := alpha::!ownnegpropclaims;
		  if hlist_lookup_neg_prop_owner exp req hl then
		    begin
		      vmsg (fun oc -> Printf.fprintf oc "NegProp at %s already has owner.\n" (Cryptocurr.addr_daliladdrstr alpha));
		      raise NotSupported (*** already owned ***)
		    end
		end
	      else
		begin
		  vmsg (fun oc -> Printf.fprintf oc "Creation of OwnsNegProp at %s not justified by publications in tx.\n" (Cryptocurr.addr_daliladdrstr alpha));
		  raise NotSupported
		end;
	  end
      | _ -> ()
    )
    outpl;
  (***
      new objects and props must be given ownership by the tx publishing the document.
   ***)
  List.iter (fun (th,tmh,tph) ->
    try
      let ensureowned oid =
	let alpha = hashval_term_addr oid in
	let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
	match hlist_lookup_obj_owner exp req oid hl with
	| Some(beta2,r2) -> () (*** already owned ***)
	| None -> (*** Since alpha was listed in full_needed we know alpha really isn't owned here ***)
	    (*** ensure that it will be owned after the tx ***)
	    if not (List.mem oid !ownobjclaims) then
	      begin
		vmsg (fun oc -> Printf.fprintf oc "Obj %s at %s newly defined in publication in tx must be given an owner.\n" (hashval_hexstring oid) (Cryptocurr.addr_daliladdrstr alpha));
		raise Not_found
	      end
      in
      let alphapure = tmh in
      let alphathy = hashtag (hashopair2 th (hashpair tmh tph)) 32l in
      ensureowned alphapure;
      ensureowned alphathy
    with Not_found -> raise NotSupported
    )
    createsobjs;
  List.iter (fun (th,tmh) ->
    try
      let ensureowned pid =
	let alpha = hashval_term_addr pid in
	let hl = ctree_lookup_addr_assets exp req tr (addr_bitseq alpha) in
	match hlist_lookup_prop_owner exp req pid hl with
	| Some(beta2,r2) -> () (*** already owned ***)
	| None -> (*** Since alpha was listed in full_needed we know alpha really isn't owned here ***)
	    (*** ensure that it will be owned after the tx ***)
	    if not (List.mem pid !ownpropclaims) then
	      begin
		vmsg (fun oc -> Printf.fprintf oc "Prop %s at %s newly proven in publication in tx must be given an owner.\n" (hashval_hexstring pid) (Cryptocurr.addr_daliladdrstr alpha));
		raise Not_found
	      end
      in
      let alphapure = tmh in
      let alphathy = hashtag (hashopair2 th tmh) 33l in
      ensureowned alphapure;
      ensureowned alphathy
    with Not_found -> raise NotSupported
    )
    createsprops;
  (*** bounties can be collected by the owners of props or negprops
       To make checking this easy, the ownership asset is spent and recreated unchanged (except the asset id).
       Note that address for the relevant signature is in the obligation of the ownership asset.
       Essentially the ownership gets trivially transfered when the bounty is collected.
       Someone can place bounties on pure propositions, but this is a bad idea.
       Someone else could collect it by creating an inconsistent theory and giving a trivial proof.
       Real bounties should only be placed on propositions within a theory.
       Addendum: the check that the ownership asset is being spent has been moved to tx (check_tx_in_signatures)
       with the alternative that if the ownership asset is not being spent and the bounty has a nonempty obligation
       then the address in the obligation has signed the tx. This allows a Bounty to be spent
       by its creator after a certain lockheight (expiration). Since this is being checked in check_tx_in_signatures,
       there is no longer a check here.
   ***)
(***
  List.iter
    (fun (alpha,(h,bday,obl,u)) -> 
      match u with
      | Bounty(v) -> (*** Note: The bounty could be collected due to a hash collision, but this does not seem to be subject to the birthday paradox so it should be safe. Otherwise we should save the propositions id (preimage of alpha) explicitly here. ***)
	  begin
	    try
	      (*** ensure that an owner of the prop or negprop signed the tx because the ownership asset was an input value ***)
	      ignore
		(List.find
		   (fun (alpha2,(h2,bday2,obl2,u2)) -> (*** remember: it's the obligation that determines who signs these; so the obligations tells who the "owners" are for the purpose of collecting bounties ***)
		     alpha = alpha2 &&
		     match u2 with
		     | OwnsProp(pid2,beta2,r2) -> true
		     | OwnsNegProp -> true
		     | _ -> false
		   )
		   aal)
	    with Not_found -> raise NotSupported
	  end
      | _ -> ()
    )
    aal;
***)
  (*** finally, return the number of currency units created or destroyed ***)
  Int64.sub (out_cost outpl) (asset_value_sum blkh al)

let ctree_supports_tx exp req tht sigt blkh tx tr =
  let (inpl,outpl) = tx in
  let aal = ctree_lookup_input_assets exp req inpl tr (fun _ _ -> ()) in
  let al = List.map (fun (_,a) -> a) aal in
  ctree_supports_tx_2 exp req tht sigt blkh tx aal al tr

let rec hlist_lub hl1 hl2 =
  match hl1 with
  | HNil -> HNil
  | HHash(_,_) -> hl2
  | HCons(a1,hr1) ->
      begin
	match hl2 with
	| HNil -> raise (Failure "incompatible hlists")
	| HHash(_,_) -> hl1
	| HCons(_,hr2) -> HCons(a1,hlist_lub hr1 hr2)
	| HConsH(_,hr2) -> HCons(a1,hlist_lub hr1 hr2)
      end
  | HConsH(h1,hr1) ->
      match hl2 with
      | HNil -> raise (Failure "incompatible hlists")
      | HHash(_,_) -> hl1
      | HCons(a2,hr2) -> HCons(a2,hlist_lub hr1 hr2)
      | HConsH(_,hr2) -> HConsH(h1,hlist_lub hr1 hr2)

let nehlist_lub hl1 hl2 =
  match hl1 with
  | NehHash(_,_) -> hl2
  | NehCons(a1,hr1) ->
      begin
	match hl2 with
	| NehHash(_,_) -> hl1
	| NehCons(_,hr2) -> NehCons(a1,hlist_lub hr1 hr2)
	| NehConsH(_,hr2) -> NehCons(a1,hlist_lub hr1 hr2)
      end
  | NehConsH(h1,hr1) ->
      match hl2 with
      | NehHash(_,_) -> hl1
      | NehCons(a2,hr2) -> NehCons(a2,hlist_lub hr1 hr2)
      | NehConsH(_,hr2) -> NehConsH(h1,hlist_lub hr1 hr2)

let rec ctreeLinv c =
  match c with
  | CLeaf(bl,hl) -> Some(bl,hl)
  | CLeft(c0) ->
      begin
	match ctreeLinv c0 with
	| Some(bl,hl) -> Some(false::bl,hl)
	| None -> None
      end
  | CRight(c1) ->
      begin
	match ctreeLinv c1 with
	| Some(bl,hl) -> Some(true::bl,hl)
	| None -> None
      end
  | _ -> None

let rec ctree_singlebranch_lub bl hl c =
  match ctreeLinv c with
  | Some(_,hl2) -> CLeaf(bl,nehlist_lub hl hl2)
  | None -> CLeaf(bl,hl)

let rec ctree_lub c1 c2 =
  match c1 with
  | CHash(_) -> c2
  | CLeaf(bl1,hl1) -> ctree_singlebranch_lub bl1 hl1 c2
  | CLeft(c10) ->
      begin
	match c2 with
	| CHash(_) -> c1
	| CLeaf(bl2,hl2) -> ctree_singlebranch_lub bl2 hl2 c1
	| CLeft(c20) -> CLeft (ctree_lub c10 c20)
	| _ -> raise (Failure "no lub for incompatible ctrees")
      end
  | CRight(c11) ->
      begin
	match c2 with
	| CHash(_) -> c1
	| CLeaf(bl2,hl2) -> ctree_singlebranch_lub bl2 hl2 c1
	| CRight(c21) -> CRight (ctree_lub c11 c21)
	| _ -> raise (Failure "no lub for incompatible ctrees")
      end
  | CBin(c10,c11) ->
      begin
	match c2 with
	| CHash(_) -> c1
	| CBin(c20,c21) -> CBin(ctree_lub c10 c20,ctree_lub c11 c21)
	| _ -> raise (Failure "no lub for incompatible ctrees")
      end

let octree_lub oc1 oc2 =
  match (oc1,oc2) with
  | (Some(c1),Some(c2)) ->
      Some(ctree_lub c1 c2)
  | (None,None) -> None
  | _ -> raise (Failure "no lub for incompatible octrees")

let rec load_expanded_ctree_a c i =
  if i > 0 then
    begin
      match c with
      | CLeft(tr0) -> CLeft(load_expanded_ctree_a tr0 (i-1))
      | CRight(tr1) -> CRight(load_expanded_ctree_a tr1 (i-1))
      | CBin(tr0,tr1) -> CBin(load_expanded_ctree_a tr0 (i-1),load_expanded_ctree_a tr1 (i-1))
      | _ -> c
    end
  else
    load_expanded_ctree c
and load_expanded_ctree c =
  try
    let c2 = load_expanded_ctree_a c 9 in
    let r = ctree_hashroot c2 in
    let ce = DbCTreeElt.dbget r in
    ctree_lub c2 ce
  with Not_found -> c

let load_expanded_octree c =
  match c with
  | Some(c) -> Some(load_expanded_ctree c)
  | None -> None

let rec hlist_reduce_to_min_support aidl hl =
  match aidl with
  | [] ->
      begin
	match hlist_hashroot hl with
	| Some(h,l) -> HHash(h,l)
	| None -> HNil
      end
  | _ ->
      begin
	match hl with
	| HCons((aid,bh,o,u) as a,hr) ->
	    if List.mem aid aidl then
	      HCons(a,hlist_reduce_to_min_support (List.filter (fun z -> not (z = aid)) aidl) hr)
	    else
	      HConsH(hashasset a,hlist_reduce_to_min_support aidl hr)
	| HConsH(h,hr) ->
	    begin
	      try
		let (aid,bh,o,u) = get_asset h in
		if List.mem aid aidl then
		  HCons((aid,bh,o,u),hlist_reduce_to_min_support (List.filter (fun z -> not (z = aid)) aidl) hr)
		else
		  HConsH(h,hlist_reduce_to_min_support aidl hr)
	      with
	      | _ -> HConsH(h,hlist_reduce_to_min_support aidl hr)
	    end
	| HHash(h,_) -> (*** do a partial lookup ***)
	    hlist_reduce_to_min_support aidl (get_hlist_element h)
	| _ -> hl
      end

let rec get_full_hlist hl =
  match hl with
  | HNil -> HNil
  | HCons(a,hr) -> HCons(a,get_full_hlist hr)
  | HConsH(h,hr) -> HCons(get_asset h,get_full_hlist hr)
  | HHash(h,_) -> get_full_hlist (get_hlist_element h)

let rec get_full_nehlist hl =
  match hl with
  | NehCons(a,hr) -> NehCons(a,get_full_hlist hr)
  | NehConsH(h,hr) -> NehCons(get_asset h,get_full_hlist hr)
  | NehHash(h,_) -> get_full_nehlist (get_nehlist_element h)
      
let rec ctree_reduce_to_min_support n inpl outpl full c =
  if n > 0 then
    begin
      if inpl = [] && outpl = [] && full = [] then
	CHash(ctree_hashroot c)
      else
	begin
	  match c with
	  | CLeaf(false::bl,hl) ->
	      begin
		match ctree_reduce_to_min_support (n-1)
		      (strip_bitseq_false inpl)
		      (strip_bitseq_false0 outpl)
		      (strip_bitseq_false0 full)
		      (CLeaf(bl,hl))
		with
		| CLeaf(bl2,hl2) -> CLeaf(false::bl2,hl2)
		| c2 -> CLeft(c2)
	      end
	  | CLeaf(true::bl,hl) ->
	      begin
		match ctree_reduce_to_min_support (n-1)
		      (strip_bitseq_true inpl)
		      (strip_bitseq_true0 outpl)
		      (strip_bitseq_true0 full)
		      (CLeaf(bl,hl))
		with
		| CLeaf(bl2,hl2) -> CLeaf(true::bl2,hl2)
		| c2 -> CRight(c2)
	      end
	  | CLeft(c0) ->
	      CLeft(ctree_reduce_to_min_support (n-1)
		      (strip_bitseq_false inpl)
		      (strip_bitseq_false0 outpl)
		      (strip_bitseq_false0 full)
		      c0)
	  | CRight(c1) ->
	      CRight(ctree_reduce_to_min_support (n-1)
		       (strip_bitseq_true inpl)
		       (strip_bitseq_true0 outpl)
		       (strip_bitseq_true0 full)
		       c1)
	  | CBin(c0,c1) ->
	      CBin(ctree_reduce_to_min_support (n-1)
		     (strip_bitseq_false inpl)
		     (strip_bitseq_false0 outpl)
		     (strip_bitseq_false0 full)
		     c0,
		   ctree_reduce_to_min_support (n-1)
		       (strip_bitseq_true inpl)
		       (strip_bitseq_true0 outpl)
		       (strip_bitseq_true0 full)
		       c1)
	  | CHash(h) -> (*** changed to expand in this case; so the name of the function is misleading ***)
	      ctree_reduce_to_min_support n inpl outpl full (get_ctree_element h)
	  | _ -> c
	end
    end
  else if full = [] then
    begin
      match c with
      | CLeaf([],NehHash(h,_)) -> 
	  if inpl = [] then
	    c
	  else
	    let aidl = List.map (fun (_,k) -> k) inpl in
	    begin
	      match get_nehlist_element h with
	      | NehConsH(h,hr) ->
		  let ((aid,_,_,_) as a) = get_asset h in
		  if List.mem aid aidl then
		    CLeaf([],NehCons(a,hlist_reduce_to_min_support (List.filter (fun z -> not (z = aid)) aidl) hr))
		  else
		    CLeaf([],NehConsH(h,hlist_reduce_to_min_support aidl hr))
	      | _ -> raise (Failure "impossible")
	    end
      | CLeaf([],(NehCons((h,bh,o,u),hr) as hl)) ->
	  if inpl = [] then
	    let (h,l) = nehlist_hashroot hl in
	    CLeaf([],NehHash(h,l))
	  else
	    let aidl = List.map (fun (_,k) -> k) inpl in
	    if List.mem h aidl then
	      CLeaf([],NehCons((h,bh,o,u),hlist_reduce_to_min_support (List.filter (fun z -> not (z = h)) aidl) hr))
	    else
	      CLeaf([],NehConsH(hashasset (h,bh,o,u),hlist_reduce_to_min_support aidl hr))
      | CLeaf([],(NehConsH(h,hr) as hl)) ->
	  if inpl = [] then
	    let (h,l) = nehlist_hashroot hl in
	    CLeaf([],NehHash(h,l))
	  else
	    let aidl = List.map (fun (_,k) -> k) inpl in
	    let ((aid,_,_,_) as a) = get_asset h in
	    if List.mem aid aidl then
	      CLeaf([],NehCons(a,hlist_reduce_to_min_support (List.filter (fun z -> not (z = h)) aidl) hr))
	    else
	      CLeaf([],NehConsH(h,hlist_reduce_to_min_support aidl hr))
      | _ -> raise (Failure "impossible")
    end
  else
    begin
      match c with
      | CLeaf([],hl) -> CLeaf([],get_full_nehlist hl)
      | _ -> raise (Failure "impossible")
    end

let octree_reduce_to_min_support inpl outpl full oc =
  match oc with
  | None -> None
  | Some(c) -> Some (ctree_reduce_to_min_support 162 inpl outpl full c)

let rec full_needed_1 outpl =
  match outpl with
  | [] -> []
  | (_,(o,(RightsObj(h,_))))::outpr -> addr_bitseq (termaddr_addr (hashval_md160 h))::full_needed_1 outpr
  | (_,(o,(RightsProp(h,_))))::outpr -> addr_bitseq (termaddr_addr (hashval_md160 h))::full_needed_1 outpr
  | (alpha,(o,(OwnsObj(_,_,_))))::outpr -> addr_bitseq alpha::full_needed_1 outpr
  | (alpha,(o,(OwnsProp(_,_,_))))::outpr -> addr_bitseq alpha::full_needed_1 outpr
  | (_,(o,TheoryPublication(gamma,nonce,thy)))::outpr ->
      let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr gamma)) (hashopair1 nonce (hashtheory (theoryspec_theory thy)))) in
      addr_bitseq beta::full_needed_1 outpr
  | (_,(o,SignaPublication(gamma,nonce,th,sl)))::outpr ->
      let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th (hashsigna (signaspec_signa sl))))) in
      addr_bitseq beta::full_needed_1 outpr
  | (_,(o,DocPublication(gamma,nonce,th,dl)))::outpr ->
      let beta = hashval_pub_addr (hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th (hashdoc dl)))) in
      addr_bitseq beta::full_needed_1 outpr
  | _::outpr -> full_needed_1 outpr

let full_needed outpl =
  let r = ref (full_needed_1 outpl) in
  List.iter
    (fun (alphapure,alphathy) ->
	r := addr_bitseq (hashval_term_addr alphapure)::addr_bitseq (hashval_term_addr alphathy)::!r)
    (output_signaspec_uses_objs outpl);
  List.iter
    (fun (alphapure,alphathy) ->
	r := addr_bitseq (hashval_term_addr alphapure)::addr_bitseq (hashval_term_addr alphathy)::!r)
    (output_signaspec_uses_props outpl);
  List.iter
    (fun (alphapure,alphathy) ->
	r := addr_bitseq (hashval_term_addr alphapure)::addr_bitseq (hashval_term_addr alphathy)::!r)
    (output_doc_uses_objs outpl);
  List.iter
    (fun (alphapure,alphathy) ->
	r := addr_bitseq (hashval_term_addr alphapure)::addr_bitseq (hashval_term_addr alphathy)::!r)
    (output_doc_uses_props outpl);
  !r

let get_tx_supporting_octree (inpl,outpl) oc =
  octree_reduce_to_min_support
    (List.map (fun (alpha,z) -> (addr_bitseq alpha,z)) inpl)
    (List.map (fun (alpha,(_,_)) -> addr_bitseq alpha) outpl)
    (full_needed outpl)
    oc

let rec get_txl_supporting_octree txl oc =
  match txl with
  | (tx::txr) ->
      octree_lub (get_tx_supporting_octree tx oc) (get_txl_supporting_octree txr oc)
  | [] -> 
      match oc with
      | Some(c) -> Some(CHash(ctree_hashroot c))
      | None -> None

let rec bitseq_prefix bl cl =
  match bl with
  | [] -> true
  | (b::br) ->
      match cl with
      | [] -> false
      | (c::cr) ->
	  if b = c then
	    bitseq_prefix br cr
	  else
	    false

(***
 ensure that the hlist/nehlist/ctree contains no extra information; this is a condition to prevent headers from being
 large by including unnecessary information
 ***)
let rec minimal_asset_supporting_hlist hl aid n =
  if n > 0 then
    match hl with
    | HCons(a,HNil) -> assetid a = aid
    | HCons(a,HHash(_,_)) -> assetid a = aid
    | HConsH(_,hr) -> minimal_asset_supporting_hlist hr aid (n-1)
    | _ -> false
  else
    false

let rec minimal_asset_supporting_nehlist hl aid n =
  if n > 0 then
    match hl with
    | NehCons(a,HNil) -> assetid a = aid
    | NehCons(a,HHash(_,_)) -> assetid a = aid
    | NehConsH(_,hr) -> minimal_asset_supporting_hlist hr aid (n-1)
    | _ -> false
  else
    false

let rec minimal_asset_supporting_ctree tr bl aid n =
  match tr with
  | CLeaf(br,hl) when br = bl -> minimal_asset_supporting_nehlist hl aid n
  | CLeaf(_,_) -> false
  | CHash(_) -> false
  | CLeft(trl) ->
      begin
	match bl with
	| (false::br) -> minimal_asset_supporting_ctree trl br aid n
	| _ -> false
      end
  | CRight(trr) ->
      begin
	match bl with
	| (true::br) -> minimal_asset_supporting_ctree trr br aid n
	| _ -> false
      end
  | CBin(trl,CHash(_)) ->
      begin
	match bl with
	| (false::br) -> minimal_asset_supporting_ctree trl br aid n
	| _ -> false
      end
  | CBin(CHash(_),trr) ->
      begin
	match bl with
	| (true::br) -> minimal_asset_supporting_ctree trr br aid n
	| _ -> false
      end
  | _ -> false;;

Hashtbl.add msgtype_handler GetHConsElement
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetHConsElement in
      let tm = Unix.time() in
      if not (recently_sent (i,h) tm cs.sentinv) then (*** don't resend ***)
	try
	  let hk = DbHConsElt.dbget h in
	  let hksb = Buffer.create 100 in
	  seosbf (seo_prod seo_hashval (seo_option (seo_prod seo_hashval seo_int8)) seosb hk (seo_hashval seosb h (hksb,None)));
	  let hkser = Buffer.contents hksb in
	  ignore (queue_msg cs HConsElement hkser);
	  cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv
	with Not_found -> ());;

Hashtbl.add msgtype_handler HConsElement
    (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetHConsElement in
      if not (DbHConsElt.dbexists h) then (*** if we already have it, abort ***)
	let tm = Unix.time() in
	if recently_requested (i,h) tm cs.invreq then (*** only continue if it was requested ***)
          let (hk,_) = sei_prod sei_hashval (sei_option (sei_prod sei_hashval sei_int8)) seis r in
	  let hkh =
	    match hk with
	    | (h1,None) -> hashtag h1 3l
	    | (h1,Some(k1,l1)) -> hashtag (hashpair h1 k1) (Int32.of_int (4096+l1))
	  in
	  if hkh = h then
	    begin
  	      DbHConsElt.dbput h hk;
	      cs.invreq <- List.filter (fun (j,k,tm0) -> not (i = j && h = k) && tm -. tm0 < 3600.0) cs.invreq
	    end
          else (*** otherwise, it seems to be a misbehaving peer --  ignore for now ***)
	    Utils.log_string (Printf.sprintf "misbehaving peer? [malformed HConsElement]\n")
	else (*** if something unrequested was sent, then seems to be a misbehaving peer ***)
	  Utils.log_string (Printf.sprintf "misbehaving peer? [unrequested HConsElement]\n"));;
	  
Hashtbl.add msgtype_handler GetCTreeElement
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetCTreeElement in
      let tm = Unix.time () in
      if not (recently_sent (i,h) tm cs.sentinv) then (*** don't resend ***)
	try
	  let c = DbCTreeElt.dbget h in
	  let csb = Buffer.create 100 in
	  seosbf (seo_ctree seosb c (seo_hashval seosb h (csb,None)));
	  let cser = Buffer.contents csb in
	  ignore (queue_msg cs CTreeElement cser);
	  cs.sentinv <- (i,h,tm)::List.filter (fun (_,_,tm0) -> tm -. tm0 < 3600.0) cs.sentinv
	with Not_found -> ());;

Hashtbl.add msgtype_handler CTreeElement
    (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetCTreeElement in
      if not (DbCTreeElt.dbexists h) then (*** if we already have it, abort ***)
	let tm = Unix.time () in
	if recently_requested (i,h) tm cs.invreq then (*** only continue if it was requested ***)
          let (c,_) = sei_ctree seis r in
	  if ctree_element_p c && ctree_hashroot c = h then
	    begin
  	      DbCTreeElt.dbput h c;
	      cs.invreq <- List.filter (fun (j,k,tm0) -> not (i = j && h = k) && tm -. tm0 < 3600.0) cs.invreq
	    end
          else (*** otherwise, it seems to be a misbehaving peer --  ignore for now ***)
	    Utils.log_string (Printf.sprintf "misbehaving peer? [malformed CTreeElement]\n")
	else (*** if something unrequested was sent, then seems to be a misbehaving peer ***)
	  Utils.log_string (Printf.sprintf "misbehaving peer? [unrequested CTreeElement]\n"));;

let hashctree c =
  let s = Buffer.create 1000 in
  seosbf (seo_ctree seosb c (s,None));
  Sha256.sha256str (Buffer.contents s)

let rec json_hlist hl =
  match hl with
  | HHash(h,l) -> JsonObj([("type",JsonStr("hlist"));("hlistcase",JsonStr("hhash"));("hhash",JsonStr(hashval_hexstring h));("len",JsonNum(string_of_int l))])
  | HNil -> JsonObj([("type",JsonStr("hlist"));("hlistcase",JsonStr("hnil"))])
  | HCons(a,hl) -> JsonObj([("type",JsonStr("hlist"));("hlistcase",JsonStr("hcons"));("first",json_asset a);("firsthash",JsonStr(hashval_hexstring (hashasset a)));("rest",json_hlist hl)])
  | HConsH(h,hl) -> JsonObj([("type",JsonStr("hlist"));("hlistcase",JsonStr("hconsh"));("firsthash",JsonStr(hashval_hexstring h));("rest",json_hlist hl)])

let json_nehlist hl =
  match hl with
  | NehHash(h,l) -> JsonObj([("type",JsonStr("nehlist"));("nehlistcase",JsonStr("nehhash"));("nehhash",JsonStr(hashval_hexstring h));("len",JsonNum(string_of_int l))])
  | NehCons(a,hl) -> JsonObj([("type",JsonStr("nehlist"));("nehlistcase",JsonStr("nehcons"));("first",json_asset a);("firsthash",JsonStr(hashval_hexstring (hashasset a)));("rest",json_hlist hl)])
  | NehConsH(h,hl) -> JsonObj([("type",JsonStr("nehlist"));("nehlistcase",JsonStr("nehconsh"));("firsthash",JsonStr(hashval_hexstring h));("rest",json_hlist hl)])

let rec json_ctree c =
  match c with
  | CLeaf(bl,hl) -> JsonObj([("type",JsonStr("ctree"));("ctreecase",JsonStr("cleaf"));("bl",JsonArr(List.map (fun b -> JsonBool(b)) bl));("nehlist",json_nehlist hl)])
  | CHash(h) -> JsonObj([("type",JsonStr("ctree"));("ctreecase",JsonStr("chash"));("h",JsonStr(hashval_hexstring h))])
  | CLeft(c0) -> JsonObj([("type",JsonStr("ctree"));("ctreecase",JsonStr("cleft"));("left",json_ctree c0)])
  | CRight(c1) -> JsonObj([("type",JsonStr("ctree"));("ctreecase",JsonStr("cright"));("right",json_ctree c1)])
  | CBin(c0,c1) -> JsonObj([("type",JsonStr("ctree"));("ctreecase",JsonStr("cbin"));("left",json_ctree c0);("right",json_ctree c1)])

let rec hlist_from_json j =
  match j with
  | JsonObj(al) ->
      let hc = List.assoc "hlistcase" al in
      if hc = JsonStr("hhash") then
	begin
	  match List.assoc "hhash" al with
	  | JsonStr(hh) ->
	      let h = hexstring_hashval hh in
	      let l = match List.assoc "len" al with JsonNum(ls) -> int_of_string ls | _ -> raise (Failure("not json of an hlist hash")) in
	      HHash(h,l)
	  | _ ->
	      raise (Failure("not json of an hlist hash"))
	end
      else if hc = JsonStr("hnil") then
	HNil
      else if hc = JsonStr("hcons") then
	begin
	  let a = asset_from_json (List.assoc "first" al) in
	  let hr = hlist_from_json (List.assoc "rest" al) in
	  HCons(a,hr)
	end
      else if hc = JsonStr("hconsh") then
	begin
	  match List.assoc "firsthash" al with
	  | JsonStr(ahh) ->
	      let ah = hexstring_hashval ahh in
	      let hr = hlist_from_json (List.assoc "rest" al) in
	      HConsH(ah,hr)
	  | _ ->
	      raise (Failure("not json of an hlist consh"))
	end
      else
	raise (Failure("not json of an hlist"))
  | _ -> raise (Failure("not json of an hlist"))

let nehlist_from_json j =
  match j with
  | JsonObj(al) ->
      let nehc = List.assoc "nehlistcase" al in
      if nehc = JsonStr("nehhash") then
	begin
	  match List.assoc "nehhash" al with
	  | JsonStr(hh) ->
	      let h = hexstring_hashval hh in
	      let l = match List.assoc "len" al with JsonNum(ls) -> int_of_string ls | _ -> raise (Failure("not json of an nehlist hash")) in
	      NehHash(h,l)
	  | _ ->
	      raise (Failure("not json of an nehlist hash"))
	end
      else if nehc = JsonStr("nehcons") then
	begin
	  let a = asset_from_json (List.assoc "first" al) in
	  let hr = hlist_from_json (List.assoc "rest" al) in
	  NehCons(a,hr)
	end
      else if nehc = JsonStr("nehconsh") then
	begin
	  match List.assoc "firsthash" al with
	  | JsonStr(ahh) ->
	      let ah = hexstring_hashval ahh in
	      let hr = hlist_from_json (List.assoc "rest" al) in
	      NehConsH(ah,hr)
	  | _ ->
	      raise (Failure("not json of an nehlist consh"))
	end
      else
	raise (Failure("not json of an nehlist"))
  | _ -> raise (Failure("not json of an nehlist"))

let rec ctree_from_json j =
  match j with
  | JsonObj(al) ->
      let ctc = List.assoc "ctreecase" al in
      if ctc = JsonStr("cleaf") then
	begin
	  let bl =
	    List.map (fun jb -> match jb with JsonBool(b) -> b | _ -> raise (Failure("not json of a ctree leaf")))
	      (match (List.assoc "bl" al) with
		JsonArr(jbl) -> jbl
	      | _ -> raise (Failure("not json of a ctree leaf")))
	  in
	  let hl = nehlist_from_json (List.assoc "nehlist" al) in
	  CLeaf(bl,hl)
	end
      else if ctc = JsonStr("chash") then
	begin
	  match List.assoc "h" al with
	  | JsonStr(hh) ->
	      let h = hexstring_hashval hh in
	      CHash(h)
	  | _ ->
	      raise (Failure("not json of a ctree hash"))
	end
      else if ctc = JsonStr("cleft") then
	begin
	  let c0 = ctree_from_json (List.assoc "left" al) in
	  CLeft(c0)
	end
      else if ctc = JsonStr("cright") then
	begin
	  let c1 = ctree_from_json (List.assoc "right" al) in
	  CRight(c1)
	end
      else if ctc = JsonStr("cbin") then
	begin
	  let c0 = ctree_from_json (List.assoc "left" al) in
	  let c1 = ctree_from_json (List.assoc "right" al) in
	  CBin(c0,c1)
	end
      else
	raise (Failure("not json of a ctree"))
  | _ ->
      raise (Failure("not json of a ctree"))
