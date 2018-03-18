(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Ser
open Sha256
open Hash
open Htree
open Logic

let printlist l = List.iter (fun ((x, y), z) -> Printf.printf "%s " (hashval_hexstring x)) l

(** ** tp serialization ***)
let rec seo_tp o m c =
  match m with
  | TpArr(m0,m1) -> (*** 00 ***)
    let c = o 2 0 c in
    let c = seo_tp o m0 c in
    let c = seo_tp o m1 c in
    c
  | Prop -> (*** 01 ***)
    o 2 1 c
  | Base(x) when x < 65536 -> (*** 10 ***)
    let c = o 2 2 c in
    seo_varintb o x c
  | Base(_) -> raise (Failure("Invalid base type"))
  | TpVar(x) when x < 65536 -> (*** 11 0 x ***)
    let c = o 3 3 c in
    let c = seo_varintb o x c in
    c
  | TpVar(_) -> raise (Failure("Invalid type variable"))
  | TpAll(a) -> (*** 11 1 ***)
    let c = o 3 7 c in
    let c = seo_tp o a c in
    c

let tp_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_tp seosb m (c,None));
  Buffer.contents c

let hashtp m = hashtag (sha256str (tp_to_str m)) 64l

let rec sei_tp i c =
  let (b,c) = i 2 c in
  if b = 0 then
    let (m0,c) = sei_tp i c in
    let (m1,c) = sei_tp i c in
    (TpArr(m0,m1),c)
  else if b = 1 then
    (Prop,c)
  else if b = 2 then
    let (x,c) = sei_varintb i c in
    (Base(x),c)
  else
    let (y,c) = i 1 c in
    if y = 0 then
      let (x,c) = sei_varintb i c in
      (TpVar(x),c)
    else
      let (m0,c) = sei_tp i c in
      (TpAll(m0),c)

(** ** tp list serialization ***)
let seo_tpl o al c = seo_list seo_tp o al c
let sei_tpl i c = sei_list sei_tp i c

let tpl_to_str al =
  let c = Buffer.create 1000 in
  seosbf (seo_tpl seosb al (c,None));
  Buffer.contents c

let hashtpl al =
  if al = [] then
    None
  else
    Some(hashtag (sha256str (tpl_to_str al)) 65l)

(** ** tm serialization ***)
let rec seo_tm o m c =
  match m with
  | TmH(h) -> (*** 000 ***)
    let c = o 3 0 c in
    let c = seo_hashval o h c in
    c
  | DB(x) when x >= 0 && x <= 65535 -> (*** 001 ***)
    let c = o 3 1 c in
    seo_varintb o x c
  | DB(x) ->
    raise (Failure "seo_tm - de Bruijn out of bounds");
  | Prim(x) when x >= 0 && x <= 65535 -> (*** 010 ***)
    let c = o 3 2 c in
    let c = seo_varintb o x c in
    c
  | Prim(x) ->
    raise (Failure "seo_tm - Prim out of bounds");
  | Ap(m0,m1) -> (*** 011 ***)
    let c = o 3 3 c in
    let c = seo_tm o m0 c in
    let c = seo_tm o m1 c in
    c
  | Lam(m0,m1) -> (*** 100 ***)
    let c = o 3 4 c in
    let c = seo_tp o m0 c in
    let c = seo_tm o m1 c in
    c
  | Imp(m0,m1) -> (*** 101 ***)
    let c = o 3 5 c in
    let c = seo_tm o m0 c in
    let c = seo_tm o m1 c in
    c
  | All(m0,m1) -> (*** 110 ***)
    let c = o 3 6 c in
    let c = seo_tp o m0 c in
    let c = seo_tm o m1 c in
    c
  | TTpAp(m0,a) -> (*** 111 0 ***)
    let c = o 4 7 c in
    let c = seo_tm o m0 c in
    let c = seo_tp o a c in
    c
  | TTpLam(m0) -> (*** 111 1 0 ***)
    let c = o 5 15 c in
    let c = seo_tm o m0 c in
    c
  | TTpAll(m0) -> (*** 111 1 1 ***)
    let c = o 5 31 c in
    let c = seo_tm o m0 c in
    c

let tm_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_tm seosb m (c,None));
  Buffer.contents c

let hashtm m = hashtag (sha256str (tm_to_str m)) 66l

let rec sei_tm i c =
  let (x,c) = i 3 c in
  if x = 0 then
    let (h,c) = sei_hashval i c in
    (TmH(h),c)
  else if x = 1 then
    let (z,c) = sei_varintb i c in
    (DB(z),c)
  else if x = 2 then
    let (z,c) = sei_varintb i c in
    (Prim(z),c)
  else if x = 3 then
    let (m0,c) = sei_tm i c in
    let (m1,c) = sei_tm i c in
    (Ap(m0,m1),c)
  else if x = 4 then
    let (m0,c) = sei_tp i c in
    let (m1,c) = sei_tm i c in
    (Lam(m0,m1),c)
  else if x = 5 then
    let (m0,c) = sei_tm i c in
    let (m1,c) = sei_tm i c in
    (Imp(m0,m1),c)
  else if x = 6 then
    let (m0,c) = sei_tp i c in
    let (m1,c) = sei_tm i c in
    (All(m0,m1),c)
  else
    let (y,c) = i 1 c in
    if y = 0 then
      let (m0,c) = sei_tm i c in
      let (a,c) = sei_tp i c in
      (TTpAp(m0,a),c)
    else
      let (y,c) = i 1 c in
      if y = 0 then
	      let (m0,c) = sei_tm i c in
	      (TTpLam(m0),c)
      else
	      let (m0,c) = sei_tm i c in
	      (TTpAll(m0),c)

(** ** pf serialization ***)
let rec seo_pf o m c =
  match m with
  | Hyp(x) when x >= 0 && x <= 65535 -> (*** 001 ***)
    let c = o 3 1 c in
    seo_varintb o x c
  | Hyp(x) ->
    raise (Failure "seo_pf - Hypothesis out of bounds");
  | Known(h) -> (*** 010 ***)
    let c = o 3 2 c in
    let c = seo_hashval o h c in
    c
  | TmAp(m0,m1) -> (*** 011 ***)
    let c = o 3 3 c in
    let c = seo_pf o m0 c in
    let c = seo_tm o m1 c in
    c
  | PrAp(m0,m1) -> (*** 100 ***)
    let c = o 3 4 c in
    let c = seo_pf o m0 c in
    let c = seo_pf o m1 c in
    c
  | PrLa(m0,m1) -> (*** 101 ***)
    let c = o 3 5 c in
    let c = seo_tm o m0 c in
    let c = seo_pf o m1 c in
    c
  | TmLa(m0,m1) -> (*** 110 ***)
    let c = o 3 6 c in
    let c = seo_tp o m0 c in
    let c = seo_pf o m1 c in
    c
  | TpAp(d,a) -> (*** 111 0 ***)
    let c = o 4 7 c in
    let c = seo_pf o d c in
    let c = seo_tp o a c in
    c
  | TpLa(d) -> (*** 111 1 ***)
    let c = o 4 15 c in
    let c = seo_pf o d c in
    c

let pf_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_pf seosb m (c,None));
  Buffer.contents c

let hashpf m = hashtag (sha256str (pf_to_str m)) 67l

let rec sei_pf i c =
  let (x,c) = i 3 c in
  if x = 0 then
    failwith "GPA"
  else if x = 1 then
    let (z,c) = sei_varintb i c in
    (Hyp(z),c)
  else if x = 2 then
    let (z,c) = sei_hashval i c in
    (Known(z),c)
  else if x = 3 then
    let (m0,c) = sei_pf i c in
    let (m1,c) = sei_tm i c in
    (TmAp(m0,m1),c)
  else if x = 4 then
    let (m0,c) = sei_pf i c in
    let (m1,c) = sei_pf i c in
    (PrAp(m0,m1),c)
  else if x = 5 then
    let (m0,c) = sei_tm i c in
    let (m1,c) = sei_pf i c in
    (PrLa(m0,m1),c)
  else if x = 6 then
    let (m0,c) = sei_tp i c in
    let (m1,c) = sei_pf i c in
    (TmLa(m0,m1),c)
  else
    let (y,c) = i 1 c in
    if y = 0 then
      let (d,c) = sei_pf i c in
      let (a,c) = sei_tp i c in
      (TpAp(d,a),c)
    else
      let (d,c) = sei_pf i c in
      (TpLa(d),c)

(** ** theoryspec serialization ***)
let seo_theoryitem o m c =
  match m with
  | Thyprim(a) -> (* 0 0 *)
    let c = o 2 0 c in
    seo_tp o a c
  | Thydef(a,m) -> (* 0 1 *)
    let c = o 2 2 c in
    let c = seo_tp o a c in
    seo_tm o m c
  | Thyaxiom(m) -> (* 1 *)
    let c = o 1 1 c in
    seo_tm o m c

let sei_theoryitem i c =
  let (b,c) = i 1 c in
  if b = 0 then
    let (b,c) = i 1 c in
    if b = 0 then
      let (a,c) = sei_tp i c in
      (Thyprim(a),c)
    else
      let (a,c) = sei_tp i c in
      let (m,c) = sei_tm i c in
      (Thydef(a,m),c)
  else
    let (m,c) = sei_tm i c in
    (Thyaxiom(m),c)

let seo_theoryspec o dl c = seo_list seo_theoryitem o dl c
let sei_theoryspec i c = sei_list sei_theoryitem i c

let theoryspec_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_theoryspec seosb m (c,None));
  Buffer.contents c

(** ** signaspec serialization ***)
let seo_signaitem o m c =
  match m with
  | Signasigna(h) -> (** 00 **)
    let c = o 2 0 c in
    seo_hashval o h c
  | Signaparam(h,a) -> (** 01 **)
    let c = o 2 1 c in
    let c = seo_hashval o h c in
    seo_tp o a c
  | Signadef(a,m) -> (** 10 **)
    let c = o 2 2 c in
    let c = seo_tp o a c in
    seo_tm o m c
  | Signaknown(m) -> (** 11 **)
    let c = o 2 3 c in
    seo_tm o m c

let sei_signaitem i c =
  let (b,c) = i 2 c in
  if b = 0 then
    let (h,c) = sei_hashval i c in
    (Signasigna(h),c)
  else if b = 1 then
    let (h,c) = sei_hashval i c in
    let (a,c) = sei_tp i c in
    (Signaparam(h,a),c)
  else if b = 2 then
    let (a,c) = sei_tp i c in
    let (m,c) = sei_tm i c in
    (Signadef(a,m),c)
  else
    let (m,c) = sei_tm i c in
    (Signaknown(m),c)

let seo_signaspec o dl c = seo_list seo_signaitem o dl c
let sei_signaspec i c = sei_list sei_signaitem i c

let signaspec_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_signaspec seosb m (c,None));
  Buffer.contents c

(** ** doc serialization ***)
let seo_docitem o m c =
  match m with
  | Docsigna(h) -> (** 00 0 **)
    let c = o 3 0 c in
    seo_hashval o h c
  | Docparam(h,a) -> (** 00 1 **)
    let c = o 3 4 c in
    let c = seo_hashval o h c in
    seo_tp o a c
  | Docdef(a,m) -> (** 01 **)
    let c = o 2 1 c in
    let c = seo_tp o a c in
    seo_tm o m c
  | Docknown(m) -> (** 10 0 **)
    let c = o 3 2 c in
    seo_tm o m c
  | Docconj(m) -> (** 10 1 **)
    let c = o 3 6 c in
    seo_tm o m c
  | Docpfof(m,d) -> (** 11 **)
    let c = o 2 3 c in
    let c = seo_tm o m c in
    seo_pf o d c

let sei_docitem i c =
  let (b,c) = i 2 c in
  if b = 0 then
    let (b,c) = i 1 c in
    if b = 0 then
      let (h,c) = sei_hashval i c in
      (Docsigna(h),c)
    else
      let (h,c) = sei_hashval i c in
      let (a,c) = sei_tp i c in
      (Docparam(h,a),c)
  else if b = 1 then
    let (a,c) = sei_tp i c in
    let (m,c) = sei_tm i c in
    (Docdef(a,m),c)
  else if b = 2 then
    let (b,c) = i 1 c in
    if b = 0 then
      let (m,c) = sei_tm i c in
      (Docknown(m),c)
    else
      let (m,c) = sei_tm i c in
      (Docconj(m),c)
  else
    let (m,c) = sei_tm i c in
    let (d,c) = sei_pf i c in
    (Docpfof(m,d),c)

let seo_doc o dl c = seo_list seo_docitem o dl c
let sei_doc i c = sei_list sei_docitem i c

let doc_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_doc seosb m (c,None));
  Buffer.contents c

let hashdoc m = hashtag (sha256str (doc_to_str m)) 70l

(** ** serialization of theories ***)
let seo_theory o (al,kl) c =
  let c = seo_tpl o al c in
  seo_list seo_hashval o kl c

let sei_theory i c =
  let (al,c) = sei_tpl i c in
  let (kl,c) = sei_list sei_hashval i c in
  ((al,kl),c)

let theory_to_str thy =
  let c = Buffer.create 1000 in
  seosbf (seo_theory seosb thy (c,None));
  Buffer.contents c

(** * computation of hash roots ***)
let rec tm_hashroot m =
  match m with
  | TmH(h) -> h
  | Prim(x) -> hashtag (hashint32 (Int32.of_int x)) 96l
  | DB(x) -> hashtag (hashint32 (Int32.of_int x)) 97l
  | Ap(m,n) -> hashtag (hashpair (tm_hashroot m) (tm_hashroot n)) 98l
  | Lam(a,m) -> hashtag (hashpair (hashtp a) (tm_hashroot m)) 99l
  | Imp(m,n) -> hashtag (hashpair (tm_hashroot m) (tm_hashroot n)) 100l
  | All(a,m) -> hashtag (hashpair (hashtp a) (tm_hashroot m)) 101l
  | TTpAp(m,a) -> hashtag (hashpair (tm_hashroot m) (hashtp a)) 102l
  | TTpLam(m) -> hashtag (tm_hashroot m) 103l
  | TTpAll(m) -> hashtag (tm_hashroot m) 104l

let rec pf_hashroot d =
  match d with
  | Known(h) -> hashtag h 128l
  | Hyp(x) -> hashtag (hashint32 (Int32.of_int x)) 129l
  | TmAp(d,m) -> hashtag (hashpair (pf_hashroot d) (tm_hashroot m)) 130l
  | PrAp(d,e) -> hashtag (hashpair (pf_hashroot d) (pf_hashroot e)) 131l
  | PrLa(m,d) -> hashtag (hashpair (tm_hashroot m) (pf_hashroot d)) 132l
  | TmLa(a,d) -> hashtag (hashpair (hashtp a) (pf_hashroot d)) 133l
  | TpAp(d,a) -> hashtag (hashpair (pf_hashroot d) (hashtp a)) 134l
  | TpLa(d) -> hashtag (pf_hashroot d) 135l

let rec docitem_hashroot d =
  match d with
  | Docsigna(h) -> hashtag h 172l
  | Docparam(h,a) -> hashtag (hashpair h (hashtp a)) 173l
  | Docdef(a,m) -> hashtag (hashpair (hashtp a) (tm_hashroot m)) 174l
  | Docknown(m) -> hashtag (tm_hashroot m) 175l
  | Docconj(m) -> hashtag (tm_hashroot m) 176l
  | Docpfof(m,d) -> hashtag (hashpair (tm_hashroot m) (pf_hashroot d)) 177l

let rec doc_hashroot dl =
  match dl with
  | [] -> hashint32 180l
  | d::dr -> hashtag (hashpair (docitem_hashroot d) (doc_hashroot dr)) 181l

let hashtheory (al,kl) =
  hashopair
    (ohashlist (List.map hashtp al))
    (ohashlist kl)

let hashgsigna (tl,kl) =
  hashpair
    (hashlist
       (List.map (fun z ->
	          match z with
	          | ((h,a),None) -> hashtag (hashpair h (hashtp a)) 160l
	          | ((h,a),Some(m)) -> hashtag (hashpair h (hashpair (hashtp a) (hashtm m))) 161l)
	         tl))
    (hashlist (List.map (fun (k,p) -> (hashpair k (hashtm p))) kl))

let hashsigna (sl,(tl,kl)) = hashpair (hashlist sl) (hashgsigna (tl,kl))

let seo_gsigna o s c =
  seo_prod
    (seo_list (seo_prod_prod seo_hashval seo_tp (seo_option seo_tm)))
    (seo_list (seo_prod seo_hashval seo_tm))
    o s c

let sei_gsigna i c =
  sei_prod
    (sei_list (sei_prod_prod sei_hashval sei_tp (sei_option sei_tm)))
    (sei_list (sei_prod sei_hashval sei_tm))
    i c

let seo_signa o s c =
  seo_prod (seo_list seo_hashval) seo_gsigna o s c

let sei_signa i c =
  sei_prod (sei_list sei_hashval) sei_gsigna i c

let signa_to_str s =
  let c = Buffer.create 1000 in
  seosbf (seo_signa seosb s (c,None));
  Buffer.contents c

(** * htrees to hold theories and signatures **)

type ttree = theory htree
type stree = signa htree

let ottree_insert t bl thy =
  match t with
  | Some(t) -> htree_insert t bl thy
  | None -> htree_create bl thy

let ostree_insert t bl s =
  match t with
  | Some(t) -> htree_insert t bl s
  | None -> htree_create bl s

let ottree_hashroot t = ohtree_hashroot hashtheory 256 t

let ostree_hashroot t = ohtree_hashroot (fun s -> Some(hashsigna s)) 256 t

let ottree_lookup (t:ttree option) h =
  match t, h with
  | Some(t), Some(h) ->
    begin
	    match htree_lookup (hashval_bitseq h) t with
	    | None -> raise Not_found
	    | Some(thy) -> thy
    end
  | _,None -> ([],[])
  | _,_ -> raise Not_found

(** * operations including type checking and proof checking ***)

let rec import_signatures th (str:stree) hl sg imported =
  match hl with
  | [] -> Some (sg,imported)
  | (h::hr) ->
    if List.mem h imported then
	    (import_signatures th str hr sg imported)
    else
	    match htree_lookup (hashval_bitseq (hashopair2 th h)) str with 
	    | Some(sl,(tptml2,kl2)) ->
       begin
	       match  import_signatures th str sl sg imported with
         | Some ((tptml3,kl3),imported) -> Some ((tptml3 @ tptml2,kl3 @ kl2), imported)
         | None -> None
       end
	    | None -> None

let rec print_tp v t base_types =
  match t with
  | TpVar n -> Printf.printf "tpvar %d %d " n v
  | Base i -> Printf.printf "base %d %d " i base_types
  | TpAll t1 -> (Printf.printf "tpall "; print_tp (v + 1) t1 base_types)
  | TpArr (t1, t2) -> (Printf.printf "tparr "; print_tp v t1 base_types; print_tp v t2 base_types)
  | Prop -> Printf.printf "prop "

let rec print_trm v ctx sgn t thy =
  match t with
  | DB i -> Printf.printf "(DB %d %d )" (List.length ctx) i
  | TmH h ->
    printlist (fst sgn);
      Printf.printf "\n";
      Printf.printf "(TmH %s)" (hashval_hexstring h)
  | Prim i -> Printf.printf "(DB %d %d )" (List.length thy) i
  | Ap (t1, t2) -> (Printf.printf "ap "; print_trm v ctx sgn t1 thy; print_trm v ctx sgn t2 thy)
  | Lam (a1, t1) -> (Printf.printf "lam "; print_tp v a1 (List.length thy); print_trm v ctx sgn t1 thy)
  | Imp (t1, t2) -> (Printf.printf "imp "; print_trm v ctx sgn t1 thy; print_trm v ctx sgn t2 thy)
  | All (b, t1) -> (Printf.printf "all "; print_tp v b (List.length thy); print_trm v ctx sgn t1 thy)
  | TTpAp (t1, b) -> (Printf.printf "tp ap "; print_tp v b (List.length thy); print_trm v ctx sgn t1 thy)
  | TTpLam t1 -> (Printf.printf "tp lam "; print_trm v ctx sgn t1 thy)
  | TTpAll t1 -> (Printf.printf "tp all "; print_trm v ctx sgn t1 thy)

let rec print_pf v ctx phi sg p thy =
  match p with
  | Known h -> Printf.printf "known %s " (hashval_hexstring h)
  | Hyp i -> Printf.printf "hypoth %d\n" i
  | PrAp (p1, p2) ->
    Printf.printf "proof ap (";
    print_pf v ctx phi sg p1 thy;
    print_pf v ctx phi sg p2 thy;
    Printf.printf ")"
  | TmAp (p1, t1) ->
    Printf.printf "trm ap (";
    print_pf v ctx phi sg p1 thy;
    print_trm v ctx sg t1 thy;
    Printf.printf ")"

  | TpAp (p1, a1) ->
    Printf.printf "stp ap (";
    print_pf v ctx phi sg p1 thy;
    print_tp v a1 (List.length thy);
    Printf.printf ")"

  | PrLa (s, p1) ->
    Printf.printf "proof lam (";
    print_pf v ctx (s :: phi) sg p1 thy;
    print_trm v ctx sg s thy;
    Printf.printf ")"

  | TmLa (a1, p1) ->
    Printf.printf "trm lam (";
    print_pf v (a1::ctx) phi sg p1 thy; (* (List.map (fun x -> uptrm x 0 1) phi) *)
    print_tp v a1 (List.length thy);
    Printf.printf ")"

  | TpLa p1 ->
    Printf.printf "stp lam (";
    print_pf (v + 1) ctx phi sg p1 thy;
    Printf.printf ")"

type pdoc =
  | PDocNil
  | PDocHash of hashval
  | PDocSigna of hashval * pdoc
  | PDocParam of hashval * stp * pdoc
  | PDocParamHash of hashval * pdoc
  | PDocDef of stp * trm * pdoc
  | PDocDefHash of hashval * pdoc
  | PDocKnown of trm * pdoc
  | PDocConj of trm * pdoc
  | PDocPfOf of trm * pf * pdoc
  | PDocPfOfHash of hashval * pdoc

exception BetaLimit
exception TermLimit
exception NonNormalTerm
exception CheckingFailure
exception NotKnown of hashval option * hashval
exception UnknownTerm of hashval option * hashval * stp
exception UnknownSigna of hashval

(** * conversion of theoryspec to theory and signaspec to signa **)
let rec theoryspec_primtps_r dl =
  match dl with
  | [] -> []
  | Thyprim(a)::dr -> a::theoryspec_primtps_r dr
  | _::dr -> theoryspec_primtps_r dr
  
let theoryspec_primtps dl = List.rev (theoryspec_primtps_r dl)

let rec theoryspec_hashedaxioms dl =
  match dl with
  | [] -> []
  | Thyaxiom(m)::dr -> tm_hashroot m::theoryspec_hashedaxioms dr
  | _::dr -> theoryspec_hashedaxioms dr

let theoryspec_theory thy = (theoryspec_primtps thy,theoryspec_hashedaxioms thy)

let hashtheory (al,kl) =
  hashopair
    (ohashlist (List.map hashtp al))
    (ohashlist kl)

let rec signaspec_signas s =
  match s with
  | [] -> []
  | Signasigna(h)::r -> h::signaspec_signas r
  | _::r -> signaspec_signas r

let rec signaspec_trms s =
  match s with
  | [] -> []
  | Signaparam(h,a)::r -> ((h, a), None)::signaspec_trms r
  | Signadef(a,m)::r -> ((tm_hashroot m, a), Some(m))::signaspec_trms r
  | _::r -> signaspec_trms r

let rec signaspec_knowns s =
  match s with
  | [] -> []
  | Signaknown(p)::r -> (tm_hashroot p,p)::signaspec_knowns r
  | _::r -> signaspec_knowns r

let signaspec_signa s = (signaspec_signas s, (signaspec_trms s, signaspec_knowns s))



let theory_burncost thy =
  Int64.mul 2100000000L (Int64.of_int (String.length (theory_to_str thy)))
  
let theoryspec_burncost s = theory_burncost (theoryspec_theory s)

let signa_burncost s =
  Int64.mul 2100000000L (Int64.of_int (String.length (signa_to_str s)))

let signaspec_burncost s = signa_burncost (signaspec_signa s)

(** * extract which objs/props are used/created by signatures and documents, as well as full_needed needed to globally verify terms have a certain type/knowns are known **)

let adj x l = if List.mem x l then l else x::l

let rec signaspec_uses_objs_aux (dl:signaspec) r : (hashval * hashval) list =
  match dl with
  | Signaparam(h,a)::dr -> signaspec_uses_objs_aux dr (adj (h,hashtp a) r)
  | _::dr -> signaspec_uses_objs_aux dr r
  | [] -> r

let signaspec_uses_objs (dl:signaspec) : (hashval * hashval) list = signaspec_uses_objs_aux dl []

let rec signaspec_uses_props_aux (dl:signaspec) r : hashval list =
  match dl with
  | Signaknown(p)::dr -> signaspec_uses_props_aux dr (adj (tm_hashroot p) r)
  | _::dr -> signaspec_uses_props_aux dr r
  | [] -> r

let signaspec_uses_props (dl:signaspec) : hashval list = signaspec_uses_props_aux dl []

let rec doc_uses_objs_aux (dl:doc) r : (hashval * hashval) list =
  match dl with
  | Docparam(h,a)::dr -> doc_uses_objs_aux dr (adj (h,hashtp a) r)
  | _::dr -> doc_uses_objs_aux dr r
  | [] -> r

let doc_uses_objs (dl:doc) : (hashval * hashval) list = doc_uses_objs_aux dl []

let rec doc_uses_props_aux (dl:doc) r : hashval list =
  match dl with
  | Docknown(p)::dr -> doc_uses_props_aux dr (adj (tm_hashroot p) r)
  | _::dr -> doc_uses_props_aux dr r
  | [] -> r

let doc_uses_props (dl:doc) : hashval list = doc_uses_props_aux dl []

let rec doc_creates_objs_aux (dl:doc) r : (hashval * hashval) list =
  match dl with
  | Docdef(a,m)::dr -> doc_creates_objs_aux dr (adj (tm_hashroot m,hashtp a) r)
  | _::dr -> doc_creates_objs_aux dr r
  | [] -> r

let doc_creates_objs (dl:doc) : (hashval * hashval) list = doc_creates_objs_aux dl []

let rec doc_creates_props_aux (dl:doc) r : hashval list =
  match dl with
  | Docpfof(p,d)::dr -> doc_creates_props_aux dr (adj (tm_hashroot p) r)
  | _::dr -> doc_creates_props_aux dr r
  | [] -> r

let doc_creates_props (dl:doc) : hashval list = doc_creates_props_aux dl []

let falsehashprop = tm_hashroot (All(Prop,DB(0)))
let neghashprop = tm_hashroot (Lam(Prop,Imp(DB(0),TmH(falsehashprop))))

let invert_neg_prop p =
  match p with
  | Imp(np,f) when tm_hashroot f = falsehashprop -> np
  | Ap(n,np) when tm_hashroot n = neghashprop -> np
  | _ -> raise Not_found

let rec doc_creates_neg_props_aux (dl:doc) r : hashval list =
  match dl with
  | Docpfof(p,d)::dr ->
      begin
	try
	  let np = invert_neg_prop p in
	  doc_creates_neg_props_aux dr (adj (tm_hashroot np) r)
	with Not_found -> doc_creates_neg_props_aux dr r
      end
  | _::dr -> doc_creates_neg_props_aux dr r
  | [] -> r

let doc_creates_neg_props (dl:doc) : hashval list = doc_creates_neg_props_aux dl []

let rec pdoc_hashroot dl =
  match dl with
  | PDocNil -> hashint32 180l
  | PDocHash(h) -> h
  | PDocSigna(h,dr) ->
      hashtag (hashpair (hashtag h 172l)
		 (pdoc_hashroot dr)) 181l
  | PDocParam(h,a,dr) ->
      hashtag (hashpair (hashtag (hashpair h (hashtp a)) 173l)
		 (pdoc_hashroot dr)) 181l
  | PDocParamHash(h,dr) ->
      hashtag (hashpair (hashtag h 173l)
		 (pdoc_hashroot dr)) 181l
  | PDocDef(a,m,dr) ->
      hashtag (hashpair (hashtag (hashpair (hashtp a) (tm_hashroot m)) 174l)
		 (pdoc_hashroot dr)) 181l
  | PDocDefHash(h,dr) ->
      hashtag (hashpair (hashtag h 174l)
		 (pdoc_hashroot dr)) 181l
  | PDocKnown(m,dr) ->
      hashtag (hashpair (hashtag (tm_hashroot m) 175l)
		 (pdoc_hashroot dr)) 181l
  | PDocConj(m,dr) ->
      hashtag (hashpair (hashtag (tm_hashroot m) 176l)
		 (pdoc_hashroot dr)) 181l
  | PDocPfOf(m,d,dr) ->
      hashtag (hashpair (hashtag (hashpair (tm_hashroot m) (pf_hashroot d)) 177l)
		 (pdoc_hashroot dr)) 181l
  | PDocPfOfHash(h,dr) ->
      hashtag (hashpair (hashtag h 177l)
		 (pdoc_hashroot dr)) 181l

(** ** pdoc serialization ***)
let rec seo_pdoc o dl c =
  match dl with
  | PDocNil -> (** 00 0 **)
      o 3 0 c
  | PDocHash(h) -> (** 00 1 **)
      let c = o 3 4 c in
      let c = seo_hashval o h c in
      c
  | PDocSigna(h,dr) -> (** 01 0 **)
      let c = o 3 1 c in
      let c = seo_hashval o h c in
      seo_pdoc o dr c
  | PDocParam(h,a,dr) -> (** 01 1 **)
      let c = o 3 5 c in
      let c = seo_hashval o h c in
      let c = seo_tp o a c in
      seo_pdoc o dr c
  | PDocParamHash(h,dr) -> (** 10 0 **)
      let c = o 3 2 c in
      let c = seo_hashval o h c in
      seo_pdoc o dr c
  | PDocDef(a,m,dr) -> (** 10 1 0 **)
      let c = o 4 6 c in
      let c = seo_tp o a c in
      let c = seo_tm o m c in
      seo_pdoc o dr c
  | PDocDefHash(h,dr) -> (** 10 1 1 **)
      let c = o 4 14 c in
      let c = seo_hashval o h c in
      seo_pdoc o dr c
  | PDocKnown(m,dr) -> (** 11 00 **)
      let c = o 4 3 c in
      let c = seo_tm o m c in
      seo_pdoc o dr c
  | PDocConj(m,dr) -> (** 11 01 **)
      let c = o 4 7 c in
      let c = seo_tm o m c in
      seo_pdoc o dr c
  | PDocPfOf(m,d,dr) -> (** 11 10 **)
      let c = o 4 11 c in
      let c = seo_tm o m c in
      let c = seo_pf o d c in
      seo_pdoc o dr c
  | PDocPfOfHash(h,dr) -> (** 11 11 **)
      let c = o 4 15 c in
      let c = seo_hashval o h c in
      seo_pdoc o dr c

let pdoc_to_str m =
  let c = Buffer.create 1000 in
  seosbf (seo_pdoc seosb m (c,None));
  Buffer.contents c

let hashpdoc m = hashtag (sha256str (pdoc_to_str m)) 71l

let rec sei_pdoc i c =
  let (b,c) = i 2 c in
  if b = 0 then
    let (b,c) = i 1 c in
    if b = 0 then
      (PDocNil,c)
    else
      let (h,c) = sei_hashval i c in
      (PDocHash(h),c)
  else if b = 1 then
    let (b,c) = i 1 c in
    if b = 0 then
      let (h,c) = sei_hashval i c in
      let (dr,c) = sei_pdoc i c in
      (PDocSigna(h,dr),c)
    else
      let (h,c) = sei_hashval i c in
      let (a,c) = sei_tp i c in
      let (dr,c) = sei_pdoc i c in
      (PDocParam(h,a,dr),c)
  else if b = 2 then
    let (b,c) = i 1 c in
    if b = 0 then
      let (h,c) = sei_hashval i c in
      let (dr,c) = sei_pdoc i c in
      (PDocParamHash(h,dr),c)
    else
      let (b,c) = i 1 c in
      if b = 0 then
	let (a,c) = sei_tp i c in
	let (m,c) = sei_tm i c in
	let (dr,c) = sei_pdoc i c in
	(PDocDef(a,m,dr),c)
      else
      let (h,c) = sei_hashval i c in
      let (dr,c) = sei_pdoc i c in
      (PDocDefHash(h,dr),c)
  else
    let (b,c) = i 2 c in
    if b = 0 then
      let (m,c) = sei_tm i c in
      let (dr,c) = sei_pdoc i c in
      (PDocKnown(m,dr),c)
    else if b = 1 then
      let (m,c) = sei_tm i c in
      let (dr,c) = sei_pdoc i c in
      (PDocConj(m,dr),c)
    else if b = 2 then
      let (m,c) = sei_tm i c in
      let (d,c) = sei_pf i c in
      let (dr,c) = sei_pdoc i c in
      (PDocPfOf(m,d,dr),c)
    else
      let (h,c) = sei_hashval i c in
      let (dr,c) = sei_pdoc i c in
      (PDocPfOfHash(h,dr),c)

let rec json_stp a =
  match a with
  | TpVar(i) -> JsonObj([("type",JsonStr("stp"));("stpcase",JsonStr("tpvar"));("tpvar",JsonNum(string_of_int i))])
  | Base(i) -> JsonObj([("type",JsonStr("stp"));("stpcase",JsonStr("base"));("base",JsonNum(string_of_int i))])
  | TpAll(a) -> JsonObj([("type",JsonStr("stp"));("stpcase",JsonStr("tpall"));("body",json_stp a)])
  | TpArr(a1,a2) -> JsonObj([("type",JsonStr("stp"));("stpcase",JsonStr("tparr"));("dom",json_stp a1);("cod",json_stp a2)])
  | Prop -> JsonObj([("type",JsonStr("stp"));("stpcase",JsonStr("prop"))])

let rec json_trm m =
  match m with
  | DB(i) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("db"));("db",JsonNum(string_of_int i))])
  | TmH(h) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("tmh"));("trmroot",JsonStr(hashval_hexstring h))])
  | Prim(i) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("prim"));("prim",JsonNum(string_of_int i))])
  | Ap(m,n) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("ap"));("func",json_trm m);("arg",json_trm n)])
  | Lam(a,m) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("lam"));("dom",json_stp a);("body",json_trm m)])
  | Imp(m,n) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("imp"));("ant",json_trm m);("suc",json_trm n)])
  | All(a,m) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("all"));("dom",json_stp a);("body",json_trm m)])
  | TTpAp(m,a) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("ttpap"));("func",json_trm m);("arg",json_stp a)])
  | TTpLam(m) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("ttplam"));("body",json_trm m)])
  | TTpAll(m) -> JsonObj([("type",JsonStr("trm"));("trmcase",JsonStr("ttpall"));("body",json_trm m)])

let rec json_pf d =
  match d with
  | Known(h) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("known"));("trmroot",JsonStr(hashval_hexstring h))])
  | Hyp(i) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("hyp"));("hyp",JsonNum(string_of_int i))])
  | PrAp(d,e) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("prap"));("func",json_pf d);("arg",json_pf e)])
  | TmAp(d,m) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("tmap"));("func",json_pf d);("arg",json_trm m)])
  | TpAp(d,a) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("tpap"));("func",json_pf d);("arg",json_stp a)])
  | PrLa(m,d) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("prla"));("dom",json_trm m);("body",json_pf d)])
  | TmLa(a,d) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("tmla"));("dom",json_stp a);("body",json_pf d)])
  | TpLa(d) -> JsonObj([("type",JsonStr("pf"));("pfcase",JsonStr("tpla"));("body",json_pf d)])

let json_theoryitem x =
  match x with
  | Thyprim(a) -> JsonObj([("type",JsonStr("theoryitem"));("theoryitemcase",JsonStr("thyprim"));("stp",json_stp a)])
  | Thyaxiom(p) -> JsonObj([("type",JsonStr("theoryitem"));("theoryitemcase",JsonStr("thyaxiom"));("prop",json_trm p)])
  | Thydef(a,m) -> JsonObj([("type",JsonStr("theoryitem"));("theoryitemcase",JsonStr("thydef"));("stp",json_stp a);("def",json_trm m)])

let json_signaitem th x =
  match x with
  | Signasigna(h) -> JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signasigna"));("signaroot",JsonStr(hashval_hexstring h))])
  | Signaparam(h,a) ->
      let objid = hashtag (hashopair2 th (hashpair h (hashtp a))) 32l in
      JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signaparam"));("trmroot",JsonStr(hashval_hexstring h));("objid",JsonStr(hashval_hexstring objid));("stp",json_stp a)])
  | Signadef(a,m) ->
      let trmroot = tm_hashroot m in
      let objid = hashtag (hashopair2 th (hashpair trmroot (hashtp a))) 32l in
      JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signadef"));("stp",json_stp a);("trmroot",JsonStr(hashval_hexstring trmroot));("objid",JsonStr(hashval_hexstring objid));("def",json_trm m)])
  | Signaknown(p) ->
      let trmroot = tm_hashroot p in
      let propid = hashtag (hashopair2 th trmroot) 33l in
      JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signaknown"));("trmroot",JsonStr(hashval_hexstring trmroot));("propid",JsonStr(hashval_hexstring propid));("prop",json_trm p)])

let json_docitem th x =
  match x with
  | Docsigna(h) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docsigna"));("signaroot",JsonStr(hashval_hexstring h))])
  | Docparam(h,a) ->
      let objid = hashtag (hashopair2 th (hashpair h (hashtp a))) 32l in
      JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docparam"));("trmroot",JsonStr(hashval_hexstring h));("objid",JsonStr(hashval_hexstring objid));("stp",json_stp a)])
  | Docdef(a,m) ->
      let trmroot = tm_hashroot m in
      let objid = hashtag (hashopair2 th (hashpair trmroot (hashtp a))) 32l in
      JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docdef"));("stp",json_stp a);("trmroot",JsonStr(hashval_hexstring trmroot));("objid",JsonStr(hashval_hexstring objid));("def",json_trm m)])
  | Docknown(p) ->
      let trmroot = tm_hashroot p in
      let propid = hashtag (hashopair2 th trmroot) 33l in
      JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docknown"));("trmroot",JsonStr(hashval_hexstring trmroot));("propid",JsonStr(hashval_hexstring propid));("prop",json_trm p)])
  | Docpfof(p,d) ->
      let trmroot = tm_hashroot p in
      let propid = hashtag (hashopair2 th trmroot) 33l in
      JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docpfof"));("trmroot",JsonStr(hashval_hexstring trmroot));("propid",JsonStr(hashval_hexstring propid));("prop",json_trm p);("pf",json_pf d)])
  | Docconj(p) ->
      let trmroot = tm_hashroot p in
      let propid = hashtag (hashopair2 th trmroot) 33l in
      JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docconj"));("trmroot",JsonStr(hashval_hexstring trmroot));("propid",JsonStr(hashval_hexstring propid));("prop",json_trm p)])

let json_theoryspec ts = JsonArr(List.map json_theoryitem ts)

let json_signaspec th ss = JsonArr(List.map (json_signaitem th) ss)

let json_doc th d = JsonArr (List.map (json_docitem th) d)

let rec stp_from_json j =
  match j with
  | JsonObj(al) ->
      let c = List.assoc "stpcase" al in
      if c = JsonStr("tpvar") then
	TpVar(int_from_json(List.assoc "tpvar" al))
      else if c = JsonStr("base") then
	Base(int_from_json(List.assoc "base" al))
      else if c = JsonStr("tpall") then
	TpAll(stp_from_json(List.assoc "body" al))
      else if c = JsonStr("tparr") then
	TpArr(stp_from_json(List.assoc "dom" al),stp_from_json(List.assoc "cod" al))
      else if c = JsonStr("prop") then
	Prop
      else
	raise (Failure("not an stp"))
  | _ -> raise (Failure("not an stp"))

let rec trm_from_json j =
  match j with
  | JsonObj(al) ->
      let c = List.assoc "trmcase" al in
      if c = JsonStr("db") then
	DB(int_from_json(List.assoc "db" al))
      else if c = JsonStr("tmh") then
	TmH(hashval_from_json(List.assoc "trmroot" al))
      else if c = JsonStr("prim") then
	Prim(int_from_json(List.assoc "prim" al))
      else if c = JsonStr("ap") then
	Ap(trm_from_json(List.assoc "func" al),trm_from_json(List.assoc "arg" al))
      else if c = JsonStr("lam") then
	Lam(stp_from_json(List.assoc "dom" al),trm_from_json(List.assoc "body" al))
      else if c = JsonStr("imp") then
	Imp(trm_from_json(List.assoc "ant" al),trm_from_json(List.assoc "suc" al))
      else if c = JsonStr("all") then
	All(stp_from_json(List.assoc "dom" al),trm_from_json(List.assoc "body" al))
      else if c = JsonStr("ttpap") then
	TTpAp(trm_from_json(List.assoc "func" al),stp_from_json(List.assoc "arg" al))
      else if c = JsonStr("ttplam") then
	TTpLam(trm_from_json(List.assoc "fbody" al))
      else if c = JsonStr("ttpall") then
	TTpAll(trm_from_json(List.assoc "fbody" al))
      else
	raise (Failure("not a trm"))
  | _ -> raise (Failure("not a trm"))

let rec pf_from_json j =
  match j with
  | JsonObj(al) ->
      let c = List.assoc "pfcase" al in
      if c = JsonStr("known") then
	Known(hashval_from_json(List.assoc "trmroot" al))
      else if c = JsonStr("hyp") then
	Hyp(int_from_json(List.assoc "hyp" al))
      else if c = JsonStr("prap") then
	PrAp(pf_from_json(List.assoc "func" al),pf_from_json(List.assoc "arg" al))
      else if c = JsonStr("tmap") then
	TmAp(pf_from_json(List.assoc "func" al),trm_from_json(List.assoc "arg" al))
      else if c = JsonStr("tpap") then
	TpAp(pf_from_json(List.assoc "func" al),stp_from_json(List.assoc "arg" al))
      else if c = JsonStr("prla") then
	PrLa(trm_from_json(List.assoc "dom" al),pf_from_json(List.assoc "body" al))
      else if c = JsonStr("tmla") then
	TmLa(stp_from_json(List.assoc "dom" al),pf_from_json(List.assoc "body" al))
      else if c = JsonStr("tpla") then
	TpLa(pf_from_json(List.assoc "body" al))
      else
	raise (Failure("not a pf"))
  | _ -> raise (Failure("not a pf"))

let theoryitem_from_json j =
  match j with
  | JsonObj(al) ->
      let c = List.assoc "theoryitemcase" al in
      if c = JsonStr("thyprim") then
	Thyprim(stp_from_json(List.assoc "stp" al))
      else if c = JsonStr("thyaxiom") then
	Thyaxiom(trm_from_json(List.assoc "prop" al))
      else if c = JsonStr("thydef") then
	Thydef(stp_from_json(List.assoc "stp" al),trm_from_json(List.assoc "def" al))
      else
	raise (Failure("not a theoryitem"))
  | _ -> raise (Failure("not a theoryitem"))

let signaitem_from_json j =
  match j with
  | JsonObj(al) ->
      let c = List.assoc "signaitemcase" al in
      if c = JsonStr("signasigna") then
	Signasigna(hashval_from_json(List.assoc "signaroot" al))
      else if c = JsonStr("signaparam") then
	Signaparam(hashval_from_json(List.assoc "trmroot" al),stp_from_json(List.assoc "stp" al))
      else if c = JsonStr("signadef") then
	Signadef(stp_from_json(List.assoc "stp" al),trm_from_json(List.assoc "def" al))
      else if c = JsonStr("signaknown") then
	Signaknown(trm_from_json(List.assoc "prop" al))
      else
	raise (Failure("not a signaitem"))
  | _ -> raise (Failure("not a signaitem"))

let docitem_from_json j =
  match j with
  | JsonObj(al) ->
      let c = List.assoc "docitemcase" al in
      if c = JsonStr("docsigna") then
	Docsigna(hashval_from_json(List.assoc "signaroot" al))
      else if c = JsonStr("docparam") then
	Docparam(hashval_from_json(List.assoc "trmroot" al),stp_from_json(List.assoc "stp" al))
      else if c = JsonStr("docdef") then
	Docdef(stp_from_json(List.assoc "stp" al),trm_from_json(List.assoc "def" al))
      else if c = JsonStr("docknown") then
	Docknown(trm_from_json(List.assoc "prop" al))
      else if c = JsonStr("docpfof") then
	Docpfof(trm_from_json(List.assoc "prop" al),pf_from_json(List.assoc "pf" al))
      else if c = JsonStr("docconj") then
	Docconj(trm_from_json(List.assoc "prop" al))
      else
	raise (Failure("not a docitem"))
  | _ -> raise (Failure("not a docitem"))

let theoryspec_from_json j =
  match j with
  | JsonArr(jl) -> List.map theoryitem_from_json jl
  | _ -> raise (Failure("not a theoryspec"))

let signaspec_from_json j =
  match j with
  | JsonArr(jl) -> List.map signaitem_from_json jl
  | _ -> raise (Failure("not a signaspec"))

let doc_from_json j =
  match j with
  | JsonArr(jl) -> List.map docitem_from_json jl
  | _ -> raise (Failure("not a doc"))
  
