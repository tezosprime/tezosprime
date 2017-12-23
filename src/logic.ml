(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Hash

type stp =
| TpVar of int
| Base of int
| TpAll of stp
| TpArr of stp * stp
| Prop

type trm =
| DB of int
| TmH of hashval
| Prim of int
| Ap of trm * trm
| Lam of stp * trm
| Imp of trm * trm
| All of stp * trm
| TTpAp of trm * stp
| TTpLam of trm
| TTpAll of trm

type pf =
| Known of hashval
| Hyp of int
| PrAp of pf * pf
| TmAp of pf * trm
| TpAp of pf * stp
| PrLa of trm * pf
| TmLa of stp * pf
| TpLa of pf

type gsign = ((hashval * stp) * trm option) list * (hashval * trm) list

type theoryitem =
| Thyprim of stp
| Thyaxiom of trm
| Thydef of stp * trm

type theoryspec = theoryitem list

type theory = stp list * hashval list

type signaitem =
| Signasigna of hashval
| Signaparam of hashval * stp
| Signadef of stp * trm
| Signaknown of trm

type signaspec = signaitem list

type signa = hashval list * gsign

type docitem =
| Docsigna of hashval
| Docparam of hashval * stp
| Docdef of stp * trm
| Docknown of trm
| Docpfof of trm * pf
| Docconj of trm

type doc = docitem list

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

let json_signaitem x =
  match x with
  | Signasigna(h) -> JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signasigna"));("signaroot",JsonStr(hashval_hexstring h))])
  | Signaparam(h,a) -> JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signaparam"));("trmroot",JsonStr(hashval_hexstring h));("stp",json_stp a)])
  | Signadef(a,m) -> JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signadef"));("stp",json_stp a);("def",json_trm m)])
  | Signaknown(p) -> JsonObj([("type",JsonStr("signaitem"));("signaitemcase",JsonStr("signaknown"));("prop",json_trm p)])

let json_docitem x =
  match x with
  | Docsigna(h) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docsigna"));("signaroot",JsonStr(hashval_hexstring h))])
  | Docparam(h,a) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docparam"));("trmroot",JsonStr(hashval_hexstring h));("stp",json_stp a)])
  | Docdef(a,m) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docdef"));("stp",json_stp a);("def",json_trm m)])
  | Docknown(p) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docknown"));("prop",json_trm p)])
  | Docpfof(p,d) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docpfof"));("prop",json_trm p);("pf",json_pf d)])
  | Docconj(p) -> JsonObj([("type",JsonStr("docitem"));("docitemcase",JsonStr("docconj"));("prop",json_trm p)])

let json_theoryspec ts = JsonArr(List.map json_theoryitem ts)

let json_signaspec ss = JsonArr(List.map json_signaitem ss)

let json_doc d = JsonArr (List.map json_docitem d)

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
  
