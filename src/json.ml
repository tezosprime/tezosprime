(* Copyright (c) 2017 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

type jsonval =
  | JsonStr of string
  | JsonNum of string (*** do not support e ***)
  | JsonObj of (string * jsonval) list
  | JsonArr of jsonval list
  | JsonBool of bool
  | JsonNull

let rec print_jsonval c j =
  match j with
  | JsonStr(x) ->
      Printf.fprintf c "\"%s\"" (String.escaped x)
  | JsonNum(x) ->
      Printf.fprintf c "%s" x
  | JsonObj([]) -> Printf.fprintf c "{}"
  | JsonObj((x,v)::l) ->
      Printf.fprintf c "{";
      Printf.fprintf c "\"%s\":" x;
      print_jsonval c v;
      List.iter
	(fun (x,v) ->
	  Printf.fprintf c ",\"%s\":" x;
	  print_jsonval c v)
	l;
      Printf.fprintf c "}"
  | JsonArr([]) -> Printf.fprintf c "[]"
  | JsonArr(v::l) ->
      Printf.fprintf c "[";
      print_jsonval c v;
      List.iter
	(fun v ->
	  Printf.fprintf c ",";
	  print_jsonval c v)
	l;
      Printf.fprintf c "]"
  | JsonBool(b) -> Printf.fprintf c "%b" b
  | JsonNull -> Printf.fprintf c "null"

let whitespace_p c = c = ' ' || c = '\n' || c = '\r' || c = '\t'

let digit_p c = let d = Char.code c in d >= 48 && d <= 57

let parse_jsonval_start(x,i) =
  let rec parse_jsonval_a x i l : jsonval * int =
    if i < l then
      let c = x.[i] in
      if c = '\'' then
	parse_jsonval_b x (i+1) l
      else if c = '"' then
	let (y,j) = parse_jsonval_s x (i+1) l in
	(JsonStr(y),j)
      else if c = '{' then
	let (r,j) = parse_jsonval_obj x (i+1) l in
	(JsonObj(r),j)
      else if c = '[' then
	let (r,j) = parse_jsonval_arr x (i+1) l in
	(JsonArr(r),j)
      else if c = 'n' then
	parse_jsonval_null x (i+1) l
      else if c = 't' then
	parse_jsonval_true x (i+1) l
      else if c = 'f' then
	parse_jsonval_false x (i+1) l
      else if digit_p c then
	let (y,j) = parse_jsonval_num x i l in
	(JsonNum(y),j)
      else if whitespace_p c then
	parse_jsonval_a x (i+1) l
      else
	raise Exit
    else
      raise Exit
  and parse_jsonval_b x i l : jsonval * int =
    let (v,j) = parse_jsonval_a x i l in
    if j < l then
      let c = x.[j] in
      if c = '\'' then
	(v,j+1)
      else
	raise Exit
    else
      raise Exit
  and parse_jsonval_s x i l : string * int =
    let b = Buffer.create 10 in
    let j = ref i in
    while (!j < l && x.[!j] != '"') do
      if x.[!j] = '\\' then raise Exit; (** for cryptocurrency applications, don't need to parse escaped characters **)
      Buffer.add_char b x.[!j];
      incr j
    done;
    if !j < l then
      (Buffer.contents b,!j+1)
    else
      raise Exit
  and parse_jsonval_num x i l : string * int =
    let b = Buffer.create 10 in
    let j = ref i in
    while (!j < l && digit_p x.[!j]) do
      Buffer.add_char b x.[!j];
      incr j
    done;
    if (!j < l && x.[!j] = '.') then
      begin
	Buffer.add_char b '.';
	incr j;
	while (!j < l && digit_p x.[!j]) do
	  Buffer.add_char b x.[!j];
	  incr j
	done;
	(Buffer.contents b,!j)
      end
    else
      (Buffer.contents b,!j)
  and parse_jsonval_obj x i l : (string * jsonval) list * int =
    if i < l then
      let c = x.[i] in
      if c = '}' then
	([],i+1)
      else if c = '"' then
	let (y,j) = parse_jsonval_s x (i+1) l in
	parse_jsonval_obj_a x y j l
      else if whitespace_p c then
	parse_jsonval_obj x (i+1) l
      else
	raise Exit
    else
      raise Exit
  and parse_jsonval_obj_a x y i l : (string * jsonval) list * int =
    if i < l then
      let c = x.[i] in
      if whitespace_p c then
	parse_jsonval_obj_a x y (i+1) l
      else if c = ':' then
	let (v,k) = parse_jsonval_a x (i+1) l in
	parse_jsonval_obj_b x (y,v) k l
      else
	raise Exit
    else
      raise Exit
  and parse_jsonval_obj_b x (y,v) i l : (string * jsonval) list * int =
    if i < l then
      let c = x.[i] in
      if whitespace_p c then
	parse_jsonval_obj_b x (y,v) (i+1) l
      else if c = '}' then
	([(y,v)],i+1)
      else if c = ',' then
	let (r,m) = parse_jsonval_obj x (i+1) l in
	((y,v)::r,m)
      else
	raise Exit
    else
      raise Exit
  and parse_jsonval_arr x i l : jsonval list * int =
    if i < l then
      let c = x.[i] in
      if c = ']' then
	([],i+1)
      else if whitespace_p c then
	parse_jsonval_arr x (i+1) l
      else
	let (v,k) = parse_jsonval_a x i l in
	parse_jsonval_arr_a x v k l
    else
      raise Exit
  and parse_jsonval_arr_a x v i l : jsonval list * int =
    if i < l then
      let c = x.[i] in
      if whitespace_p c then
	parse_jsonval_arr_a x v (i+1) l
      else if c = ',' then
	let (r,m) = parse_jsonval_arr x (i+1) l in
	(v::r,m)
      else if c = ']' then
	([v],i+1)
      else
	raise Exit
    else
      raise Exit
  and parse_jsonval_null x i l =
    if i+2 < l && x.[i] = 'u' && x.[i+1] = 'l' && x.[i+2] = 'l' then
      (JsonNull,i+3)
    else
      raise Exit
  and parse_jsonval_true x i l =
    if i+2 < l && x.[i] = 'r' && x.[i+1] = 'u' && x.[i+2] = 'e' then
      (JsonBool true,i+3)
    else
      raise Exit
  and parse_jsonval_false x i l =
    if i+3 < l && x.[i] = 'a' && x.[i+1] = 'l' && x.[i+2] = 's' && x.[i+3] = 'e' then
      (JsonBool false,i+4)
    else
      raise Exit
  in
  parse_jsonval_a x i (String.length x)

let parse_jsonval x = parse_jsonval_start(x,0)

