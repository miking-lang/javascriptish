(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE

   The following file defines the abstract syntax tree and
   its pretty printing function.
*)

open Ustring.Op
open Msg
open Printf

type const =
(* Primitive data types *)
 | CTrue | CFalse
 | CInt of int
(* Binary operators *)
 | CAdd  | CSub    | CMul   | CDiv     | CMod
 | CLess | CLessEq | CGreat | CGreatEq
 | CEq   | CNotEq
 | COrL  | CAndL
(* Unary operators *)
 | CNeg  | CNot
(* Utility functions *)
 | CPrint

(* If a variable is declared as const or as a mutable variable *)
type isconst = bool

type tm =
(* Statements *)
 | TmDef         of info * isconst * ustring * tm
 | TmWhile       of info * tm * tm
 | TmIf          of info * tm * tm * tm option
 | TmAssign      of info * ustring * tm
 | TmRet         of info * tm
(* Expressions *)
 | TmVar         of info * isconst * ustring
 | TmConst       of info * const
 | TmFunc        of info * ustring list * tm
 | TmCall        of info * tm * tm list
(* Other *)
 | TmScope       of info * tm list


(* Returns the info field from a term *)
let tm_info t =
  match t with
  (* Statements *)
  | TmDef (fi,_,_,_) -> fi
  | TmWhile(fi,_,_) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmAssign(fi,_,_) -> fi
  | TmRet(fi,_) -> fi
  (* Expressions *)
  | TmVar(fi,_,_) -> fi
  | TmConst(fi,_) -> fi
  | TmFunc(fi,_,_) -> fi
  | TmCall(fi,_,_) -> fi
  (* Other *)
  | TmScope(fi,_) -> fi


(* Kind of pretty printing *)
type print_type =
  | PrnNormal (* Pretty printing for standard javascriptish *)
  | PrnWeb    (* Pretty printing for web browsers *)
  | PrnNode   (* Pretty printing for Node.js *)


(* Pretty print a constant value *)
let rec pprint_const prec ptype n c args =
  let ppargs prec = Ustring.concat (us"") (List.map (pprint_general prec ptype n) args) in
  let ppa prec k = pprint_general prec ptype n (List.nth args k) in
  let precparan cprec ts = if prec > cprec then us"(" ^. ts ^. us")" else ts in
  (match c with
  (* Primitive data types *)
   | CTrue -> us"true"
   | CFalse -> us"false"
   | CInt(i) -> us(sprintf "%d" i)
  (* Binary operators *)
   | CAdd -> precparan 8 (ppa 8 0 ^. us" + " ^. ppa 8 1)
   | CSub -> precparan 8 (ppa 8 0 ^. us" - " ^. ppa 8 1)
   | CMul -> precparan 9 (ppa 9 0 ^. us" * " ^. ppa 9 1)
   | CDiv -> precparan 9 (ppa 9 0 ^. us" / " ^. ppa 9 1)
   | CMod -> precparan 9 (ppa 9 0 ^. us" % " ^. ppa 9 1)
   | CLess -> precparan 6 (ppa 6 0 ^. us" < " ^. ppa 6 1)
   | CLessEq -> precparan 6 (ppa 6 0 ^. us" <= " ^. ppa 6 1)
   | CGreat -> precparan 6 (ppa 6 0 ^. us" > " ^. ppa 6 1)
   | CGreatEq -> precparan 6 (ppa 6 0 ^. us" >= " ^. ppa 6 1)
   | CEq -> precparan 6 (ppa 6 0 ^. us" == " ^. ppa 6 1)
   | CNotEq -> precparan 6 (ppa 6 0 ^. us" != " ^. ppa 6 1)
   | COrL -> precparan 2 (ppa 2 0 ^. us" || " ^. ppa 2 1)
   | CAndL -> precparan 3 (ppa 3 0 ^. us" && " ^. ppa 3 1)
  (* Unary operators *)
   | CNeg -> precparan 12 (us"-" ^. ppargs 12)
   | CNot -> precparan 4 (us"!" ^. ppargs 4)
  (* Utility functions *)
   | CPrint ->
     (match ptype with
     | PrnNormal -> us"print"
     | PrnWeb -> us"document.write"
     | PrnNode -> us"console.log"))




(* Pretty print a term. *)
and pprint_general prec ptype n tm  =
  let rec mkspace n = if n = 0 then us"" else mkspace n ^. us" " in
  let tabsize = 4 in
  let rec pp prec n stmt tm  =
    (if stmt then mkspace (tabsize*n) else us"") ^.
    (match tm with
    | TmDef(fi,isconst,s,t1) ->
        if isconst then us"const " else us"var " ^.
        s ^. us" = " ^. pp prec n stmt t1
    | TmWhile(fi,t1,t2) -> us""
    | TmIf(fi,t1,tt,tfop) -> us""
    | TmAssign(fi,s,t1) -> us""
    | TmRet(fi,t1) -> us""
    | TmVar(fi,isconst,s) -> s
    | TmConst(fi,c) -> pprint_const prec ptype n c []
    | TmFunc(fi,slst,t1) -> us""
    | TmCall(fi,t1,tlst) ->
      (match t1 with
      | TmConst(fi,c) -> pprint_const prec ptype n c tlst
      | _ -> failwith "TODO")
    | TmScope(fi,tlst) ->
      Ustring.concat (us"") (List.map (pp prec n true) tlst)
    ) ^. if stmt then us"\n" else us""
  in
    pp prec n false tm


(* Short cut for printing out normal *)
let pprint tm = pprint_general 0 PrnNormal 0 tm


(* Info type used for pretty printing error messages *)
type 'a tokendata = {i:info; v:'a}
