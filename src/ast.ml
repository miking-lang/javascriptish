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
(* Unary operators *)
 | CNot  | COrL  | CAndL
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
let pprint_const c ptype =
(match c with
(* Primitive data types *)
 | CTrue -> "true"
 | CFalse -> "false"
 | CInt(i) -> sprintf "%d" i
(* Binary operators *)
 | CAdd -> "+"
 | CSub -> "-"
 | CMul -> "*"
 | CDiv -> "/"
 | CMod -> "%"
 | CLess -> "<"
 | CLessEq -> "<="
 | CGreat -> ">"
 | CGreatEq -> ">="
 | CEq -> "=="
 | CNotEq -> "!="
(* Unary operators *)
 | CNot -> "!"
 | COrL -> "||"
 | CAndL -> "&&"
(* Utility functions *)
 | CPrint ->
   (match ptype with
   | PrnNormal -> "print"
   | PrnWeb -> "document.write"
   | PrnNode -> "console.log")
)|> us



(* Pretty print a term. *)
let pprint_general ptype n tm  =
  let rec mkspace n = if n = 0 then us"" else mkspace n ^. us" " in
  let tabsize = 4 in
  let rec pp n stmt tm  =
    (if stmt then mkspace (tabsize*n) else us"") ^.
    (match tm with
    | TmDef(fi,isconst,s,t1) -> us""
    | TmWhile(fi,t1,t2) -> us""
    | TmIf(fi,t1,tt,tfop) -> us""
    | TmAssign(fi,s,t1) -> us""
    | TmRet(fi,t1) -> us""
    | TmVar(fi,isconst,s) -> us""
    | TmConst(fi,c) -> pprint_const c ptype ^. if stmt then us"\n" else us""
    | TmFunc(fi,slst,t1) -> us""
    | TmCall(fi,t1,tlst) -> us""
    | TmScope(fi,tlst) ->
       Ustring.concat (us"") (List.map (pp n true) tlst))
  in
    pp n false tm


(* Short cut for printing out normal *)
let pprint tm = pprint_general PrnNormal 0 tm


(* Info type used for pretty printing error messages *)
type 'a tokendata = {i:info; v:'a}
