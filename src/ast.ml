(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE
*)

open Ustring.Op
open Msg


type bop =
| BopAdd  | BopSub    | BopMul   | BopDiv     | BopMod
| BopLess | BopLessEq | BopGreat | BopGreatEq
| BopEq   | BopNotEq

type uop =
| UopNot  | UopLogOr  | UopLogAnd


type tm =
(* Statements *)
| TmVarDef      of info * ustring * tm
| TmWhile       of info * tm * tm
| TmIf          of info * tm * tm * tm option
| TmAssign      of info * ustring * tm
| TmRet         of info * tm
| TmPrint       of info * tm
(* Expressions *)
| TmVar         of info * ustring
| TmBinop       of info * bop * tm * tm
| TmUnop        of info * uop * tm
| TmInt         of info * int
| TmBool        of info * bool
| TmFunc        of info * ustring list * tm
| TmCall        of info * tm * tm list
(* Other *)
| TmScope       of info * tm list




(* Returns the info field from a term *)
let tm_info t =
  match t with
  (* Statements *)
  | TmVarDef (fi,_,_) -> fi
  | TmWhile(fi,_,_) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmAssign(fi,_,_) -> fi
  | TmRet(fi,_) -> fi
  | TmPrint(fi,_) -> fi
  (* Expressions *)
  | TmVar(fi,_) -> fi
  | TmBinop(fi,_,_,_) -> fi
  | TmUnop(fi,_,_) -> fi
  | TmInt(fi,_) -> fi
  | TmBool(fi,_) -> fi
  | TmFunc(fi,_,_) -> fi
  | TmCall(fi,_,_) -> fi
  (* Other *)
  | TmScope(fi,_) -> fi


type 'a tokendata = {i:info; v:'a}


(* Pretty print a term. *)
let pprint tm =
  match tm with
  | TmVarDef(fi,s,t1) -> us""
  | TmWhile(fi,t1,t2) -> us""
  | TmIf(fi,t1,tt,tfop) -> us""
  | TmAssign(fi,s,t1) -> us""
  | TmRet(fi,t1) -> us""
  | TmPrint(fi,t1) -> us""
  | TmVar(fi,s) -> us""
  | TmBinop(fi,bop,t1,t2) -> us""
  | TmUnop(fi,uop,t1) -> us""
  | TmInt(fi,i) -> us""
  | TmBool(fi,b) -> us(if b then "true" else "false")
  | TmFunc(fi,slst,t1) -> us""
  | TmCall(fi,t1,tlst) -> us""
  | TmScope(fi,tlst) -> us""
