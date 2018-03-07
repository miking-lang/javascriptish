open Printf
open Ast
open Ustring.Op
open Msg
(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE
   This file is created by Johan Myrsmeden. 

   The main file for performing program analysis.
*)

(* Function to append two lists *)
let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

(* Function to print a list of ustrings *)
let print_list lst = 
	uprint_endline (Ustring.concat (us", ") lst)

(* A reduce function : apply function f to each element in list
   and collect in accumulator *)
let rec reduce f lst acc = 
	match lst with 
		| [] -> []
		| x::xs -> append (f x acc) (reduce f xs acc)

(*let rec map f lst = 
	match lst with 
		| [] -> []
		| x::xs -> (f x)::(map f xs)*)

let map f l = List.fold_right (fun x a -> (f x) :: a) l []

(* Function to conduct a list of all variables in an ast *)
let fetch_variables ast =
	let rec traverse ast acc = 
	  match ast with
		(* Statements *)
		 | TmDef(_,_,name,tm) -> traverse tm acc
		 | TmWhile (_, tm_head, tm_body) -> traverse tm_head (traverse tm_body acc)
		 | TmIf(_,_,tm2,tm3) -> append (traverse tm2 acc) (match tm3 with 
		 	| Some(tm) -> traverse tm acc 
		 	| None -> acc )
		 | TmAssign(_,name,tm) -> traverse tm (name::acc)
		 | TmRet(_,_) -> acc
		(* Expressions *)
		 | TmVar(_,_,name) -> name::acc
		 | TmConst(_,_) -> acc
		 | TmFunc(_,_,tm) -> traverse tm acc
		 | TmCall(_,_,tmlist) -> reduce traverse tmlist acc
		(* Other *)
		 | TmScope(_,tmlist) -> reduce traverse tmlist acc
	in traverse ast []

let rename_variable ast =
	let rec traverse ast =  
	(*uprint_string(us"Traversing:" ^. (pprint ast));*)
	  match ast with
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) -> (
		 	match tm with 
		 		| TmFunc(fi2, params, tm2) -> TmDef(fi, isconst, name, TmFunc(fi, params, traverse tm2))
		 		| TmVar(fi2, isconst2, name) -> TmDef(fi, isconst, name, TmVar(fi2, isconst2, us"vardef"))
		 		| TmAssign(fi2,name,tm2) -> TmDef(fi, isconst, name, TmAssign(fi2, us"defas", traverse tm2))
		 		| _ -> TmDef(fi, isconst, us"odeffad", traverse tm))
		 | TmWhile (fi, tm_head, tm_body) -> TmWhile(fi, traverse tm_head, traverse tm_body)
		 | TmIf(fi,tm1,tm2,tm3) -> TmIf(fi, traverse tm1, traverse tm2, 
		 	(match tm3 with 
		 		| Some(tm) -> Some(traverse tm)
		 		| None -> tm3 ))
		 | TmAssign(fi,name,tm) -> TmAssign(fi, us"varis", traverse tm)
		 | TmRet(fi,tm) -> TmRet(fi, traverse tm)
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> TmVar(fi, isconst, us"variabeln")
		 | TmConst(fi,const) -> TmConst(fi, const)
		 | TmFunc(fi,const,tm) -> TmFunc(fi, const, traverse tm)
		 | TmCall(fi,tm,tmlist) -> TmCall(fi, tm, map traverse tmlist)
		(* Other *)
		 | TmScope(fi,tmlist) -> TmScope(fi, map traverse tmlist)
	in traverse ast

(* Our main function, called from jsh.ml when
	program is ran with argument 'analyze' *)

let analyze ast = 
	printf "Listing all variables in file: \n";
	let variables = fetch_variables ast in
	print_list variables;
	let renamed_tree = rename_variable ast in 
	printf "Program with renamed variables: \n";
	uprint_endline (pprint renamed_tree)