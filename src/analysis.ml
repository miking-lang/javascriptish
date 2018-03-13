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

(* A function to loop over elements in lst 
   calling function f with accumulator acc *)
let rec loop f lst acc = 
	match lst with 
		| [] -> acc
		| x::xs -> loop f xs (f x acc)

let map f l = List.fold_right (fun x a -> (f x) :: a) l []
let map_two_args f l y = List.fold_right (fun x a -> (f x y) :: a) l []

(* Does item exists in lst? *)
let rec exists item lst = 
	match lst with 
		| [] -> false 
		| x::xs -> if Ustring.equal x item then true else (exists item xs)

(* Function to conduct a list of all variables in an ast *)
let fetch_variables ast =
	let rec traverse ast acc = 
	uprint_endline (pprint ast);
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

(* Fetch all variables until new scope is reached *)
let fetch_variables_scope ast =
	let rec traverse ast acc = 
	uprint_endline (pprint ast);
	  match ast with
		(* Statements *)
		 | TmDef(_,isconst,name,tm) -> traverse tm (if isconst then acc else (name::acc))
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
		 | TmScope(_,tmlist) -> acc
	in traverse ast []

(* Function to rename some variables, only to practice transformation of the AST *)
let rename_variable ast =
	let rec traverse ast =  
	(*uprint_string(us"Traversing:" ^. (pprint ast));*)
	  match ast with
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) -> (
		 	match tm with 
		 		| TmFunc(fi2, params, tm2) -> TmDef(fi, isconst, name, TmFunc(fi, params, traverse tm2))
		 		| TmVar(fi2, isconst2, name2) -> TmDef(fi, isconst, name, TmVar(fi2, isconst2, us"vardef"))
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

let replace_in_list lst name_to_replace new_name = 
	let replace x a = 
		(if (Ustring.equal x name_to_replace) then new_name else x) :: a
	in map replace lst

let rename_in_scope ast name_to_replace new_name = 
	let rec traverse ast = 
		uprint_endline (pprint ast);
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) -> uprint_endline (us"Def av " ^. name); 
		 	(match tm with 
		 		| TmFunc(fi2, const, tm2) -> 
		 		print_string "Def is func";
		 		uprint_endline(pprint tm2);
		 			if Ustring.equal name name_to_replace then
		 				TmDef(fi, isconst, new_name, traverse tm)
		 			else
		 				TmDef(fi, isconst, name, traverse tm)
		 		| _ -> TmDef(fi, isconst, name, traverse tm))
		 | TmWhile (fi, tm_head, tm_body) -> TmWhile(fi, traverse tm_head, traverse tm_body)
		 | TmIf(fi,tm1,tm2,tm3) -> TmIf(fi, traverse tm1, traverse tm2, 
		 	(match tm3 with 
		 		| Some(tm) -> Some(traverse tm)
		 		| None -> tm3 ))
		 | TmAssign(fi,name,tm) -> print_endline "Assign";
		 	if Ustring.equal name name_to_replace then
		 		TmAssign(fi, new_name, traverse tm)
		 	else
		 		TmAssign(fi, name, traverse tm)
		 | TmRet(fi,tm) -> print_endline "Ret"; TmRet(fi, traverse tm)
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> 
		 	print_string "-Var";
		 	if Ustring.equal name name_to_replace then
		 		TmVar(fi, isconst, new_name)
		 	else
		 		TmVar(fi, isconst, name)
		 | TmConst(fi,const) -> print_endline "Const"; TmConst(fi, const)
		 | TmFunc(fi,params,tm) -> print_string "Params:"; print_list params; TmFunc(fi, params, traverse tm)
		 | TmCall(fi,tm,tmlist) -> TmCall(fi, tm, map traverse tmlist)
		(* Other *)
		 | TmScope(fi,tmlist) -> print_endline "Scope"; TmScope(fi, map traverse tmlist)
	in traverse ast

let analyze_scope ast = 
	let rec traverse ast env = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) ->
		 	(match tm with 
		 		| TmFunc(fi2, const, tm2) -> traverse tm env
	 			| TmVar(fi2, isconst2, name2) -> traverse tm (name::env)
		 		| TmAssign(fi2, name2, tm2) -> traverse tm (name::env)
		 		| TmConst(fi2, const2) -> traverse tm (if isconst then env else (name::env))
		 		| _ -> traverse tm (name::env))
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body env
		 | TmIf(fi,tm1,tm2,tm3) -> append (traverse tm2 env) (match tm3 with 
		 	| Some(tm) -> traverse tm env 
		 	| None -> env )
		 | TmAssign(fi,name,tm) -> traverse tm env
		 | TmRet(fi,tm) -> traverse tm env
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> 
		 	(if exists name env then
		 		uprint_endline (name ^. us" EXISTS in scope")
		 	else 
		 		uprint_endline (name ^. us" DOES NOT EXIST in scope"));
		 	env
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm env
		 | TmCall(fi,tm,tmlist) -> loop traverse tmlist env
		(* Other *)
		 | TmScope(fi,tmlist) -> (loop traverse tmlist env); env
	in traverse ast []


(* Our main function, called from jsh.ml when
	program is ran with argument 'analyze' *)

let analyze ast = 
	(*printf "Listing all variables in file: \n";
	let variables = fetch_variables ast in
	print_list variables;*)
	let analyze_results = analyze_scope ast in 
	print_list analyze_results