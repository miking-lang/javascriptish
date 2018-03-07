open Printf
open Ast
open Ustring.Op
open Msg
(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE

   The main file for performing program analysis.
*)

let rec reduce f tmlist acc =
	match tmlist with 
		| [] -> acc
		| tm::tms -> reduce f tms (f acc tm)

let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

let print_list lst = 
	uprint_endline (Ustring.concat (us", ") lst)

let rec traverse_tmlist f tmlist acc = 
	match tmlist with 
		| [] -> []
		| tm::tms -> append (f tm acc) (traverse_tmlist f tms acc)

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
		 | TmCall(_,_,tmlist) -> traverse_tmlist traverse tmlist acc
		(* Other *)
		 | TmScope(_,tmlist) -> traverse_tmlist traverse tmlist acc
	in traverse ast []


let analyze ast = 
	printf "Listing all variables in file: \n";
	let variables = fetch_variables ast in
	print_list variables