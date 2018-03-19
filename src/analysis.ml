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

module StringMap = Map.Make (String)

(* ERROR MESSAGES *)
(*
* VAR_NOT_IN_SCOPE
* WRONG_NUM_OF_PARAMS

*)

(* A function consists of a name and number of params *)
type fd = FuncData of ustring * int
type environment_info = 
	| ErrorMsg of info * ustring * ustring (* fi, name, message *)
	| FunctionMsg of info * ustring * ustring * int * bool
	| VariableInfo of ustring
	| FunctionInfo of ustring * int

(* Function to append two lists *)
let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

(* Function to print a list of different environment_infos *)
let print_list lst = 
	let rec loop lst = 
		match lst with 
			| [] -> ()
			| x::xs -> match x with 
				| VariableInfo(name) -> loop xs
				| ErrorMsg(fi, name, msg) -> uprint_endline (msg ^. us": " ^. name); loop xs
				| _ -> ()
	in loop lst

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

let rec boolean_reduce f lst = 
	match lst with 
		| [] -> false
		| x::xs -> (f x) || (boolean_reduce f xs)

(* Does name exists in lst? *)
let rec exists name lst = 
	match lst with 
		| [] -> false 
		| x::xs -> (match x with 
			| VariableInfo(name2) -> if Ustring.equal name name2 then true else (exists name xs)
			| FunctionInfo(name2, num_args) -> if Ustring.equal name name2 then true else (exists name xs)
			| _ -> false
		)

let rec exists_funcdata item lst = 
	match lst with 
		| [] -> false 
		| x::xs -> (
			match x, item with 
				| FuncData(name, _), FuncData(name2, _) -> (if Ustring.equal name name2 then true else (exists_funcdata item xs))	
		)
		

(* Check if item exists in env-part of map env *)		
let exists_in_environment name env lst_name = 
	exists name (StringMap.find lst_name env)

(* Functions to handle environment 
   it is a map with lists, under the strings provided in lst_names
   Both contains Ustrings *)
let get_empty_environment lst_names =
	let rec loop lst = 
		match lst with 
			| [] -> StringMap.empty
			| x::xs -> StringMap.add x [] (loop xs)
	in loop lst_names

let print_environment env lst_name headline = 
	let lst = StringMap.find lst_name env in 
	print_endline headline;
	print_list lst

let get_scope_environment env keep_lst replace =
	let rec loop lst = 
		match lst with 
			| [] -> StringMap.empty
			| x::xs -> StringMap.add x (StringMap.find x env) (loop xs)
	in 
	let new_map = loop keep_lst in 
	StringMap.add replace [] new_map
let add_env_var env lst_name var = 
	let current = StringMap.find lst_name env in
	let new_content = var::current in 
	StringMap.add lst_name new_content env

let merge_environments env1 env2 lst_names = 
	let rec loop lst = 
		match lst with 
			| [] -> StringMap.empty
			| x::xs -> StringMap.add x (List.append (StringMap.find x env1) (StringMap.find x env2)) (loop xs)
	in loop lst_names
		
(*)

let merge_environments lst_name env1 env2 lst_names = 
	let errors = append (StringMap.find lst_name env1) (StringMap.find lst_name env2) in 
	StringMap.add lst_name errors (get_empty_environment lst_names)
*)
let get_num_params_from_func_data data = 
	match data with FuncData(name, num_params) -> num_params

let get_num_params_in_list lst name = 
	let rec loop lst = 
		match lst with 
			| [] -> 0
			| x::xs -> (
			match x with 
				| FunctionInfo(func_name, num_params) -> if Ustring.equal name func_name then num_params else (loop xs))
				| _ -> 0
		in loop lst

(* Function to analyze scope. 
   env is a list of two lists: 
   the first is the allowed variables 
   the second is the problematic variables *)
let analyze_scope ast errors = 
	let rec traverse ast env = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) ->
		 	(match tm with 
		 		| TmFunc(fi2, params, tm2) -> let data = FunctionInfo(name, (List.length params)) in traverse tm (add_env_var env "function_definitions" data)
	 			| TmVar(fi2, isconst2, name2) -> let varinfo = VariableInfo(name) in traverse tm (add_env_var env "env" varinfo)
		 		| TmAssign(fi2, name2, tm2) -> let varinfo = VariableInfo(name) in  traverse tm (add_env_var env "env" varinfo)
		 		| TmConst(fi2, const2) -> let varinfo = VariableInfo(name) in  traverse tm (if isconst then env else (add_env_var env "env" varinfo))
		 		| _ -> let varinfo = VariableInfo(name) in  traverse tm (add_env_var env "env" varinfo))
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body env
		 | TmIf(fi,tm1,tm2,tm3) -> merge_environments (traverse tm2 env) (match tm3 with 
		 	| Some(tm) -> traverse tm env 
		 	| None -> env ) ["env"; "errors"; "function_definitions"]
		 | TmAssign(fi,name,tm) -> traverse tm env
		 | TmRet(fi,tm) -> traverse tm env
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> 
		 	if exists_in_environment name env "env" then
		 		env
		 	else 
		 		let error = ErrorMsg(fi,name, us"VAR_NOT_IN_SCOPE") in add_env_var env "errors" error
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm (loop (fun name acc -> let varinfo = VariableInfo(name) in add_env_var acc "env" varinfo) params env)
		 | TmCall(fi,tm,tmlist) -> 
		 	(* Since this is a function call, we want to check if it is a defined function (in comparison to for instance print) *)
		 	(match tm with 
		 		| TmVar(fi2, isconst2, name) -> (
		 			if exists_in_environment name env "function_definitions" then
		 				if (List.length tmlist) <> (get_num_params_in_list (StringMap.find "function_definitions" env) name) then
		 					let error = ErrorMsg(fi2, name, us"WRONG_NUMBER_OF_PARAMS") in add_env_var env "errors" error 
		 				else env
		 			else 
		 				env
		 		)
		 		| _ -> loop traverse tmlist env)
		(* Other *)
		 | TmScope(fi,tmlist) -> get_scope_environment (loop traverse tmlist env) ["errors"; "function_definitions"] "env"
	in traverse ast (StringMap.add "errors" errors (get_empty_environment ["env";"function_definitions"]))

(* Function to find missing calls to declared functions.
   Populates a string map with a list of Ustring with the declared functions in 'function_definitions'
   and the calls in 'calls' and checks that every declared function is called *)
(*let find_missing_calls ast = 
	let rec traverse ast env = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) ->
		 	(match tm with 
		 		| TmFunc(fi2, const, tm2) -> traverse tm (add_env_var env "function_definitions" name)
	 			| TmVar(fi2, isconst2, name2) -> traverse tm env
		 		| TmAssign(fi2, name2, tm2) -> traverse tm env
		 		| TmConst(fi2, const2) -> traverse tm env
		 		| _ -> traverse tm env)
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body env
		 | TmIf(fi,tm1,tm2,tm3) -> merge_environments "function_definitions" (traverse tm2 env) (match tm3 with 
		 	| Some(tm) -> traverse tm env 
		 	| None -> env ) ["function_definitions";"calls"]
		 | TmAssign(fi,name,tm) -> traverse tm env
		 | TmRet(fi,tm) -> traverse tm env
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> env
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm env
		 | TmCall(fi,tm,tmlist) -> 
		 	(match tm with 
		 		| TmVar(fi2, isconst2, name) -> loop traverse tmlist (add_env_var env "calls" name)
		 		| _ -> loop traverse tmlist env)
		(* Other *)
		 | TmScope(fi,tmlist) -> loop traverse tmlist env
	in 
	let calls_map = traverse ast (get_empty_environment ["function_definitions";"calls"]) in 
	reduce (fun x acc -> if exists x (StringMap.find "calls" calls_map) then acc else x::acc) (StringMap.find "function_definitions" calls_map) []
*)
(* Function to check that the declared functions are called with the correct number of parameters.
   Populates a string map with a list of FuncData, which contains the name of the function and the
   number of parameters, both for the defined functions as well as the calls. For each defined function,
   check that all calls have the correct number of parameters. *)
(*let number_of_function_parameters ast = 
	let rec traverse ast env = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) ->
		 	(match tm with 
		 		| TmFunc(fi2, params, tm2) -> let data = FunctionInfo(name, (List.length params)) in traverse tm (add_env_var env "function_definitions" data)
	 			| TmVar(fi2, isconst2, name2) -> traverse tm env
		 		| TmAssign(fi2, name2, tm2) -> traverse tm env
		 		| TmConst(fi2, const2) -> traverse tm env
		 		| _ -> traverse tm env)
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body env
		 | TmIf(fi,tm1,tm2,tm3) -> merge_environments "function_definitions" (traverse tm2 env) (match tm3 with 
		 	| Some(tm) -> traverse tm env 
		 	| None -> env ) ["function_definitions";"errors"]
		 | TmAssign(fi,name,tm) -> traverse tm env
		 | TmRet(fi,tm) -> traverse tm env
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> env
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm env
		 | TmCall(fi,tm,tmlist) -> 
		 	(* Since this is a function call, we want to check if it is a defined function (in comparison to for instance print) *)
		 	(match tm with 
		 		| TmVar(fi2, isconst2, name) -> (
		 			if exists_in_environment name env "function_definitions" then
		 				if (List.length tmlist) <> (get_num_params_in_list (StringMap.find "function_definitions" env) name) then
		 					let error = ErrorMsg(fi2, name, us"WRONG_NUMBER_OF_PARAMS") in add_env_var env "errors" error 
		 				else env
		 			else 
		 				env
		 		)
		 		| _ -> loop traverse tmlist env)
		(* Other *)
		 | TmScope(fi,tmlist) -> loop traverse tmlist env
	in 
	traverse ast (get_empty_environment ["function_definitions";"errors"])
(*)
(* Function to detect uncatched return values
   by traversing the ast and detecting defined functions with return statements
   and checking that calls to those functions are contained in a assignment term *)
let detect_uncatched_return_values ast = 
	let rec is_non_void ast = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) -> is_non_void tm
		 | TmWhile (fi, tm_head, tm_body) -> is_non_void tm_body
		 | TmIf(fi,tm1,tm2,tm3) -> is_non_void tm2 || (match tm3 with 
		 	| Some(tm) -> is_non_void tm
		 	| None -> false )
		 | TmAssign(fi,name,tm) -> is_non_void tm
		 | TmRet(fi,tm) -> true
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> false
		 | TmConst(fi,const) -> false
		 | TmFunc(fi,params,tm) -> is_non_void tm
		 | TmCall(fi,tm,tmlist) -> false
		(* Other *)
		 | TmScope(fi,tmlist) -> boolean_reduce is_non_void tmlist
	in
	let rec traverse ast env = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) ->
		 	(match tm with 
		 		| TmFunc(fi2, const, tm2) -> if is_non_void tm then add_env_var env "function_definitions" name else env
	 			| TmVar(fi2, isconst2, name2) -> traverse tm env
		 		| TmAssign(fi2, name2, tm2) -> traverse tm env
		 		| TmConst(fi2, const2) -> traverse tm env
		 		| TmCall(fi2, tm, tmlist) -> env (* A call in a definition means that we handle return *)
		 		| _ -> traverse tm env)
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body env
		 | TmIf(fi,tm1,tm2,tm3) -> merge_environments "function_definitions" (traverse tm2 env) (match tm3 with 
		 	| Some(tm) -> traverse tm env 
		 	| None -> env ) ["function_definitions";"calls"]
		 | TmAssign(fi,name,tm) -> (
		 	match tm with 
		 		| TmCall(fi2, tm2, tmlist) -> env
		 		| _ -> traverse tm env
		 	)
		 | TmRet(fi,tm) -> (match tm with 
		 	| TmCall(fi2, tm2, tmlist) -> env
		 	|	_ -> traverse tm env
		 	)
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> env
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm env
		 | TmCall(fi,tm,tmlist) -> (* If we are here, we are not in an assignment *)
		 	(match tm with 
		 		| TmVar(fi2, isconst2, name) -> 
		 			if exists_in_environment name env "function_definitions" then
			 			add_env_var env "calls" name
			 		else env
		 		| _ -> env)
		(* Other *)
		 | TmScope(fi,tmlist) -> loop traverse tmlist env
	in 
	let calls_map = traverse ast (get_empty_environment ["function_definitions";"calls"]) in 
	StringMap.find "calls" calls_map
*)
*)

(* Our main function, called from jsh.ml when
	program is ran with argument 'analyze' *)

let analyze ast = 
	(*printf "Listing all variables in file: \n";
	let variables = fetch_variables ast in
	print_list variables;*)
	let analyze_results = analyze_scope ast [] in
	if (List.length (StringMap.find "errors" analyze_results)) > 0 then 
		print_environment analyze_results "errors" "Variables missing in scope:";
	(*let missing_calls = find_missing_calls ast in 
	if (List.length missing_calls) > 0 then 
		(print_endline "File contains missing calls:";
		print_list missing_calls);*)
	(*let function_parameters_result = number_of_function_parameters ast in 
	let function_parameters = StringMap.find "errors" function_parameters_result in
	if List.length function_parameters > 0 then
		(print_endline "Functions that has wrong number of parameters:";
		print_list function_parameters);*)
	(*let uncatched_returns = detect_uncatched_return_values ast in 
	if List.length uncatched_returns > 0 then 
		(print_endline "Function calls that do not catch return values:";
		print_list uncatched_returns)*)