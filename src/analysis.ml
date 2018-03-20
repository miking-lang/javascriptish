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
	| FunctionInfo of info * ustring * int * bool * bool (* name, number_of_arguments, called?, non_void? *)
	| CallInfo of info * ustring

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
				| VariableInfo(name) -> uprint_endline(us"Variable: " ^. name); loop xs
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
			| FunctionInfo(info, name2, num_args, called, non_void) -> if Ustring.equal name name2 then true else (exists name xs)
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

let print_errors lst headline = 
	print_endline headline;
	print_list lst

let get_scope_environment env old_env keep_lst replace =
	let rec loop lst = 
		match lst with 
			| [] -> StringMap.empty
			| x::xs -> StringMap.add x (StringMap.find x env) (loop xs)
	in 
	let new_map = loop keep_lst in 
	let old_content = StringMap.find replace old_env in 
	StringMap.add replace old_content new_map
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

(* Mark function with provided name as called *)
let mark_as_called env function_name = 
	let rec loop lst = 
		match lst with 
			| [] -> []
			| x::xs -> match x with 
				| FunctionInfo(fi, name, num_args, called, non_void) -> (
					if Ustring.equal name function_name then 
						let new_function = FunctionInfo(fi, name, num_args, true, non_void) in 
						new_function::xs
					else 
						x::(loop xs)
					)
				| _ -> loop xs
	in StringMap.add "function_definitions" (loop (StringMap.find "function_definitions" env)) env

let does_return_value env function_name = 
	let rec loop lst = 
		match lst with 
			| [] -> false
			| x::xs -> match x with 
				| FunctionInfo(fi, name, num_args, called, non_void) -> (
					if Ustring.equal name function_name then 
						non_void
					else 
						loop xs
					)
				| _ -> loop xs
	in loop (StringMap.find "function_definitions" env)
	
let get_num_params_from_func_data data = 
	match data with FuncData(name, num_params) -> num_params

let get_num_params_in_list lst name = 
	let rec loop lst = 
		match lst with 
			| [] -> 0
			| x::xs -> (
			match x with 
				| FunctionInfo(info, func_name, num_params, called, non_void) -> if Ustring.equal name func_name then num_params else (loop xs)
				| _ -> loop xs)
		in loop lst

(* Fnction to check if the term ends with a return statement or not *)
let rec is_non_void tm = 
	match tm with 
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

let check_function_for_return env tm in_assignment =
	(match tm with 
 		| TmVar(fi2, isconst2, name) -> (
 			if not in_assignment && does_return_value env name then 
 				let error = ErrorMsg(fi2, name, us"UNCATCHED_RETURN") in
 				add_env_var env "errors" error
 			else
 				env)
 		| _ -> env
 		)

let handle_tm_call f env tm tmlist in_assignment = 
	(* Since this is a function call, we want to check if it is a defined function (in comparison to for instance print) *)
 	(match tm with 
 		| TmVar(fi2, isconst2, name) -> (
 			let env = check_function_for_return env tm in_assignment in
 			if exists_in_environment name env "function_definitions" then
 				(* Mark function as called *)
 				let env = mark_as_called env name in
 				if (List.length tmlist) <> (get_num_params_in_list (StringMap.find "function_definitions" env) name) then
 					let error = ErrorMsg(fi2, name, us"WRONG_NUMBER_OF_PARAMS") in add_env_var env "errors" error 
 				else env
 			else 
 				env
 		)
 		| _ -> loop f tmlist env)

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
		 		| TmFunc(fi2, params, tm2) -> let data = FunctionInfo(fi, name, (List.length params), false, (is_non_void tm)) in traverse tm (add_env_var env "function_definitions" data)
	 			| TmVar(fi2, isconst2, name2) -> let varinfo = VariableInfo(name) in traverse tm (add_env_var env "env" varinfo)
		 		| TmAssign(fi2, name2, tm2) -> let varinfo = VariableInfo(name) in traverse tm (add_env_var env "env" varinfo)
		 		| TmConst(fi2, const2) -> let varinfo = VariableInfo(name) in traverse tm (if isconst then env else (add_env_var env "env" varinfo))
		 		| TmCall(fi2, tm2, tmlist) -> (* A call in a definition means that we handle a return value *) let varinfo = VariableInfo(name) in handle_tm_call traverse (add_env_var env "env" varinfo) tm2 tmlist true
		 		| _ -> let varinfo = VariableInfo(name) in  traverse tm (add_env_var env "env" varinfo))
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body env
		 | TmIf(fi,tm1,tm2,tm3) -> merge_environments (traverse tm2 env) (match tm3 with 
		 	| Some(tm) -> traverse tm env 
		 	| None -> env ) ["env"; "errors"; "function_definitions"]
		 | TmAssign(fi,name,tm) -> (
		 	let varinfo = VariableInfo(name) in
		 	let env = (add_env_var env "env" varinfo) in
		 	match tm with 
		 		| TmCall(fi2, tm2, tmlist) -> handle_tm_call traverse env tm2 tmlist true (* Handling return value *)
		 		| _ -> traverse tm env
		 	)
		 | TmRet(fi,tm) -> (match tm with 
		 	| TmCall(fi2, tm2, tmlist) -> handle_tm_call traverse env tm2 tmlist true (* Handling return value *)
		 	|	_ -> traverse tm env
		 	)
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> 
		 	if exists_in_environment name env "env" then
		 		env
		 	else 
		 		let error = ErrorMsg(fi,name, us"VAR_NOT_IN_SCOPE") in add_env_var env "errors" error
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm (loop (fun name acc -> let varinfo = VariableInfo(name) in add_env_var acc "env" varinfo) params env)
		 | TmCall(fi,tm,tmlist) -> 
		 	(* If we are here, we are not in an assignment *)
		 	handle_tm_call traverse env tm tmlist false
		(* Other *)
		 | TmScope(fi,tmlist) -> get_scope_environment (loop traverse tmlist env) env ["errors"; "function_definitions"] "env"
	in 
	let environment = traverse ast (StringMap.add "errors" errors (get_empty_environment ["env";"function_definitions"])) in 
	let errors = StringMap.find "errors" environment in 
	let function_definitions = StringMap.find "function_definitions" environment in 
	reduce (fun x acc -> 
		match x with 
			| FunctionInfo(fi, name, num_params, called, non_void) -> (
				if not called then 
					let error = ErrorMsg(fi, name, us"FUNCTION_NOT_CALLED") in 
					error::acc
				else 
					acc
				)
			| _ -> acc
	) function_definitions errors

(* Our main function, called from jsh.ml when
	program is ran with argument 'analyze' *)

let analyze ast = 
	(*printf "Listing all variables in file: \n";
	let variables = fetch_variables ast in
	print_list variables;*)
	let analyze_results = analyze_scope ast [] in
	if (List.length analyze_results) > 0 then 
		print_errors analyze_results "Found errors:";
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