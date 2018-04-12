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

type environment_info = 
	| ErrorMsg of message (* See msg.ml *)
	| VariableInfo of info * ustring (* fi, name *)
	| FunctionInfo of info * ustring * int * bool * bool * ustring list (* name, number_of_arguments, called?, non_void?, parameter names*)

let print_env_info item = 
	match item with
		| VariableInfo(_, name) -> uprint_string(us"Variable: " ^. name); print_string "\n"
		| ErrorMsg(msg) -> uprint_endline (message2str(msg))
		| FunctionInfo (_,name,_,_,_,_) -> uprint_string(us"Function: " ^. name); print_string "\n"

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
			| x::xs -> print_env_info x; loop xs
	in loop lst

(* A reduce function : apply function f to each element in list
   and collect in accumulator *)
let rec reduce f lst acc = 
	match lst with 
		| [] -> acc
		| x::xs -> append(f x acc) (reduce f xs acc)

(* A function to loop over elements in lst 
   calling function f with accumulator acc *)
let rec loop f lst acc = 
	match lst with 
		| [] -> acc
		| x::xs -> loop f xs (f x acc)

let rec boolean_reduce f lst = 
	match lst with 
		| [] -> false
		| x::xs -> (f x) || (boolean_reduce f xs)

let rec boolean_reduce_and f lst = 
	match lst with 
		| [] -> true
		| x::xs -> (f x) && (boolean_reduce f xs)

(* Does name exists in lst? *)
let rec exists name lst = 
	match lst with 
		| [] -> false 
		| x::xs -> (match x with 
			| VariableInfo(_,name2) -> if Ustring.equal name name2 then true else (exists name xs)
			| FunctionInfo(_,name2,_,_,_,_) -> if Ustring.equal name name2 then true else (exists name xs)
			| _ -> false
		)		

(* Check if item exists in env-part of map env *)		
let exists_in_environment name env lst_name = 
	exists name (StringMap.find lst_name env)

(* Functions to handle environment 
   Environment is a map with lists of environment_info, with string keys *)
let get_empty_environment lst_names =
	let rec loop lst = 
		match lst with 
			| [] -> StringMap.empty
			| x::xs -> StringMap.add x [] (loop xs)
	in loop lst_names

let print_errors lst headline = 
	print_endline headline;
	print_list lst

(* Function that keeps the lists from old_env given by the keep_lst and replaces the one given by replace *)
let get_scope_environment env old_env keep_lst replace =
	let rec loop lst = 
		match lst with 
			| [] -> StringMap.empty
			| x::xs -> StringMap.add x (StringMap.find x env) (loop xs)
	in 
	let new_map = loop keep_lst in 
	let old_content = StringMap.find replace old_env in 
	StringMap.add replace old_content new_map

(* Function to add variable to environment *)
let add_env_var env lst_name var = 
	let current = StringMap.find lst_name env in
	let new_content = var::current in 
	StringMap.add lst_name new_content env

(* Mark function with provided name as called *)
let mark_as_called env function_name = 
	let rec loop lst = 
		match lst with 
			| [] -> []
			| x::xs -> match x with 
				| FunctionInfo(fi, name, num_args, called, non_void, params) -> (
					if Ustring.equal name function_name then 
						let new_function = FunctionInfo(fi, name, num_args, true, non_void, params) in 
						new_function::xs
					else 
						x::(loop xs)
					)
				| _ -> loop xs
	in StringMap.add "function_definitions" (loop (StringMap.find "function_definitions" env)) env

(* Check if a function does return a value *)
let does_return_value env function_name = 
	let rec loop lst = 
		match lst with 
			| [] -> false
			| x::xs -> match x with 
				| FunctionInfo(_,name,_,_,non_void,_) -> (
					if Ustring.equal name function_name then 
						non_void
					else 
						loop xs
					)
				| _ -> loop xs
	in loop (StringMap.find "function_definitions" env)

(* Get the number of parameters for the function 
   with the provided name in a list of environment 
   infos (variant function info) *)
let get_num_params_in_list lst name = 
	let rec loop lst = 
		match lst with 
			| [] -> 0
			| x::xs -> (
			match x with 
				| FunctionInfo(info,func_name,num_params,_,_,_) -> if Ustring.equal name func_name then num_params else (loop xs)
				| _ -> loop xs)
		in loop lst

(* Get the parameters for the function 
   with the provided name in a list of environment 
   infos (variant function info) *)
let get_params_in_list lst name = 
	let rec loop lst = 
		match lst with 
			| [] -> []
			| x::xs -> (
			match x with 
				| FunctionInfo(info,func_name,_,_,_,params) -> if Ustring.equal name func_name then params else (loop xs)
				| _ -> loop xs)
		in loop lst

(* Is our constant a boolean? *)
let is_const_bool const = 
	match const with 
		| CTrue -> true
		| CFalse -> true
		| _ -> false
(* Get minimum value of a, b and c *)
let minimum a b c = 
	if a < b then (
		if a < c then 
			a 
		else 
			c
		)
	else (
		if b < c then 
			b 
		else
			c
	)
(* The Levenshtein Distance algorithm. Gives the lowest number 
   of mutations between Ustring s and Ustring t *)
let rec levenshtein_distance s len_s t len_t = 
	if len_s = 0 then len_t
	else (
		if len_t = 0 then len_s
		else (
			let cost = if Ustring.equal (Ustring.sub s (len_s-1) 1) (Ustring.sub t (len_t-1) 1) then 
				0
			else 
				1 in 
			minimum 
				((levenshtein_distance s (len_s-1) t len_t) + 1)
				((levenshtein_distance s len_s t (len_t-1)) + 1)
				((levenshtein_distance s (len_s-1) t (len_t-1)) + cost)
			
		)
	) 

(* Function to find possible variable names if the provided name is misspelled *)
let find_possible_variables name env = 
	let rec loop lst distance suggestion = 
		match lst with 
			| [] -> suggestion
			| x::xs -> (
				match x with 
					| VariableInfo(_, x) -> (let dist = levenshtein_distance name (Ustring.length name) x (Ustring.length x) in 
							if dist < distance then 
								(loop xs dist x)
							else 
								(loop xs distance suggestion))
					| FunctionInfo(_,_,_,_,_,_) -> us""
					| ErrorMsg(_) -> us"")
	in loop (StringMap.find "env" env) 10 (us"")


(* Function to check if the term ends with a return statement or not *)
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
	 | TmBreak(fi) -> false

(* Check if a function returns a value and if it is caught or not *)
let check_function_for_return env tm in_assignment =
	(match tm with 
 		| TmVar(fi2, isconst2, name) -> (
 			if not in_assignment && does_return_value env name then 
 				let error = ErrorMsg(UNCAUGHT_RETURN, WARNING, fi2, [name]) in
 				add_env_var env "errors" error
 			else
 				env)
 		| _ -> env)

let uses_same_parameter_names params function_name function_definitions = 
	let rec is_in lst value = 
		match lst with 
			| [] -> false 
			| x::xs -> (match x with 
				| TmVar(fi, isconst2, name) -> if Ustring.equal name value then true else (is_in xs value)
				| _ -> print_endline "Something else!!!"; false)
		in
	let function_params = get_params_in_list function_definitions function_name in
	boolean_reduce_and (fun x -> is_in params x) function_params


(* The function that handles calls, and checks for 
   * Returns
   * Number of parameters 
   * Argument definitions *)
let rec handle_tm_call f env tm tmlist in_assignment = 
 	(match tm with 
 		| TmVar(fi2, isconst2, name) -> (
 			loop (fun tm2 acc -> 
 				(match tm2 with 
 					| TmCall(fi3, tm3, tmlist3) -> handle_tm_call f acc tm3 tmlist3 true
 					| _ -> f tm2 acc)) tmlist (
 			let env = check_function_for_return env tm in_assignment in
 			if exists_in_environment name env "function_definitions" then (* Since this is a function call, we want to check if it is a defined function (in comparison to for instance print) *)
 				(* Mark function as called *)
 				let env = mark_as_called env name in
 				(* Calculate number of parameters and arguments *)
 				let expected_num_params = get_num_params_in_list (StringMap.find "function_definitions" env) name in
 				let provided_num_args = List.length tmlist in
 				if provided_num_args <> expected_num_params then
 					let error = ErrorMsg(WRONG_NUMBER_OF_PARAMS, ERROR, fi2, [name; (ustring_of_int expected_num_params); (ustring_of_int provided_num_args)]) in add_env_var env "errors" error 
 				else (
 					(* Number of parameters were correct, see if all variables passed as arguments are called the same things as the parameters *)
 					if uses_same_parameter_names tmlist name (StringMap.find "function_definitions" env) then 
 						let error = ErrorMsg(SAME_PARAMETER_NAMES, NOTE, fi2,[name]) in add_env_var env "errors" error 
 					else env)
 			else 
 				env))
 		| TmConst(fi, const) -> (* We are using a const function, which means that we are handling return value *)
 			loop (fun tm2 acc -> 
 				(match tm2 with 
 					| TmCall(fi3, tm3, tmlist3) -> handle_tm_call f acc tm3 tmlist3 true
 					| _ -> f tm2 acc)
 			) tmlist env
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
		 		| TmFunc(fi2, params, tm2) -> let data = FunctionInfo(fi, name, (List.length params), false, (is_non_void tm), params) in traverse tm (add_env_var env "function_definitions" data)
	 			| TmVar(fi2, isconst2, name2) -> let varinfo = VariableInfo(fi, name) in traverse tm (add_env_var env "env" varinfo)
		 		| TmAssign(fi2, name2, tm2) -> let varinfo = VariableInfo(fi, name) in traverse tm (add_env_var env "env" varinfo)
		 		| TmConst(fi2, const2) -> let varinfo = VariableInfo(fi, name) in traverse tm (if isconst then env else (add_env_var env "env" varinfo))
		 		| TmCall(fi2, tm2, tmlist) -> (* A call in a definition means that we handle a return value *) let varinfo = VariableInfo(fi, name) in handle_tm_call traverse (add_env_var env "env" varinfo) tm2 tmlist true
		 		| _ -> let varinfo = VariableInfo(fi, name) in  traverse tm (add_env_var env "env" varinfo))
		 | TmWhile (fi, tm_head, tm_body) -> traverse tm_body (
		 	match tm_head with 
		 		| TmCall(fi1, tm, tmlist) -> handle_tm_call traverse env tm tmlist true
		 		| _ -> traverse tm_head env)
		 | TmIf(fi,tm1,tm2,tm3) -> (
		 	let env = traverse tm2 (match tm3 with 
		 		| Some(tm) -> traverse tm env 
		 		| None -> env ) in
		 	match tm1 with 
		 		| TmCall(fi1, tm, tmlist) -> handle_tm_call traverse env tm tmlist true
		 		| _ -> traverse tm1 env)
		 | TmAssign(fi,name,tm) -> 
		 	(if exists_in_environment name env "env" then
		 		(match tm with 
		 		| TmCall(fi2, tm2, tmlist) -> handle_tm_call traverse env tm2 tmlist true (* Handling return value *)
		 		| _ -> traverse tm env)
		 	else 
		 		(let suggestion = find_possible_variables name env in 
		 		let error = ErrorMsg(VAR_NOT_IN_SCOPE, ERROR, fi, [name;suggestion]) in 
		 		let env = add_env_var env "errors" error in
				match tm with 
		 			| TmCall(fi2, tm2, tmlist) -> handle_tm_call traverse env tm2 tmlist true (* Handling return value *)
		 			| _ -> traverse tm env))
		 | TmRet(fi,tm) -> (match tm with 
		 	| TmCall(fi2, tm2, tmlist) -> handle_tm_call traverse env tm2 tmlist true (* Handling return value *)
		 	|	_ -> traverse tm env)
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> 
		 	if exists_in_environment name env "env" then
		 		env
		 	else 
		 		(let suggestion = find_possible_variables name env in 
		 		let error = ErrorMsg(VAR_NOT_IN_SCOPE, ERROR, fi, [name;suggestion]) in 
		 		add_env_var env "errors" error)
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm (loop (fun name acc -> let varinfo = VariableInfo(fi, name) in add_env_var acc "env" varinfo) params env)
		 | TmCall(fi,tm,tmlist) -> 
		 	(* If we are here, we are not in an assignment *)
		 	handle_tm_call traverse env tm tmlist false
	 	 | TmBreak(fi) -> env
		(* Other *)
		 | TmScope(fi,tmlist) -> get_scope_environment (loop traverse tmlist env) env ["errors"; "function_definitions"] "env"
	in 
	let environment = traverse ast (StringMap.add "errors" errors (get_empty_environment ["env";"function_definitions"])) in 
	let errors = StringMap.find "errors" environment in 
	let function_definitions = StringMap.find "function_definitions" environment in 
	loop (fun x acc -> 
		match x with 
			| FunctionInfo(fi,name,num_params,called,non_void,_) -> (
				if not called then 
					let error = ErrorMsg(FUNCTION_NOT_CALLED, WARNING, fi, [name]) in 
					error::acc
				else 
					acc)
			| _ -> acc) function_definitions errors

(* Function that checks for patterns 
   that indicate the usage of flag 
   variables instead of break statements *)
let check_loops ast errors = 
	let rec traverse ast env = 
		match ast with 
		(* Statements *)
		 | TmDef(fi,isconst,name,tm) -> traverse tm env
		 | TmWhile (fi, tm_head, tm_body) -> (match tm_body with 
		 		| TmScope(fi, tmlist) ->
		 			(loop (fun tm env -> 
		 				(match tm with 
		 					| TmIf(fi,tm1,tm2,tm3) -> 
		 						(match tm2 with 
		 							| TmScope(fi, tmlist) -> 
		 								(loop (fun tm5 env -> 
		 									(match tm5 with 
					 							| TmAssign(fi, name, tm) -> (match tm with 
					 								| TmConst(fi2, const) -> let varinfo = VariableInfo(fi, name) in add_env_var env "booleans" varinfo
					 								| TmDef(_,_,_,_) -> env
									 				| TmWhile(_,_,_) -> env
									 				| TmIf(_,_,_,_) -> env
									 				| TmAssign(_,_,_) -> env
									 				| TmRet(_,_) -> env
									 				| TmVar(_,_,_) -> env
									 				| TmFunc(_,_,_) -> env
									 				| TmCall(_,_,_) -> env
									 				| TmBreak(_) -> env
													| TmScope(_,_) -> env)
					 							| TmDef(_,_,_,_) -> env
								 				| TmWhile(_,_,_) -> env
								 				| TmIf(_,_,_,_) -> env
								 				| TmRet(_,_) -> env
								 				| TmVar(_,_,_) -> env
								 				| TmConst(_,_) -> env
								 				| TmFunc(_,_,_) -> env
								 				| TmCall(_,_,_) -> env
								 				| TmBreak(_) -> env
												| TmScope(_,_) -> env)) tmlist env)
		 							| TmDef(_,_,_,_) -> env
					 				| TmWhile(_,_,_) -> env
					 				| TmIf(_,_,_,_) -> env
					 				| TmAssign(_,_,_) -> env
					 				| TmRet(_,_) -> env
					 				| TmVar(_,_,_) -> env
					 				| TmConst(_,_) -> env
					 				| TmFunc(_,_,_) -> env
					 				| TmCall(_,_,_) -> env
					 				| TmBreak(_) -> env)
		 					| TmDef(_,_,_,_) -> env
			 				| TmWhile(_,_,_) -> env
			 				| TmAssign(_,_,_) -> env
			 				| TmRet(_,_) -> env
			 				| TmVar(_,_,_) -> env
			 				| TmConst(_,_) -> env
			 				| TmFunc(_,_,_) -> env
			 				| TmCall(_,_,_) -> env
			 				| TmBreak(_) -> env
							| TmScope(_,_) -> env)) tmlist env)
		 		| TmDef(_,_,_,_) -> env
 				| TmWhile(_,_,_) -> env
 				| TmIf(_,_,_,_) -> env
 				| TmAssign(_,_,_) -> env
 				| TmRet(_,_) -> env
 				| TmVar(_,_,_) -> env
 				| TmConst(_,_) -> env
 				| TmFunc(_,_,_) -> env
 				| TmCall(_,_,_) -> env
 				| TmBreak(_) -> env)
		 | TmIf(fi,tm1,tm2,tm3) -> 
		 	(match tm1 with 
		 		| TmVar(fi2, isconst, name) -> (
		 			if exists_in_environment name env "booleans" then
		 				let error = ErrorMsg(BOOLEAN_INSTEAD_OF_BREAK, WARNING, fi2, []) in 
		 				add_env_var env "errors" error
		 			else 
		 				env)
		 		| _ -> env)
		 | TmAssign(fi,name,tm) -> 
		 	(let varinfo = VariableInfo(fi, name) in
		 	add_env_var env "env" varinfo)
		 | TmRet(fi,tm) -> env
		(* Expressions *)
		 | TmVar(fi,isconst,name) -> env 
		 | TmConst(fi,const) -> env
		 | TmFunc(fi,params,tm) -> traverse tm (loop (fun name acc -> let varinfo = VariableInfo(fi, name) in add_env_var acc "env" varinfo) params env)
		 | TmCall(fi,tm,tmlist) -> env
	 	 | TmBreak(fi) -> env
		(* Other *)
		 | TmScope(fi,tmlist) -> loop traverse tmlist env
	in 
	let environment = traverse ast (StringMap.add "errors" errors (get_empty_environment ["env";"booleans"])) in 
	StringMap.find "errors" environment


(* Our main function, called from jsh.ml when
	program is ran with argument 'analyze' *)

let analyze ast = 
	let analyze_results = analyze_scope ast [] in
	let loop_results = check_loops ast analyze_results in 
	if (List.length loop_results) > 0 then
		print_errors loop_results "Found errors:"