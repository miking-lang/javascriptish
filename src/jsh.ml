(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE
*)


open Utils
open Ustring.Op
open Printf
open Ast
open Msg




(* Different tasks for the program *)
type prog_task = ProgRun | ProgPrint | ProgAnalyze


(* Start of the program *)
let progstart task filename  =
  let fs1 = open_in filename in
  let tablength = 8 in
  begin try
    Lexer.init (us filename) tablength;
    let ast = fs1 |> Ustring.lexing_from_channel |> Parser.main Lexer.main in
    match task with
    | ProgPrint -> uprint_endline (pprint ast)
    | ProgAnalyze -> failwith "TODO"
    | ProgRun -> failwith "TODO"

    with
    | Lexer.Lex_error m ->
        fprintf stderr "%s\n" (Ustring.to_utf8 (Msg.message2str m))
    | Error m ->
        fprintf stderr "%s\n" (Ustring.to_utf8 (Msg.message2str m))
    | Parsing.Parse_error ->
        fprintf stderr "%s\n"
	(Ustring.to_utf8 (Msg.message2str (Lexer.parse_error_message())))
  end;
  close_in fs1



(* Print out main menu *)
let menu() =
  printf "Usage: jsh [print|analyze|run] <file>\n";
  printf "\n"


(* Main function. Checks arguments and reads file names *)
let main =
  (* Check command  *)
  (match Array.to_list Sys.argv |> List.tl with

  (* Read in a program and print the JavaScript code to the standard output *)
  | "print"::name::lst -> progstart ProgPrint name

  (* Analyze the program, without running it  *)
  | "analyze"::name::lst  ->  progstart ProgAnalyze name

  (* Run one program  *)
  | "run"::name::lst  -> progstart ProgRun name

  (* Show the menu *)
  | _ -> menu())
