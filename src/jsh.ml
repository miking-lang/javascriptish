(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE

   The main file for the JavaScriptish compiler/interpreter.
*)


open Utils
open Ustring.Op
open Printf
open Ast
open Msg
(* open Analysis *)

(* Different tasks for the program *)
type prog_task = ProgRun | ProgPrint  (* | ProgAnalyze *)


(* Start of the program *)
let progstart task filename  =
  let fs1 = open_in filename in
  let tablength = 8 in
  begin try
    Lexer.init (us filename) tablength;
    let ast = fs1 |> Ustring.lexing_from_channel |> Parser.main Lexer.main in
    match task with
    | ProgPrint -> uprint_endline (pprintext PrnNode ast)
    (* | ProgAnalyze -> analyze ast *)
    | ProgRun ->
        let name = "tempfile.js" in
        Ustring.write_file name (pprintext PrnNode ast);
        Sys.command ("node " ^ name) |> ignore;
        Sys.remove name

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
  printf "Javascriptish. By David Broman\n";
  printf "\nUsage: jsh [run|node|analyze] <jsh file>\n\n";
  printf "       run  = Parse and execute a jsh file.\n";
  printf "       node = Generate Node.js compatible code.\n";
  printf "       analyze = Analyze a jsh file.\n";
  printf "\n"


(* Main function. Checks arguments and reads file names *)
let main =
  (* Check command  *)
  (match Array.to_list Sys.argv |> List.tl with

  (* Read in a program and print the JavaScript code to the standard output *)
  | "node"::name::lst -> progstart ProgPrint name

  (* Analyze the program, without running it  *)
  (* | "analyze"::name::lst  ->  progstart ProgAnalyze name *)

  (* Run one program  *)
  | "run"::name::lst | name::lst  -> progstart ProgRun name

  (* Show the menu *)
  | _ -> menu())
