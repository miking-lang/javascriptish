(*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE
*)


open Ustring.Op
open Printf
type row = int
type col = int
type filename = ustring
type info =
  | Info of filename * row * col * row * col
  | NoInfo



type id =
  | LEX_UNKNOWN_CHAR
  | LEX_COMMENT_NOT_TERMINATED
  | LEX_STRING_NOT_TERMINATED
  | LEX_INVALID_ESCAPE
  | PARSE_ERROR
  | VAR_NOT_IN_SCOPE
  | WRONG_NUMBER_OF_PARAMS
  | UNCAUGHT_RETURN
  | FUNCTION_NOT_CALLED
  | BOOLEAN_INSTEAD_OF_BREAK
  | ERROR of string


type severity =
  | ERROR
  | WARNING

type arguments = ustring list

(** Error and warning messages. Created by the lexer, parser,
    and type checker.  *)
type message = id * severity * info * arguments


exception Error of message

(** [id2str id] returns the identifier string for [id], e.g.,
    "LEX_UNKNOWN_CHAR" *)
let id2str id args =
  match id  with
    | LEX_UNKNOWN_CHAR -> us"Unknown character"
    | LEX_COMMENT_NOT_TERMINATED -> us"Comment is not terminated"
    | LEX_STRING_NOT_TERMINATED -> us"String is not terminated"
    | LEX_INVALID_ESCAPE -> us"Invalid escape characters"
    | PARSE_ERROR -> us"Parse error"
    | VAR_NOT_IN_SCOPE -> if (Ustring.equal (List.nth args 1) (us"")) then (us"I could not find the variable " ^. (List.nth args 0) ^. us" in the current scope. Remember that a variable declared inside a function is not available outside that function.") else (us"I could not find the variable " ^. (List.nth args 0) ^. us" in the current scope, did you mean " ^. (List.nth args 1) ^. us"?")
    | WRONG_NUMBER_OF_PARAMS -> us"You seem to have passed the wrong number of arguments to the function " ^. (List.nth args 0) ^. us". I expected " ^. (List.nth args 1) ^. us", but got " ^. (List.nth args 2)
    | UNCAUGHT_RETURN -> us"The function " ^. (List.nth args 0) ^. us" returns a value, but that value is never used. Perhaps you want to assign that value to a variable?"
    | FUNCTION_NOT_CALLED -> us"You have declared a function called " ^. (List.nth args 0) ^. us", but you never call it. Remember to call your functions. If you do not use your function, consider removing it from your code to make it more readable."
    | BOOLEAN_INSTEAD_OF_BREAK -> us"You are using a boolean variable instead of a break statement. Consider changing this in order to make your code more readable and efficient."
    | ERROR msg -> us msg

(** [severity2str s] returns the severity strings ["ERROR"] or
    ["WARNING"]. *)
let severity2str s =
  match s with
    | ERROR -> us"ERROR"
    | WARNING -> us"WARNING"

let info2str_startline fi =
  match fi with
    | Info(filename,l1,c1,l2,c2) -> l1
    | NoInfo -> assert false

(** [message2str m] returns a string representation of message [m].
    Is message is not intended to be read by humans. *)
let message2str (id,sev,info,args)  =
  match info with
    | Info(filename,l1,c1,l2,c2) ->
	begin
	  us"FILE \"" ^. filename ^. us"\" " ^.
	    (ustring_of_int l1) ^. us":" ^.
	    (ustring_of_int c1) ^. us"-" ^.
	    (ustring_of_int l2) ^. us":" ^.
	    (ustring_of_int c2) ^. us" " ^.
	    (severity2str sev) ^. us": " ^.
	    (id2str id args)
        end
    |  NoInfo -> us"NO INFO: " ^. (id2str id args)

let raise_error fi msg =
  raise (Error (ERROR(msg),ERROR,fi,[]))
