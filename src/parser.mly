/*
   Javascriptish is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE
*/


%{

  open Ustring.Op
  open Msg
  open Ast
  open Printf

  (** Create a new info, taking left and right part *)
  let mkinfo fi1 fi2 =
    match (fi1,fi2) with
      | (Info(fn,r1,c1,_,_), Info(_,_,_,r2,c2)) -> Info(fn,r1,c1,r2,c2)
      | (Info(fn,r1,c1,r2,c2), NoInfo) -> Info(fn,r1,c1,r2,c2)
      | (NoInfo, Info(fn,r1,c1,r2,c2)) -> Info(fn,r1,c1,r2,c2)
      | (_,_) -> NoInfo
%}

/* Misc tokens */
%token EOF
%token <Ustring.ustring Ast.tokendata> IDENT
%token <Ustring.ustring Ast.tokendata> FUNIDENT
%token <Ustring.ustring Ast.tokendata> STRING
%token <Ustring.ustring Ast.tokendata> CHAR
%token <int Ast.tokendata> UINT

/* Keywords */
%token <unit Ast.tokendata> VAR
%token <unit Ast.tokendata> FUNCTION
%token <unit Ast.tokendata> WHILE
%token <unit Ast.tokendata> IF
%token <unit Ast.tokendata> ELSE
%token <unit Ast.tokendata> RETURN
%token <unit Ast.tokendata> TRUE
%token <unit Ast.tokendata> FALSE

%token <unit Ast.tokendata> EQ            /* "="  */
%token <unit Ast.tokendata> ADD           /* "+"  */
%token <unit Ast.tokendata> SUB           /* "-"  */
%token <unit Ast.tokendata> MUL           /* "*"  */
%token <unit Ast.tokendata> DIV           /* "/"  */
%token <unit Ast.tokendata> MOD           /* "%"  */
%token <unit Ast.tokendata> LESS          /* "<"  */
%token <unit Ast.tokendata> LESSEQUAL     /* "<=" */
%token <unit Ast.tokendata> GREAT         /* ">"  */
%token <unit Ast.tokendata> GREATEQUAL    /* ">=" */
%token <unit Ast.tokendata> EQUAL         /* "==" */
%token <unit Ast.tokendata> NOTEQUAL      /* "!=" */
%token <unit Ast.tokendata> NOT           /* "!"   */
%token <unit Ast.tokendata> OR            /* "||" */
%token <unit Ast.tokendata> AND           /* "&&" */
%token <unit Ast.tokendata> ASSIGN        /* "=" */


/* Symbolic Tokens */
%token <unit Ast.tokendata> LPAREN        /* "("  */
%token <unit Ast.tokendata> RPAREN        /* ")"  */
%token <unit Ast.tokendata> LSQUARE       /* "["  */
%token <unit Ast.tokendata> RSQUARE       /* "]"  */
%token <unit Ast.tokendata> LCURLY        /* "{"  */
%token <unit Ast.tokendata> RCURLY        /* "}"  */
%token <unit Ast.tokendata> CONS          /* "::" */
%token <unit Ast.tokendata> COLON         /* ":"  */
%token <unit Ast.tokendata> COMMA         /* ","  */
%token <unit Ast.tokendata> DOT           /* "."  */
%token <unit Ast.tokendata> BAR           /* "|"  */


%start main

%left OR  /*prec 2*/
%left AND  /*prec 3*/
%left NEG  /*prec 4*/
%left LESS LESSEQUAL GREAT GREATEQUAL EQUAL NOTEQUAL /*prec 6*/
%nonassoc NOT /*prec8 */
%left ADD SUB /*prec 8*/
%left MUL DIV MOD /*prec 9*/
%nonassoc UNARYMINUS /*prec 12*/



%type <Ast.tm> main

%%

main:
 | seq
    { TmScope(NoInfo,$1) }

seq:
 |  {[]}
 |  stmt seq
      {$1::$2}

stmt:
 | VAR IDENT ASSIGN expr
     { let fi = mkinfo $1.i $3.i in
       TmDef(fi,false,$2.v,$4) }
 | IDENT ASSIGN expr
     { let fi = mkinfo $1.i $2.i in
       TmAssign(fi,$1.v,$3) }
 | WHILE LPAREN expr RPAREN LCURLY seq RCURLY
     { let fi1 = mkinfo $1.i $4.i in
       let fi2 = mkinfo $5.i $7.i in
       TmWhile(fi1,$3,TmScope(fi2,$6)) }
 | IF LPAREN expr RPAREN LCURLY seq RCURLY
     { let fi1 = mkinfo $1.i $4.i in
       let fi2 = mkinfo $5.i $7.i in
       TmIf(fi1,$3,TmScope(fi2,$6),None) }
 | IF LPAREN expr RPAREN LCURLY seq RCURLY ELSE LCURLY seq RCURLY
     { let fi1 = mkinfo $1.i $4.i in
       let fi2 = mkinfo $5.i $7.i in
       let fi3 = mkinfo $9.i $11.i in
       TmIf(fi1,$3,TmScope(fi2,$6),Some(TmScope(fi3,$10))) }
 | IDENT LPAREN args RPAREN
     { let fi = mkinfo $1.i $4.i in
       TmCall(fi,TmVar($1.i,true,$1.v),$3) }
 | FUNCTION IDENT LPAREN params RPAREN LCURLY seq RCURLY
     { let fi1 = mkinfo $1.i $8.i in
       let fi2 = mkinfo $5.i $8.i in
       TmDef(fi1,true,$2.v,TmFunc(fi1,$4,TmScope(fi2,$7))) }
 | RETURN expr
     { let fi = mkinfo $1.i (tm_info $2) in
       TmRet(fi,$2) }


expr:
 | atom
     { $1 }
 | expr ADD expr
     { TmCall($2.i,TmConst($2.i,CAdd),[$1;$3]) }

 | expr SUB expr
      { TmCall($2.i,TmConst($2.i,CSub),[$1;$3]) }
 | expr MUL expr
     { TmCall($2.i,TmConst($2.i,CMul),[$1;$3]) }
 | expr DIV expr
     { TmCall($2.i,TmConst($2.i,CDiv),[$1;$3]) }
 | expr LESS expr
     { TmCall($2.i,TmConst($2.i,CLess),[$1;$3]) }
 | expr LESSEQUAL expr
     { TmCall($2.i,TmConst($2.i,CLessEq),[$1;$3]) }
 | expr GREAT expr
     { TmCall($2.i,TmConst($2.i,CGreat),[$1;$3]) }
 | expr GREATEQUAL expr
     { TmCall($2.i,TmConst($2.i,CGreatEq),[$1;$3]) }
 | expr EQUAL expr
     { TmCall($2.i,TmConst($2.i,CEq),[$1;$3]) }
 | expr NOTEQUAL expr
     { TmCall($2.i,TmConst($2.i,CNotEq),[$1;$3]) }
 | NOT expr
     { TmCall($1.i,TmConst($1.i,CNot),[$2]) }
 | expr OR expr
     { TmCall($2.i,TmConst($2.i,COrL),[$1;$3]) }
 | expr AND expr
     { TmCall($2.i,TmConst($2.i,CAndL),[$1;$3]) }
 | SUB expr  %prec UNARYMINUS
     { let fi = mkinfo $1.i (tm_info $2) in
       TmCall(fi,TmConst(fi,CNeg),[$2]) }
 | IDENT LPAREN args RPAREN
     { let fi = mkinfo $1.i $4.i in
       TmCall(fi,TmVar($1.i,true,$1.v),$3) }

params:
 |   {[]}
 | params_rev
     { List.rev $1 }

params_rev:
 | IDENT
     {[$1.v]}
 | params_rev COMMA IDENT
     {$3.v::$1}

args:
 |   {[]}
 | args_rev
     { List.rev $1 }

args_rev:
 | expr
     {[$1]}
 | args_rev COMMA expr
     {$3::$1}


atom:
 | TRUE
     { TmConst($1.i,CTrue) }
 | FALSE
     { TmConst($1.i,CFalse) }
 | UINT
     { TmConst($1.i,CInt($1.v)) }
 | IDENT
     { TmVar($1.i,false,$1.v) }
 | LPAREN expr RPAREN
     { $2 }
