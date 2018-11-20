
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | STRUCT
  | STAR
  | SET
  | SEMI
  | RP
  | RETURN
  | RB
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEW
  | NEQ
  | MOD
  | MINUS
  | MAIN
  | LT
  | LP
  | LOCK
  | LE
  | LB
  | INTEGER
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FOR
  | EQUAL
  | EOF
  | END
  | ELSE
  | ELIF
  | DOT
  | DIV
  | CONTINUE
  | CONST_INT of (int)
  | CONST_BOOL of (bool)
  | COMMA
  | BREAK
  | BOOLEAN
  | BEGIN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceLocalisedAST.program)
