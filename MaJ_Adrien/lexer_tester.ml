open SourceParser
open SourceLexer
open Preprocessor

let token_to_string = function
  | BEGIN -> "BEGIN"
  | END -> "END"
  | EOF -> "EOF"
  | IDENT s -> Printf.sprintf "IDENT %s" s
  | INTEGER -> "INTEGER"
  | CONST_INT i -> Printf.sprintf "INT %d" i
  | BOOLEAN -> "BOOLEAN"
  | CONST_BOOL b -> Printf.sprintf "BOOL %b" b
  | LP -> "LP"
  | RP -> "RP"
  | EQUAL -> "EQUAL"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LE -> "LE"
  | GE -> "GE"
  | GT -> "GT"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | VAR -> "VAR"
  | MAIN -> "MAIN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "MULT"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | PRINT -> "PRINT"
  | SEMI -> "SEMI"
  | SET -> "SET"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | CONTINUE -> "CONTINUE"
  | BREAK -> "BREAK"
  | FOR -> "FOR"
  | COMMA -> "COMMA"
  
let main =
  (*let file = Sys.argv.(1) in
  Preprocessor.make file;
    let new_file = (Filename.chop_suffix file ".cid") ^ ".pp.cid" in*)
  let pp_file = Lexing.from_channel (open_in Sys.argv.(1)) in
  let rec aux f =
    let read_token = token f in
    if read_token != EOF then
      begin
	Printf.printf "%s" (token_to_string read_token);
	Printf.printf "\n";
	aux f
      end
    else Printf.printf "EOF\n" in
  aux pp_file
