# 1 "preprocessor.mll"
 
  (* Contexte *)
  (* Définition de la hashtable qui va contenir les noms des macros associés à un double contenant le nombre d'arguments et le texte de cette macro *)
  let macro_text = Hashtbl.create 17

(* Définition de la hashtable qui va contenir les rangs des arguments d'une macro associée à leurs noms pour cette macro *)
let args_rank = Hashtbl.create 17

(* Réference string qui stockera temporairement un nom de macro *)
let temp_macro_name = ref ""

(* Référence int qui stockera le nombre d'arguments d'une macro *)
let args = ref 0
  
(* Fonction pour écrire dans un fichier *)
let print c s = Printf.fprintf c s  

# 20 "preprocessor.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\252\255\253\255\000\000\000\000\000\000\000\000\000\000\
    \002\000\255\255\031\000\106\000\001\000\253\255\254\255\255\255\
    \193\000\012\001\000\000\255\255\028\000\253\255\254\255\255\255\
    \181\000\254\255\000\000\134\001\253\255\209\001\000\000\000\000\
    \074\000\254\255\255\255\096\001\254\255\106\001\073\000\254\255\
    \255\255\029\000\253\255\030\000\032\000\033\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\001\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\001\000\000\000\000\000";
  Lexing.lex_default =
   "\002\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\014\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\000\000\022\000\000\000\000\000\000\000\
    \025\000\000\000\255\255\028\000\000\000\255\255\255\255\255\255\
    \033\000\000\000\000\000\036\000\000\000\255\255\039\000\000\000\
    \000\000\043\000\000\000\043\000\043\000\043\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \031\000\000\000\000\000\003\000\000\000\000\000\044\000\042\000\
    \255\255\044\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\000\000\000\000\000\000\045\000\000\000\023\000\
    \000\000\045\000\000\000\000\000\004\000\005\000\006\000\009\000\
    \000\000\007\000\000\000\000\000\000\000\000\000\008\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\000\000\000\000\015\000\019\000\000\000\000\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\034\000\040\000\000\000\
    \000\000\000\000\000\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\000\000\
    \000\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\013\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\021\000\255\255\255\255\000\000\
    \255\255\255\255\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\000\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\000\000\000\000\031\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\255\255\255\255\000\000\255\255\255\255\041\000\041\000\
    \043\000\044\000\044\000\045\000\255\255\255\255\255\255\255\255\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\255\255\255\255\255\255\041\000\255\255\020\000\
    \255\255\045\000\255\255\255\255\003\000\004\000\005\000\008\000\
    \255\255\006\000\255\255\255\255\255\255\255\255\007\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\255\255\255\255\012\000\018\000\255\255\255\255\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\032\000\038\000\255\255\
    \255\255\255\255\255\255\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\255\255\
    \255\255\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\012\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\020\000\041\000\043\000\255\255\
    \044\000\045\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\255\255\255\255\
    \255\255\038\000\032\000\255\255\255\255\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\255\255\255\255\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\024\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\255\255\255\255\255\255\255\255\255\255\255\255\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec scan c lexbuf =
   __ocaml_lex_scan_rec c lexbuf 0
and __ocaml_lex_scan_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 26 "preprocessor.mll"
                   s
# 249 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 7) in
# 27 "preprocessor.mll"
 ( print c "%s" s; def_macro_name c lexbuf )
# 253 "preprocessor.ml"

  | 1 ->
# 29 "preprocessor.mll"
 ( macro_use c lexbuf )
# 258 "preprocessor.ml"

  | 2 ->
let
# 30 "preprocessor.mll"
           char
# 264 "preprocessor.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 31 "preprocessor.mll"
 ( print c "%c" char; scan c lexbuf )
# 268 "preprocessor.ml"

  | 3 ->
# 33 "preprocessor.mll"
 ( () )
# 273 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_scan_rec c lexbuf __ocaml_lex_state

and macro_use c lexbuf =
   __ocaml_lex_macro_use_rec c lexbuf 10
and __ocaml_lex_macro_use_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 37 "preprocessor.mll"
                      macro
# 286 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 38 "preprocessor.mll"
 ( if fst (Hashtbl.find macro_text macro) = 0 then
	    begin
	      try print c "%s" (snd (Hashtbl.find macro_text macro));
		  scan c lexbuf;
	      with Not_found -> Printf.printf "Macro mal définie : %s\n" macro; 
	    end
	  else
	    begin
	      temp_macro_name := macro;
	      args := fst (Hashtbl.find macro_text macro);
	      open_arg c lexbuf
	    end )
# 301 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_macro_use_rec c lexbuf __ocaml_lex_state

and open_arg c lexbuf =
   __ocaml_lex_open_arg_rec c lexbuf 12
and __ocaml_lex_open_arg_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 54 "preprocessor.mll"
 ( args_use c lexbuf )
# 313 "preprocessor.ml"

  | 1 ->
# 56 "preprocessor.mll"
 ( scan c lexbuf )
# 318 "preprocessor.ml"

  | 2 ->
# 57 "preprocessor.mll"
          ( () )
# 323 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_open_arg_rec c lexbuf __ocaml_lex_state

and args_use c lexbuf =
   __ocaml_lex_args_use_rec c lexbuf 16
and __ocaml_lex_args_use_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 61 "preprocessor.mll"
                      arg
# 336 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 62 "preprocessor.mll"
 ( Hashtbl.add args_rank (fst (Hashtbl.find macro_text !temp_macro_name)-(!args)) arg; close_arg c lexbuf )
# 340 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_args_use_rec c lexbuf __ocaml_lex_state

and close_arg c lexbuf =
   __ocaml_lex_close_arg_rec c lexbuf 18
and __ocaml_lex_close_arg_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 68 "preprocessor.mll"
 ( args := !args-1;
	  if !args=0 then
	    begin
	      parse_text c (Lexing.from_string (snd (Hashtbl.find macro_text !temp_macro_name)));
	      scan c lexbuf
	    end
	  else open_arg c lexbuf
	)
# 359 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_close_arg_rec c lexbuf __ocaml_lex_state

and parse_text c lexbuf =
   __ocaml_lex_parse_text_rec c lexbuf 20
and __ocaml_lex_parse_text_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 81 "preprocessor.mll"
 ( parse_rank c lexbuf )
# 371 "preprocessor.ml"

  | 1 ->
let
# 82 "preprocessor.mll"
           char
# 377 "preprocessor.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 83 "preprocessor.mll"
 ( print c "%c" char; parse_text c lexbuf )
# 381 "preprocessor.ml"

  | 2 ->
# 85 "preprocessor.mll"
 ( () )
# 386 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_parse_text_rec c lexbuf __ocaml_lex_state

and parse_rank c lexbuf =
   __ocaml_lex_parse_rank_rec c lexbuf 24
and __ocaml_lex_parse_rank_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 89 "preprocessor.mll"
                rank
# 399 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 90 "preprocessor.mll"
 ( try print c "%s" (Hashtbl.find args_rank (int_of_string rank)); parse_text c lexbuf
	  with Not_found -> Printf.printf "Macro mal définie : %s\n" rank; )
# 404 "preprocessor.ml"

  | 1 ->
# 93 "preprocessor.mll"
 ( Printf.printf "Macro mal définie : rang invalide\n"; )
# 409 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_parse_rank_rec c lexbuf __ocaml_lex_state

and def_macro_name c lexbuf =
   __ocaml_lex_def_macro_name_rec c lexbuf 27
and __ocaml_lex_def_macro_name_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 105 "preprocessor.mll"
                      s
# 422 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 106 "preprocessor.mll"
 ( print c "%s" s; def_macro_name c lexbuf )
# 426 "preprocessor.ml"

  | 1 ->
let
# 107 "preprocessor.mll"
                      new_macro_name
# 432 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 108 "preprocessor.mll"
 ( print c "%s" new_macro_name; temp_macro_name := new_macro_name; def_args c lexbuf)
# 436 "preprocessor.ml"

  | 2 ->
# 110 "preprocessor.mll"
 ( Printf.printf "Macro mal définie : première accolade manquante\n"; scan c lexbuf )
# 441 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_def_macro_name_rec c lexbuf __ocaml_lex_state

and def_args c lexbuf =
   __ocaml_lex_def_args_rec c lexbuf 32
and __ocaml_lex_def_args_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 114 "preprocessor.mll"
          char
# 454 "preprocessor.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 115 "preprocessor.mll"
     ( print c "%c" char; arg_number c lexbuf )
# 458 "preprocessor.ml"

  | 1 ->
let
# 116 "preprocessor.mll"
        char
# 464 "preprocessor.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 117 "preprocessor.mll"
     ( print c "%c" char; def_text c lexbuf )
# 468 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_def_args_rec c lexbuf __ocaml_lex_state

and arg_number c lexbuf =
   __ocaml_lex_arg_number_rec c lexbuf 35
and __ocaml_lex_arg_number_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 121 "preprocessor.mll"
                number
# 481 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 122 "preprocessor.mll"
 ( print c "%s" number; args := (int_of_string number); close_arg_number c lexbuf )
# 485 "preprocessor.ml"

  | 1 ->
# 125 "preprocessor.mll"
 ( Printf.printf "Macro mal définie : nombre d'arguments manquants\n"; scan c lexbuf )
# 490 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_arg_number_rec c lexbuf __ocaml_lex_state

and close_arg_number c lexbuf =
   __ocaml_lex_close_arg_number_rec c lexbuf 38
and __ocaml_lex_close_arg_number_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 129 "preprocessor.mll"
           char
# 503 "preprocessor.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 130 "preprocessor.mll"
      ( print c "%c" char; def_text c lexbuf )
# 507 "preprocessor.ml"

  | 1 ->
# 132 "preprocessor.mll"
      ( Printf.printf "Macro mal définie : deuxième accolade manquante\n"; scan c lexbuf )
# 512 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_close_arg_number_rec c lexbuf __ocaml_lex_state

and def_text c lexbuf =
   __ocaml_lex_def_text_rec c lexbuf 41
and __ocaml_lex_def_text_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 147 "preprocessor.mll"
                      s
# 525 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 148 "preprocessor.mll"
 ( print c "%s" s; def_text c lexbuf )
# 529 "preprocessor.ml"

  | 1 ->
let
# 149 "preprocessor.mll"
                   new_text
# 535 "preprocessor.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 150 "preprocessor.mll"
 ( print c "%s" new_text;
	  Hashtbl.add macro_text !temp_macro_name (!args, new_text); scan c lexbuf)
# 540 "preprocessor.ml"

  | 2 ->
# 153 "preprocessor.mll"
 ( Printf.printf "Macro mal définie : texte manquant\n"; scan c lexbuf )
# 545 "preprocessor.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_def_text_rec c lexbuf __ocaml_lex_state

;;

# 169 "preprocessor.mll"
  
	  let make f =
	    (* On créé le fichier file.pp.cid dans lequel on va écrire *)
	    let file = (Filename.chop_suffix f ".cid") ^ ".pp.cid" in
	    let c_out = open_out file in
	    scan c_out (Lexing.from_channel (open_in f));
	    close_out c_out;
	    (* On renvoie le fichier créé que compilo.ml va pouvoir utiliser pour le reste de l'exécution *)
	    file
	
# 563 "preprocessor.ml"
