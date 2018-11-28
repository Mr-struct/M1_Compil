{

  (* Contexte *)
  open Lexing
  open SourceParser

  (* Traitement des chaînes de caractères alphabétiques *)
  let id_or_keyword =
    (* Définition d'une table des mots-clés et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "main", MAIN;
	"var", VAR;
	"integer", INTEGER;
	"boolean", BOOLEAN;
	"void", VOID;
	"if", IF;
	"else", ELSE;
	"while", WHILE;
	"true", CONST_BOOL(true);
	"false", CONST_BOOL(false);
	"for", FOR;
	"continue", CONTINUE;
	"break", BREAK;
	"elif", ELIF;
	"new", NEW;
	"struct", STRUCT;
	"locked", LOCK;
	"return", RETURN;
      ];
    fun s ->
      (* On cherche la chaîne [s] dans la table. Si on trouve un mot-clé alors
         on renvoie le token associé. *)
      try  Hashtbl.find h s
      (* Et sinon on considère qu'il s'agit d'un identifiant. *)
      with Not_found -> begin (*Printf.printf "%s\n" s;*) IDENT(s) end

  let is_operator =
    (* Définition d'une table des operateurs et des lexèmes associés *)
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [	"+", PLUS;
	"-", MINUS;
	"*", STAR;
	"/", DIV;
	"%", MOD;
	"==", EQUAL;
	"!=", NEQ;
	"<", LT;
	"<=", LE;
	">", GT;
	">=", GE;
	"!", NOT;
	"&&", AND;
	"||", OR;
      ] ;
    fun s ->
      (* On cherche l'opérateur [s] dans la table. Si on trouve alors
         on renvoie le token associé. *)
        Hashtbl.find h s
        
}

(* Raccourci : caractères alphabétiques *)
let alpha = ['a'-'z' 'A'-'Z' '_']
(* Raccourci : chiffres *)
let digit = ['0'-'9']
(* Raccourci : opérateurs *)
let operator = '+' | '-' |  '*' | '/' | '%'| "==" | "!=" |  '<' | "<=" | '>' | ">=" | '!' | "&&" | "||"

(* Expressions régulières définissant les lexèmes *)
rule token = parse
  (* Les espaces et tabulations sont ignorés *)
  | ' ' | '\t'
      { token lexbuf }
  (* Reconnaissance des retour à la ligne avec mise à jour de la position du lexbuf *)
  | '\n'
      { new_line lexbuf; token lexbuf }
  (* Quand on reconnait le caractère des commentaires on rentre dans une deuxième règle qui ignore toute la ligne *)
  | "//"
      { comm lexbuf }
  (* Gestion des commentaires longs *)
  | "/*"
      { comm_long lexbuf }
  (* Les chaînes alphabétiques sont traitées par la fonction [id_or_keyword]
     pour être associées à des mots-clés ou des identifiants. *)
  | alpha+
      { id_or_keyword (lexeme lexbuf) }
  (* Gestion des constantes entières *)
  | digit+
      { CONST_INT(int_of_string (lexeme lexbuf)) }
  (* Début et fin de bloc *)
  | "{"
      { BEGIN }
  | "}"
      { END }
  (* Gestion des parenthèses *)
  | "("
      { LP }
  | ")"
      { RP }
  (* Gestion des crochets pour les tableaux *)
  | "["
      { LB }
  | "]"
      { RB }
  (* Gestion du set *)
  | ":="
      { SET }
  (* Gestion du point virgule *)
  | ";"
      { SEMI }
  (* Gestion de la virgule *)
  | ","
      { COMMA }
  (* Gestion du point *)
  | "."
      { DOT }
  (* Gestion  des opérateurs traités par la fonction [is_operator] *)
  | operator
      { is_operator (lexeme lexbuf) }
  (* Fin de fichier *)
  | eof
      { EOF }
  (* Ignorer les définitions de macro *)
  | '#'
      { macro lexbuf }
  (* Caractères non reconnus *)
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf) ^ " at " ^
		     (string_of_int lexbuf.lex_curr_p.pos_lnum) ^ ", " ^
		     (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))) }

and comm = parse
    |'\n'
	{ new_line lexbuf; token lexbuf }
    | _
	{ comm lexbuf }
    | eof
	{ EOF }

and comm_long = parse
    | "*/"
	{ token lexbuf }
    | '\n'
	{ new_line lexbuf; comm_long lexbuf }
    | _
	{ comm_long lexbuf }
    | eof
	{ failwith ("Commentaire non fermé") }

and macro = parse
    | '\n'
	{ new_line lexbuf; token lexbuf }
    | _
	{ macro lexbuf }
