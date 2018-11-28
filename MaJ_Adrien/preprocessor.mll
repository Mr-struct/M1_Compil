{
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
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alpha_digit = ['a'-'z' 'A'-'Z' '0'-'9']

(* Règle principale qui reconnait soit #DEFINE et rentre dans les règles de définition de macro, soit juste # et rentre dans les règles d'utilisation de macro *)
(* Tous les autres caractères sont réimprimés tel quel *)
rule scan c = parse
    | "#DEFINE" as s
	{ print c "%s" s; def_macro_name c lexbuf }
    | "#"
	{ macro_use c lexbuf }
    | _ as char
	{ print c "%c" char; scan c lexbuf }
    | eof
	{ () }

(* Règle qui gère l'utilisation d'une macro : si elle n'a pas d'arguments on imprime juste son texte, sinon on rentre dans les règles pour macro avec arguments *)
and macro_use c = parse
    | alpha_digit+ as macro
	{ if fst (Hashtbl.find macro_text macro) = 0 then
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
	    end }
    
(* Règle qui gère l'ouverture des accolades pour les arguments d'une macro *)
and open_arg c = parse
    | '{'
	{ args_use c lexbuf }
    | _
	{ scan c lexbuf }
    | eof { () }

(* Règle qui gère la lecture des noms des arguments d'une macro, puis ajoute dans la hashtable correspondante le rang de cet argument avec son nom *)
and args_use c = parse
    | alpha_digit+ as arg
	{ Hashtbl.add args_rank (fst (Hashtbl.find macro_text !temp_macro_name)-(!args)) arg; close_arg c lexbuf }

(* Règle qui gère la fermeture des accolades pour les arguments d'une macro *)
(* Si on ferme l'accolade et qu'on a encore des arguments à lire pour cette macro on appele la règle d'ouverture des accolades, sinon on va parser le texte de la macro pour remplacer les #i par leur valeur *)
and close_arg c = parse 
    | '}'
	{ args := !args-1;
	  if !args=0 then
	    begin
	      parse_text c (Lexing.from_string (snd (Hashtbl.find macro_text !temp_macro_name)));
	      scan c lexbuf
	    end
	  else open_arg c lexbuf
	}

(* Règle qui va parser les textes des macros avec des #i et les remplacer par leurs valeurs *)
(* Si on lit un # on rentre dans la règle qui va aller chercher les arguments associés au rang, sinon on imprime tel quel les autres caractères *)
and parse_text c = parse
    | '#'
	{ parse_rank c lexbuf }
    | _ as char
	{ print c "%c" char; parse_text c lexbuf }
    | eof
	{ () }

(* Règle qui reconnait un nombre et va chercher dans la hashtable le nom de l'argument associé, puis l'imprime *)
and parse_rank c = parse
    | digit+ as rank
	{ try print c "%s" (Hashtbl.find args_rank (int_of_string rank)); parse_text c lexbuf
	  with Not_found -> Printf.printf "Macro mal définie : %s\n" rank; }
    | _
	{ Printf.printf "Macro mal définie : rang invalide\n"; }

(* Première version de la règle qui gére la définition du nom d'une macro, pour les macros sans arguments seulement *)
(*	
and macro_use c = parse
    | alpha_digit+ as macro
	{ try print c "%s" (Hashtbl.find h macro); scan c lexbuf
	  with Not_found -> Printf.printf "Macro non définie : %s\n" macro; scan c lexbuf }
*)

(* Règle de définition des noms des macros *)
and def_macro_name c = parse
    | ' '+ | '\t'+ as s
	{ print c "%s" s; def_macro_name c lexbuf }
    | alpha_digit+ as new_macro_name
	{ print c "%s" new_macro_name; temp_macro_name := new_macro_name; def_args c lexbuf}
    | _
	{ Printf.printf "Macro mal définie : première accolade manquante\n"; scan c lexbuf }

(* Règle qui gère l'ouverture des accolades dans la définition du nombre d'arguments d'une macro *)
and def_args c = parse
	| '{' as char
	    { print c "%c" char; arg_number c lexbuf }
	| _ as char
	    { print c "%c" char; def_text c lexbuf }

(* Règle qui gère le nombre d'arguments d'une macro *)
and arg_number c = parse 
    | digit+ as number
	{ print c "%s" number; args := (int_of_string number); close_arg_number c lexbuf } 
    
    | _
	{ Printf.printf "Macro mal définie : nombre d'arguments manquants\n"; scan c lexbuf } 

(* Règle qui gère la fermeture des accolades dans la définition du nombre d'arguments d'une macro *)	
and close_arg_number c = parse
  | '}' as char
      { print c "%c" char; def_text c lexbuf }
  | _
      { Printf.printf "Macro mal définie : deuxième accolade manquante\n"; scan c lexbuf } 

(* Première version de la règle qui gère la définition du nom d'une macro, pour les macros sans arguments seulement *)
	(*
and def_macro_name c = parse
    | ' '+ | '\t'+ as s
	{ print c "%s" s; def_macro_name c lexbuf }
    | alpha_digit+ as new_macro_name
	{ print c "%s" new_macro_name; temp_macro_name := new_macro_name; def_text c lexbuf}
    | _
	{ Printf.printf "Macro mal définie"; scan c lexbuf }
	*)

(* Règle qui gère la définition du texte associé à une macro *)
and def_text c = parse
    | ' '+ | '\t'+ as s
	{ print c "%s" s; def_text c lexbuf }
    | [^'\n']+  as new_text
	{ print c "%s" new_text;
	  Hashtbl.add macro_text !temp_macro_name (!args, new_text); scan c lexbuf}
    | '\n'
	{ Printf.printf "Macro mal définie : texte manquant\n"; scan c lexbuf }
	
	
	(* Première version de la régle qui gère la définition du texte associé à une macro, pour les macros sans arguments seulement *)
	(*
and def_text c = parse
    | ' '+ | '\t'+ as s
	{ print c "%s" s; def_text c lexbuf }
    | [^' ' '\t'] [^'\n']* [^' ' '\t' '\n'] as new_text
	{ print c "%s" new_text;
	(* Printf.printf "%s, %s\n" !temp_macro_name new_text; *)
	  Hashtbl.add h !temp_macro_name new_text; scan c lexbuf}
    | '\n'
	{ Printf.printf "Macro mal définie"; scan c lexbuf }
      *)
	
	{
	  let make f =
	    (* On créé le fichier file.pp.cid dans lequel on va écrire *)
	    let file = (Filename.chop_suffix f ".cid") ^ ".pp.cid" in
	    let c_out = open_out file in
	    scan c_out (Lexing.from_channel (open_in f));
	    close_out c_out;
	    (* On renvoie le fichier créé que compilo.ml va pouvoir utiliser pour le reste de l'exécution *)
	    file
	}

