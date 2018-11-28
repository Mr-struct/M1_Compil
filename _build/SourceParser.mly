%{

  (* Contexte *)
  open Lexing
  open CommonAST
  open SourceLocalisedAST

  exception StructError
  
  (* Fonction qui remplit l table des symboles passé en argument à partir d'une liste de variables *)
  (* et d'un type donné *)
  let rec fill_tbl l typ tbl =
    match l with
    | [] -> tbl
    | hd::tl -> fill_tbl tl typ (Symb_Tbl.add hd typ tbl)
       
  let convert_list list_id typ bool =
    let rec aux acc list_id =
    match list_id with
    | [] -> acc
    | hd::tl -> aux ((hd, typ, bool)::acc) tl
    in aux [] list_id
    
  (* Fonction pour afficher le contenu d'une table des symboles *)
  let print_map var typ =
    match typ with
    | TypInt -> print_string(var ^ " TypInt\n")
    | TypBool -> print_string(var ^ " TypBool\n")
    | typArray -> ()

  (* Fonction qui prend une liste de (location*localised_expression) et créé la Sequence d'instructions correspondantes aux affectations *)
  let set_list list l c =
    let rec aux list instr =
      match list with
      | [] -> instr
      | hd::tl -> let i = mk_instr ( Set(fst hd, snd hd) ) l c in
		  let new_instr = mk_instr (Sequence(instr, i)) l c in
		  aux tl new_instr
    in aux list (mk_instr Nop l c)
    
%}

(* Définition des lexèmes *)
%token <int> CONST_INT
%token <bool> CONST_BOOL
%token <string> IDENT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQ LE LT GE GT
%token AND OR NOT
%token LP RP LB RB

%token MAIN
%token VAR NEW
%token INTEGER BOOLEAN VOID STRUCT LOCK

%token IF ELSE ELIF WHILE FOR CONTINUE BREAK
%token SEMI COMMA DOT
%token SET
%token BEGIN END RETURN
%token EOF

(* Définition du symbole initial *)
%start prog
%type <SourceLocalisedAST.program> prog

%left OR
%left AND
%left EQUAL NEQ
%left LE LT GE GT
%left PLUS MINUS
%left STAR DIV MOD
%right NOT UMINUS NEW
%left LB
%left SEMI

%%



  (* IDEE NON FONCTIONNELLE *)
(* Symbole non-terminal principal [prog], version avec gestion des declaration + affectation *)
(*
prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| vars=var_decls; main=main; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { (* Symb_Tbl.iter print_map vars; *)
    let l = $startpos.pos_lnum in
    let c = $startpos.pos_cnum - $startpos.pos_bol in
    let i = set_list (snd vars) l c in
    let new_main = mk_instr (Sequence(i, main)) l c in
    { main = new_main; globals = Symb_Tbl.add "arg" TypInt (fst vars); } }
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;
*)

(* Symbole non-terminal principal [prog], version sans gestion des declaration + affectation *)
  prog:
(* Règles : un programme est formé d'une séquence de déclarations de variables
   suivie du bloc de code principal. *)
| vars_structs=var_structs_decls; fun_decl=fun_decls; EOF
  (* Les déclarations de variables donnent une table des symboles, à laquelle
     est ajoutée la variable spéciale [arg] (avec le type entier). *)
  { (* Symb_Tbl.iter print_map vars; *)
    let l = $startpos.pos_lnum in
    let c = $startpos.pos_cnum - $startpos.pos_bol in
    let expr = mk_expr (Location(Identifier(Id "arg"))) l c in
    { main = mk_instr (ProCall(Id "main", [expr])) l c;
      globals = Symb_Tbl.add "arg" TypInt (fst vars_structs);
      structs = snd vars_structs;
      functions = fun_decl} }  
  
(* Aide : ajout d'une règle pour récupérer grossièrement les erreurs se 
   propageant jusqu'à la racine. *)
| error { let pos = $startpos in
          let message =
            Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;





  (* IDEE NON FONCTIONNELLE *)  
(* Séquence de déclaration de variables avec la fonctionnalité declaration + affectation *)
(*
var_decls:
(* Si pas de déclaration, on renvoie la table vide. *)
| (* empty *)  { (Symb_Tbl.empty, []) }
| VAR; INTEGER; id=ident; SEMI; vds=var_decls
   { let t = fill_tbl (fst id) TypInt (fst vds) in (t, (snd id)@(snd vds) ) }
| VAR; BOOLEAN; id=ident; SEMI; vds=var_decls
   { let t = fill_tbl (fst id) TypBool (fst vds) in (t, (snd id)@(snd vds) ) }
;
*)

(* Séquence de déclaration de variables sans la fonctionnalité declaration + affectation *)
var_structs_decls:
(* Si pas de déclaration, on renvoie la table vide. *)
| (* empty *)  { (Symb_Tbl.empty, Symb_Tbl.empty) }
| VAR; t=typ; id=ident; SEMI; vsd=var_structs_decls
   { (fill_tbl id t (fst vsd), snd vsd) }
| STRUCT; id=IDENT; BEGIN; fd=field_decl; END; vsd=var_structs_decls
   { (fst vsd, Symb_Tbl.add id {fields = fd} (snd vsd)) }
;

field_decl:
| t=typ; id=ident; SEMI
   { convert_list id t false}
| t=typ; id=ident; SEMI; fd=field_decl
   { (convert_list id t false)@fd }
| LOCK; t=typ; id=ident; SEMI
   { convert_list id t true }
| LOCK; t=typ; id=ident; SEMI; fd=field_decl
   { (convert_list id t true)@fd }

;

var_decl:
| (* empty *)  { Symb_Tbl.empty }
| VAR; t=typ; id=ident; SEMI; vsd=var_decl
   { (fill_tbl id t vsd) }
;

fun_decls:
| (* empty *) { Symb_Tbl.empty }
| t=typ; id=IDENT; LP; params=formal_params; RP; BEGIN; vars=var_decl; i=localised_instruction; END; fd = fun_decls
   { Symb_Tbl.add id {signature = { return = t; formals = params }; code=i; locals=vars} fd }
| id=IDENT; LP; params=formal_params; RP; BEGIN; vars=var_decl; i=localised_instruction; END; fd = fun_decls
   { Symb_Tbl.add id {signature = { return = TypVoid; formals = params }; code=i; locals=vars} fd }
| MAIN; LP; params=formal_params; RP; BEGIN; vars=var_decl; i=localised_instruction; END; fd = fun_decls
   { Symb_Tbl.add "main" {signature = { return = TypInt; formals = params }; code=i; locals=vars} fd }
| MAIN; BEGIN; vars=var_decl; i=localised_instruction; END; fd=fun_decls
   { Symb_Tbl.add "main" {signature = { return = TypInt; formals = [("arg", TypInt)]}; code=i; locals=vars} fd }
;

formal_params:
|  { [] }
| t=typ; id=IDENT; COMMA; params=formal_params { (id, t)::params }
| t=typ; id=IDENT { [(id, t)] }
;

typ:
| INTEGER { TypInt }
| BOOLEAN { TypBool }
| VOID { TypVoid }
| t=typ; LB; RB { TypArray t }
| id=IDENT { TypStruct id }
;


(* Règle qui créé la liste des variables pour les déclarations simultanées sans la fonctionnalité declaration + affectation *)

ident:
|id=IDENT; COMMA; l=ident
  { [id]@l }
|id=IDENT
   { [id] }
  ;




(* IDEE NON FONCTIONELLE *)
(* Règle qui créé un (string list * (location * localised_expression) list) pour la déclaration + affectation de variables*)
(* La liste de string correspond à la liste d'identifiant à ajouter dans la table des symboles *)
(* La deuxième liste contient les affectations qui leurs correspondent ( (variable,valeur) ) *)
(*
ident:
| id=IDENT; SET; n=CONST_INT; COMMA; double=ident
   { let list = [id]@(fst double) in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     let list2 = [ (Identifier(Id id), mk_expr (Literal (Int n)) l c) ]@ (snd double) in
     (list, list2)
  }

| id=IDENT; SET; n=CONST_INT
   { let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     ([id], [ (Identifier(Id id), mk_expr (Literal (Int n)) l c)] ) }

| id=IDENT; SET; b=CONST_BOOL; COMMA; double=ident
   { let list = [id]@(fst double) in
     let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in 
     let list2 = [ (Identifier(Id id), mk_expr (Literal (Bool b)) l c) ]@ (snd double) in
     (list, list2)
   }
   
| id=IDENT; SET; b=CONST_BOOL
   { let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     ([id], [ (Identifier(Id id), mk_expr (Literal (Bool b)) l c) ]) }

| id=IDENT; COMMA; double=ident
   { let list = [id]@(fst double) in
     (list, snd double)
   }

| id=IDENT{ ([id], []) }
;
*)


(* IDEE NON FONCTIONELLE *)
(* Première idée pour la gestion des déclarations simultanées, en utilisant des fonctions partielles. *)
(* Avec par exemple var integer i,j; var boolean continuer; i et continuer seront dans la table des symboles finales *)
(* mais pas j. D'après nos tests et tentatives de debug cela devrait fonctionner et on ne comprends pas pourquoi *)
(* j n'est pas dans la table finale. *)
(*
var_decl:
| VAR; INTEGER; vars=ident_int
   { Printf.printf "("; vars }
| VAR; BOOLEAN; vars=ident_bool
   { Printf.printf "("; vars }
;

ident_int:
| id=IDENT; COMMA; ident_int
   { Printf.printf "Symb_Tbl.add %s TypeInt " id;  Symb_Tbl.add id TypInt }
| id=IDENT;
   { Printf.printf "Symb_Tbl.add %s TypeInt " id; Symb_Tbl.add id TypInt }
;

ident_bool:
| id=IDENT; COMMA; ident_bool
   { Printf.printf "Symb_Tbl.add %s TypeBool " id; Symb_Tbl.add id TypBool }
| id=IDENT;
   { Printf.printf "Symb_Tbl.add %s TypeBool " id; Symb_Tbl.add id TypBool }
;
*)



(* MAIN *)


(* Bloc de code principal, formé du mot-clé [main] suivi par le bloc
   proprement dit. *)
(*main:
(* | MAIN; i=block { i } *)
| MAIN; i=block { }
  ;*)

(* Un bloc est une instruction ou séquence d'instructions entre accolades. *)
block:
| BEGIN; i=localised_instruction; END { i }
;

(* Instruction localisée : on mémorise les numéros de ligne et de colonne du
   début de l'instruction.
   Voir dans la doc la définition de [Lexing.position] pour la signification
   de [pos_lnum], [pos_cnum] et [pos_bol]. *)
localised_instruction:
| i=instruction { let l = $startpos.pos_lnum in
                  let c = $startpos.pos_cnum - $startpos.pos_bol in
                  mk_instr i l c }
;

(* Instructions *)
instruction:
(* Si pas d'instruction, on renvoie l'instruction neutre. *)
| (* empty *)  { Nop }
| l=location; SET; e=localised_expression
  { Set(l, e) }
| IF; LP; e=localised_expression; RP; i1=block; ELSE; i2=block
   { Conditional(e, i1, i2) }
| IF; LP; e=localised_expression; RP; i1=block;
   { Conditional(e, i1, mk_instr Nop (fst i1.i_pos) (snd i1.i_pos)) }
| IF; LP; e=localised_expression; RP; i1=block; ELIF; i2=elif_instruction
   { let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     Conditional(e, i1, (mk_instr i2 l c))  }
| WHILE; LP; e=localised_expression; RP; i=block
  { Loop(e, i) }
| FOR; LP; i1=localised_instruction; COMMA; e=localised_expression; COMMA; i2 = localised_instruction; RP; i3=block
  { LoopFor(i1, e, i2, i3) }
| BREAK { Break }
| CONTINUE { Continue }
| i1=localised_instruction; SEMI; i2=localised_instruction
   { Sequence(i1, i2) }
| RETURN; LP; e=localised_expression; RP { Return(e) }
| id=IDENT; LP; a=arguments; RP { ProCall(Id id, a) }
;

(* Règle pour la gestion des blocs elif *)
elif_instruction:
| LP; e=localised_expression; RP; i1=block; i2=elif_instruction
   { let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     Conditional(e, i1, (mk_instr i2 l c))  }
| LP; e=localised_expression; RP; i1=block; ELSE; i2=block
   { Conditional(e, i1, i2) }
;

(* Expression localisée : on mémorise les numéros de ligne et de colonne du
   début de l'expression. *)
localised_expression:
| e=expression
   { let l = $startpos.pos_lnum in
     let c = $startpos.pos_cnum - $startpos.pos_bol in
     mk_expr e l c }

location:
| e=rec_location; DOT; f=IDENT { FieldAccess(e, f) }
| e1=localised_expression; LB; e2=localised_expression; RB { ArrayAccess(e1, e2) }
| id=IDENT { Identifier(Id id) }
;

rec_location:
| id=IDENT { mk_expr (Location(Identifier(Id id))) 0 0 }
| e=rec_location; DOT; f=IDENT
    { mk_expr (Location(FieldAccess(e, f))) 0 0 }
;

(* Expression *)
expression:
| n=CONST_INT { Literal (Int n) }
| b=CONST_BOOL { Literal (Bool b) }
| id=location { Location(id) }
| NEW; t=typ; LB; e=localised_expression; RB { NewArray(e, t) }
| NEW; t=typ { match t with
  | TypStruct name -> NewRecord(name)
  | _ -> raise StructError }
| id=IDENT; LP; a=arguments; RP { FunCall(Id id, a) }
| LP; e=localised_expression; RP { e.expr }
| MINUS; e=localised_expression %prec UMINUS { UnaryOp(Minus, e) }
| NOT; e=localised_expression { UnaryOp(Not, e) }
| e1 = localised_expression; PLUS; e2=localised_expression { BinaryOp(Add, e1, e2) } 
| e1 = localised_expression; MINUS; e2=localised_expression { BinaryOp(Sub, e1, e2) }
| e1 = localised_expression; STAR; e2=localised_expression { BinaryOp(Mult, e1, e2) }
| e1 = localised_expression; DIV; e2=localised_expression { BinaryOp(Div, e1, e2) }
| e1 = localised_expression; MOD; e2=localised_expression { BinaryOp(Mod, e1, e2) }
| e1 = localised_expression; EQUAL; e2=localised_expression { BinaryOp(Eq, e1, e2) }
| e1 = localised_expression; NEQ; e2=localised_expression { BinaryOp(Neq, e1, e2) }
| e1 = localised_expression; LT; e2=localised_expression { BinaryOp(Lt, e1, e2) }
| e1 = localised_expression; LE; e2=localised_expression { BinaryOp(Le, e1, e2) }
| e1 = localised_expression; GT; e2=localised_expression { BinaryOp(Gt, e1, e2) }
| e1 = localised_expression; GE; e2=localised_expression { BinaryOp(Ge, e1, e2) }
| e1 = localised_expression; AND; e2=localised_expression { BinaryOp(And, e1, e2) }
| e1 = localised_expression; OR; e2=localised_expression { BinaryOp(Or, e1, e2) }
;

arguments:
|  { [] }
| e= localised_expression; COMMA; a=arguments { e::a }
| e=localised_expression { [e] }
;			     
