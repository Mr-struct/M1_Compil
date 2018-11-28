open CommonAST
open GotoAST
open Mips

(* Type du contexte d'allocation d'une fonction, avec les tables des symboles associant les paramètres
   et variables locales à leurs positions *)
type context = {
  params: int Symb_Tbl.t;
  local_vars: int Symb_Tbl.t;
}

(* Fonctions auxiliaires fournissant les pseudo-instructions [push] et [pop]. *)
let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

let build_context_params params =
  let rec aux l acc cpt =
    match l with
    | [] -> acc
    | hd::tl -> let new_acc = Symb_Tbl.add (fst hd) (4*cpt) acc
		in aux tl new_acc (cpt+1)
  in aux params Symb_Tbl.empty 1

let build_context_locals locals =
  let rec aux l acc cpt =
    match l with
    | [] -> acc
    | hd::tl -> let new_acc = Symb_Tbl.add (fst hd) (-4*cpt) acc
		in aux tl new_acc (cpt+1)
  in aux locals Symb_Tbl.empty 2
  
(**
   Fonction de traduction des expressions.
   [translate_expression : GotoAST.expression -> Mips.text]
   Rappel de la convention : le code généré par [translate_expression e] doit
   placer la valeur de l'expression [e] au sommet de la pile.
*)
  
let rec translate_expression context (e: GotoAST.expression) = match e with
  |Literal(l) ->
     begin
       match l with
       |Int i -> 
	  li t0 i
       |Bool b ->
	  if b then
	    li t0 (-1)
	  else
	    li t0 0
     end
  |Location loc ->
     begin
       match loc with
       | Identifier((Id id)) -> 
	  begin
	    try let pos = Symb_Tbl.find id context.local_vars in
		lw t0 pos fp
	    with Not_found ->
	      try let pos = Symb_Tbl.find id context.params in
		  lw t0 pos fp
	      with Not_found ->
		translate_location context loc
		@@ lw t0 0 t0
	  end
       | BlockAccess(e1, e2) ->
	  translate_location context loc
	  @@ lw t0 0 t0
     end
  |UnaryOp(o,e) ->
     begin
       match o with
       | Minus ->
	  translate_expression context e
	  @@ pop t0
	  @@ neg t0 t0
       | Not ->
	  translate_expression context e
	  @@ pop t0
	  @@ not_ t0 t0
     end
  |BinaryOp(o,e1,e2) ->
     begin
       match o with
       | Add ->
	  begin
	    match e1, e2 with
	    |Literal(Int l), e2 ->
	       translate_expression context e2
	       @@ addi t0 t0 l
	    | e1, Literal(Int l) -> 
	       translate_expression context e1
	       @@ addi t0 t0 l
	    |  _, _ ->
	       translate_expression context e1
	       @@ push t0
	       @@ translate_expression context e2
	       @@ pop t1
	       @@ add t0 t0 t1
	  end
       | Sub ->
	  begin
	    match e1, e2 with
	    |Literal(Int l), e2 ->
	       translate_expression context e2
	       @@ neg t0 t0
	       @@ addi t0 t0 l
	    | e1, Literal(Int l) -> 
	       translate_expression context e1
	       @@ subi t0 t0 l
	    |_, _ -> 
	       translate_expression context e1
	       @@ push t0
	       @@ translate_expression context e2
	       @@ pop t1
	       @@ sub t0 t1 t0
	  end
       | Mult ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ mul t0 t0 t1
       | Div ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ div t0 t1 t0
       | Mod ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ rem t0 t1 t0
       | Eq ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ seq t0 t0 t1
       | Neq ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ sne t0 t0 t1
       | Lt ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ slt t0 t1 t0
       | Le ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ sle t0 t1 t0
       | Gt ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ sgt t0 t1 t0
       | Ge ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ sge t0 t1 t0
       | And ->
	  translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ and_ t0 t1 t0
       | Or ->
	 translate_expression context e1
	  @@ push t0
	  @@ translate_expression context e2
	  @@ pop t1
	  @@ or_ t0 t1 t0
     end
  | FunCall(Id f, params) ->
     (List.fold_left (fun code p -> (translate_expression context p) @@ push t0 @@ code) nop params)
     @@ push fp
     @@ push ra
     @@ move fp sp
     @@ addi fp fp 8
     @@ subi sp sp (4* (Symb_Tbl.cardinal context.local_vars)) (* n fois pop zero, n=nombre de variables locales *)
     @@ jal f
     @@ addi sp sp (4* (Symb_Tbl.cardinal context.local_vars)) (* n fois pop zero, n=nombre de variables locales *)
     @@ pop ra
     @@ pop fp
     @@ addi sp sp (4* (List.length params)) (* n fois pop zero *)       
  | NewBlock(e) ->
     translate_expression context e
     @@ addi a0 a0 1
     @@ li t1 4
     @@ mul a0 t0 t1
     @@ li v0 9
     @@ syscall
     @@ sw t0 0 v0
     @@ addi t0 v0 4

and translate_location context = function
  | GotoAST.Identifier(Id l) ->
     begin
       try let pos = Symb_Tbl.find l context.local_vars in
	   move t0 fp
	   @@ addi t0 t0 pos
       with Not_found ->
	 la t0 l
     end
  | GotoAST.BlockAccess(e1,e2) ->
     translate_expression context e1
     @@ push t0
     @@ translate_expression context e2
     @@ pop t1
     @@ lw t2 (-4) t1 (* On stocke dans t2 la taille de notre tableau *)
     @@ sub t2 t0 t2
     @@ bgez t2 "atoi_error" (* Si t0 > t2 error *)
     @@ li t2 4
     @@ mul t0 t0 t2
     @@ add t0 t1 t0
(**
   Fonction de traduction des instructions.
   [translate_instruction : GotoAST.instruction -> Mips.text]
*)
let rec translate_instruction context (i: GotoAST.instruction) = match i with
  | Sequence(i1,i2) ->
     translate_instruction context i1 
     @@ translate_instruction context i2
  | Print(e) ->
     translate_expression context e
     @@ move a0 t0
     @@ li v0 11
     @@ syscall
  | Set(l,e) ->
     translate_expression context e
     @@ push t0
     @@ translate_location context l
     @@ pop t1
     @@ sw t1 0 t0
  | Label(Lab l) ->
     label l
  | Goto(Lab l) ->
     jal l
  | ConditionalGoto(Lab l,e) ->
     translate_expression context e
     @@ bne zero t0 l
  | Return(e) ->
     translate_expression context e
     @@ jr ra
  | Nop -> nop

let translate_function key value acc = 
  let context = {params = build_context_params value.signature.formals;
		 local_vars = build_context_locals (Symb_Tbl.bindings value.locals)} in
  label key
  @@ translate_instruction context value.code
  @@ acc
 

(** 
    Fonction de traduction des programmes
    [translate_program : GotoAST.program -> Mips.program]
    Rien à changer dans cette fonction, elle fournit déjà l'infrastructure dans
    laquelle insérer le code principal.
*)
let translate_program program =
  (* Initialisation : lit le paramètre donné en entrée et enregistre le résultat
     dans les données statiques sous l'étiquette [arg].
     À défaut de paramètre, [arg] vaudra zéro. *)
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
      
  (* Terminaison du programme avec l'appel système [exit] *)
  and close =
    li v0 10
    @@ syscall

  (* Fonctions prédéfinies.
     En l'occurrence, fonction de lecture du paramètre d'entrée. *)
  and built_ins =
    (* Le paramètre est donné sous la forme d'une chaîne de caractères
       terminée par le caractère [000]. *)
    label "atoi"
      
    (* Variables *)
    @@ move t0 a0 (* t0 : adresse du caractère à lire *)
    @@ li   t1 0  (* t1 : accumulateur pour la valeur calculée *)
    (* On garde t2 pour des calculs intermédiaires *)
      
    (* Constantes *)
    @@ li   t3 10 (* Base décimale *)
    @@ li   t4 48 (* Code ASCII caractère '0' *)
    @@ li   t5 57 (* Code ASCII caractère '9' *)

    (* Début de la boucle de lecture *)
    @@ label "atoi_loop"
    @@ lbu  t2 0 t0 (* Lecture d'un octet *)

    (* Conditions d'arrêt et d'erreur *)
    @@ beq  t2 zero "atoi_end" (* Fin si lecture de [000] *)
    @@ blt  t2 t4 "atoi_error" (* Erreur si caractère non compris entre 0 et 9 *)
    @@ bgt  t2 t5 "atoi_error"

    (* Mise à jour de l'accumulateur *)
    @@ addi t2 t2 (-48) (* Conversion caractère en nombre *)
    @@ mul  t1 t1 t3
    @@ add  t1 t1 t2 (* t1 <- 10 * t1 + t2 *)

    (* Suite de la lecture *)
    @@ addi t0 t0 1
    @@ b "atoi_loop"

    (* Arrêt du programme en cas d'erreur de lecture *)
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall

    (* Renvoi du résultat via [v0] en cas de succès *)
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra

    (* Code de la fonction print_int *)  
    @@ comment "print_int"
    @@ label "print_int"
    @@ lw a0 4 sp
    @@ li v0 1
    @@ syscall
    (* @@ sw a0 0 sp *)
    (* @@ subi sp sp 4 *)
    @@ move t0 a0
    @@ jr ra

    (* Code de la fonction power *)
    @@ comment "power"
    @@ label "power"
    @@ lw s0 8 sp
    @@ lw s1 4 sp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    (* @@ sw t0 0 sp *)
    (* @@ subi sp sp 4 *)
    @@ jr ra
  in
  
  (* Construction du texte du programme *)
  let main_code = translate_instruction
    {params=Symb_Tbl.empty;
     local_vars=build_context_locals (Symb_Tbl.bindings program.main_locals)}
    program.main in
  let functions = Symb_Tbl.fold translate_function program.functions nop in
  let text = init @@ main_code @@ close @@ built_ins @@ functions in

  (* Initialisation de la partie des données statiques *)
  let data = Symb_Tbl.fold
    (fun var _ code -> label var @@ dword [0] @@ code)
    program.globals nop
  in

  (* Programme généré *)
{ text; data }
