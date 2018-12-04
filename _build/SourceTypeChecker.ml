open CommonAST
open SourceLocalisedAST

exception Type_error of typ * typ * (int * int)
exception Undefined_struct

type binaryOp =
  |None
  |Some of (typ*typ)

let rec find_locked list field =
  match list with
  | [] -> failwith "Champ non existant"
  |  hd::tl -> match hd with
    | name, _, bool -> if  name = field then bool else find_locked tl field
       
      
let rec find_field list field =
  match list with
  | [] -> failwith "Champ non existant"
  | hd::tl ->
     match hd with
     | name, typ, _ -> if name = field then typ else find_field tl field

let rec find_arg params name =
  match params with
  | [] -> NotFunc
  | hd::tl -> if fst hd = name then snd hd
    else find_arg tl name
	 
let rec type_expression context e = match e.expr with
  | Literal (Int _) -> TypInt
  | Literal(Bool _) -> TypBool
  | Location(loc) -> type_location context loc 
  | UnaryOp(o, e) ->
     let type_o = if o = Minus then TypInt else TypBool in
     let type_e = type_expression context e in
     if type_o = type_e then type_o else raise(Type_error(type_o, type_e, e.e_pos))
  | BinaryOp(o, e1, e2) ->
     begin
       let type_o = if o = Eq || o = Neq then None
	 else if o = Add || o = Sub || o = Mult || o = Div || o = Mod then Some(TypInt, TypInt)
	 else if o = Lt || o = Le || o = Gt || o = Ge then Some(TypInt, TypBool) 
	 else Some(TypBool, TypBool) in
       let type_e1 = type_expression context e1 in
       let type_e2 = type_expression context e2 in
       match type_o with
       |None -> if type_e1 = type_e2 then TypBool
	 else raise (Type_error(type_e1, type_e2, e1.e_pos))
       |Some(TypInt, TypInt) -> if type_e1 = TypInt && type_e2 = TypInt then TypInt
	 else raise (Type_error(TypInt, TypBool, e1.e_pos))
       |Some(TypInt, TypBool) -> if type_e1 = TypInt && type_e2 = TypInt then TypBool
	 else raise (Type_error(TypInt, TypBool, e1.e_pos))
       |Some(TypBool, TypBool) -> if type_e1 = TypBool && type_e2 = TypBool then TypBool
	 else raise (Type_error(TypBool, TypInt, e1.e_pos))
       |Some(_, _) -> failwith "Unhandled case"
     end
  | NewArray(e, t) -> let type_e = type_expression context e in
		      if type_e = TypInt then TypArray(t)
		      else raise (Type_error(TypInt, type_e, e.e_pos))
  | FunCall(Id id, l) ->
     let formals = List.rev (List.fold_left (fun acc arg -> (type_expression context arg)::acc) [] l) in
     let new_name = change_func_name id formals in
     let func_sig = Symb_Tbl.find new_name context.function_signatures in
     List.iter2
       (fun expr arg -> let type_e = type_expression context expr in if type_e != snd arg then raise (Type_error(type_e, snd arg, expr.e_pos)) )
       l
       func_sig.formals;
     (Symb_Tbl.find new_name context.function_signatures).return
  | NewRecord(s) ->
     try let _ = Symb_Tbl.find s context.struct_types in
	 TypStruct s
     with Not_found -> failwith "Cette structure n'est pas définie"
			
and type_location context = function
  | Identifier(Id id) ->
     Symb_Tbl.find id context.identifier_types
  | ArrayAccess(e1, e2) -> let type_e1 = type_expression context e1 in
			   let type_e2 = type_expression context e2 in
			   if type_e2 = TypInt then
			     match type_e1 with
			     | TypArray t -> t
			     | _ -> raise(Type_error(TypArray TypInt, type_e1, e1.e_pos))
			   else raise(Type_error(TypInt, type_e2, e1.e_pos))
  | FieldAccess(e, f) -> let type_e = type_expression context e in
			 match type_e with
			 | TypStruct s ->
			    begin
			      try let fields = Symb_Tbl.find s context.struct_types in
				  find_field fields.fields f
			      with Not_found -> failwith "Cette structure n'existe pas"
			    end
			 | _ -> raise (Type_error(TypStruct "error", type_e, e.e_pos))
			       
let rec typecheck_instruction context i = match i.instr with
  | Set(loc, e) ->
     begin
     match loc with
     | FieldAccess(struc ,f) ->
	begin
	  let typ = type_expression context struc in
	  match typ with
	  | TypStruct name ->
	     begin
	       let name_type = Symb_Tbl.find name context.struct_types in
	       let bool = find_locked name_type.fields f in
	       match bool with
	       | true -> failwith "Ce champ est protégé"
	       | false ->
		  let type_e = type_expression context e in
		  let loc_type = type_location context loc in
		  if loc_type = type_e then ()
		  else raise (Type_error(loc_type, type_e, i.i_pos))
	     end
	  | _ -> failwith "Cette structure n'est pas définie"
	end
     |_ -> 
	let loc_type = type_location context loc in
	let type_e = type_expression context e in
	if loc_type = type_e then ()
	else raise (Type_error(loc_type, type_e, i.i_pos))
     end
  | Conditional(e, i1, i2) ->
     if type_expression context e = TypBool then begin typecheck_instruction context i1; typecheck_instruction context i2 end
     else raise (Type_error(TypBool, type_expression context e, i.i_pos))
  | Loop(e, i) ->
     if type_expression context e = TypBool then typecheck_instruction context i
     else raise (Type_error(TypBool, type_expression context e, i.i_pos))
  | LoopFor(s1, c, s2, i) ->
     if type_expression context c = TypBool then begin typecheck_instruction context s1; typecheck_instruction context s2; typecheck_instruction context i; end
     else raise (Type_error(TypBool, type_expression context c, i.i_pos))
  | Sequence(i1, i2) -> typecheck_instruction context i1; typecheck_instruction context i2
  | Return(e) ->
     begin
       match context.return_type with
       | NotFunc -> failwith "Return en dehors d'une fonction"
       | TypVoid -> failwith "Pas de return dans une procedure"
       | t -> if type_expression context e = t then ()
	 else raise (Type_error(type_expression context e, t, i.i_pos))
     end
  | ProCall(Id id, l) ->
     begin
       let formals = List.rev (List.fold_left (fun acc arg -> (type_expression context arg)::acc) [] l) in
       let new_name = change_func_name id formals in
       let func_sig = Symb_Tbl.find new_name context.function_signatures in
       List.iter2
	 (fun expr arg -> let type_e = type_expression context expr in if type_e != snd arg then raise (Type_error(type_e, snd arg, expr.e_pos)) )
	 l
	 func_sig.formals;
     end
  | Break -> ()
  | Continue -> ()
  | Nop -> ()
    
let extract_context p =
  let predefined_signatures =
    let print = { return=TypVoid; formals=[("i", TypInt)]} in
    let print_int = { return=TypVoid; formals=[("i", TypInt)] } in
    let power = { return=TypInt; formals=[("x", TypInt); ("n", TypInt)] } in
    let functions = Symb_Tbl.fold (fun key value acc ->
      Symb_Tbl.add key value.signature acc) p.functions Symb_Tbl.empty in
    Symb_Tbl.add "print_int" print (Symb_Tbl.add "print_int_int" print_int (Symb_Tbl.add "power_int_int" power functions)) in
  { identifier_types = p.globals;
    struct_types = p.structs;
    function_signatures = predefined_signatures;
    return_type = NotFunc;
  }
    
let typecheck_program p =
  let type_context = extract_context p in
  typecheck_instruction type_context p.main;
  Symb_Tbl.iter (fun key value ->
    let new_identifiers = List.fold_left (fun acc arg -> Symb_Tbl.add (fst arg) (snd arg) acc) p.globals value.signature.formals in
    let new_identifiers = Symb_Tbl.fold (fun key value acc -> Symb_Tbl.add key value acc) value.locals new_identifiers in
    typecheck_instruction {type_context with identifier_types = new_identifiers; return_type = value.signature.return} value.code)
    p.functions;
  type_context;
