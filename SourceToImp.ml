module Src = SourceLocalisedAST
module Imp = ImpAST
open CommonAST
open SourceTypeChecker


let get_rank l f =
  let rec aux l acc =
    match l with
    | [] -> raise Not_found
    | hd::tl ->
       match hd with
       | name, _, _ -> if name = f then acc else aux tl (acc+1)
  in aux l 0
            
let rec strip_instruction type_context i = match Src.(i.instr) with
  | Src.Set(l, e) -> Imp.Set(strip_location type_context l, strip_expression type_context e)
  | Src.Conditional(e, i1, i2) ->
     Imp.Conditional(strip_expression type_context e, strip_instruction type_context i1, strip_instruction type_context i2)
  | Src.Loop(e, i) -> Imp.Loop(strip_expression type_context e, strip_instruction type_context i)
  | Src.LoopFor(s1, c, s2, i) -> Imp.LoopFor(strip_instruction type_context s1, strip_expression type_context c,
					   strip_instruction type_context s2, strip_instruction type_context i)
  | Src.Break -> Imp.Break
  | Src.Continue -> Imp.Continue
  | Src.Sequence(i1, i2) -> Imp.Sequence(strip_instruction type_context i1, strip_instruction type_context i2)
  | Src.Return(e) -> Imp.Return(strip_expression type_context e)
  | Src.ProCall(Id id, params) ->
     let params_types = List.rev (List.fold_left (fun acc arg -> (type_expression type_context arg)::acc) [] params) in
     let new_name = change_func_name id params_types in
     Imp.ProCall((Id new_name), List.map (strip_expression type_context) params)
  | Src.Nop -> Imp.Nop

and strip_expression type_context e = match Src.(e.expr) with
  | Src.Literal(l) -> Imp.Literal(l)
  | Src.Location(l) -> Imp.Location(strip_location type_context l)
  | Src.UnaryOp(o, e) -> Imp.UnaryOp(o, strip_expression type_context e)
  | Src.BinaryOp(o, e1, e2)  -> Imp.BinaryOp(o, strip_expression type_context e1, strip_expression type_context e2)
  | Src.NewArray(e, ty) -> Imp.NewBlock(strip_expression type_context e)
  | Src.NewRecord(name) ->
     let name_type = Symb_Tbl.find name type_context.struct_types in
     let size = List.length name_type.fields in
     Imp.NewBlock(Imp.Literal(Int size))
  | Src.FunCall(Id id, params) ->
     begin
       let params_types = List.rev (List.fold_left (fun acc arg -> (type_expression type_context arg)::acc) [] params) in
       let new_name = change_func_name id params_types in
       Imp.FunCall((Id new_name), List.map (strip_expression type_context) params)
     end
and strip_location type_context = function
  | Src.Identifier id -> Imp.Identifier id
  | Src.ArrayAccess(e1, e2) -> Imp.BlockAccess(strip_expression type_context e1, strip_expression type_context e2)
  | Src.FieldAccess(e, f) ->
     let typ = type_expression type_context e in
     match typ with
     | TypStruct name ->
	begin
	  let name_type = Symb_Tbl.find name type_context.struct_types in
	  let rank = get_rank name_type.fields f in
	  Imp.BlockAccess(strip_expression type_context e, Imp.Literal(Int rank))
	end
     | _ -> failwith "Cette structure n'est pas définie"
	
let strip_program p type_context =
  let main = strip_instruction type_context (Src.(p.main)) in
  let globals = Src.(p.globals) in
  let functions = Symb_Tbl.fold (
    fun key value acc ->
      let new_identifiers = List.fold_left (fun acc arg -> Symb_Tbl.add (fst arg) (snd arg) acc) type_context.identifier_types Src.(value.signature.formals) in
      let new_identifiers = Symb_Tbl.fold (fun key value acc -> Symb_Tbl.add key value acc) value.locals new_identifiers in
      Symb_Tbl.add key Imp.({signature = Src.(value.signature);
			     code = strip_instruction {type_context with identifier_types = new_identifiers; return_type = value.signature.return} Src.(value.code);
			     locals = Src.(value.locals)}) acc
  )
    Src.(p.functions)
    Symb_Tbl.empty
  in
  Imp.({ main; globals; functions;})
    
