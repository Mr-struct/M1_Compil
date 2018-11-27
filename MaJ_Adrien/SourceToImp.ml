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
  | Src.Print(e) -> Imp.Print(strip_expression type_context e)
  | Src.Set(l, e) -> Imp.Set(strip_location type_context l, strip_expression type_context e)
  | Src.Conditional(e, i1, i2) ->
     Imp.Conditional(strip_expression type_context e, strip_instruction type_context i1, strip_instruction type_context i2)
  | Src.Loop(e, i) -> Imp.Loop(strip_expression type_context e, strip_instruction type_context i)
  | Src.LoopFor(s1, c, s2, i) -> Imp.LoopFor(strip_instruction type_context s1, strip_expression type_context c,
					   strip_instruction type_context s2, strip_instruction type_context i)
  | Src.Break -> Imp.Break
  | Src.Continue -> Imp.Continue
  | Src.Sequence(i1, i2) -> Imp.Sequence(strip_instruction type_context i1, strip_instruction type_context i2)
  | Src.ProCall(id, params) -> Imp.ProCall(id, List.map (strip_expression type_context) params)
  | Src.Return(e) -> Imp.Return(strip_expression type_context e)
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
  | Src.FunCall(id, params) -> Imp.FunCall(id, List.map (strip_expression type_context) params)
       
and strip_location type_context = function
  | Src.Identifier id -> Imp.Identifier id
  | Src.ArrayAccess(e1, e2) -> Imp.BlockAccess(strip_expression type_context e1, strip_expression type_context e2)
  | Src.FieldAccess(e, f) ->
     let typ = type_expression "" type_context e in
     match typ with
     | TypStruct name ->
	begin
	  let name_type = Symb_Tbl.find name type_context.struct_types in
	  let rank = get_rank name_type.fields f in
	  (*Printf.printf "Field = %s, Rank = %d\n" f rank;*)
	  Imp.BlockAccess(strip_expression type_context e, Imp.Literal(Int rank))
	end
     | _ -> failwith "Cette structure n'est pas définie"
	
let strip_program p type_context =
  let main = strip_instruction type_context (Src.(p.main)) in
  let globals = Src.(p.globals) in
  let main_locals = Src.(p.main_locals) in
  let functions = Symb_Tbl.fold (
    fun key value acc ->
      Symb_Tbl.add key Imp.({signature = Src.(value.signature);
			     code = strip_instruction type_context Src.(value.code);
			     locals = Src.(value.locals)}) acc
  )
    Src.(p.functions)
    Symb_Tbl.empty
  in
  Imp.({ main; globals; main_locals; functions;})
    
