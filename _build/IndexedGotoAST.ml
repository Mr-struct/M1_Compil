open CommonAST
module Gto = GotoAST

type expression = Gto.expression
type location = Gto.location
  
type instruction = int * instr_descr
and instr_descr =
  | Sequence        of instruction * instruction
  | Set             of location * expression
  | Label           of label
  | Goto            of label
  | ConditionalGoto of label * expression
  | ProCall         of identifier * expression list
  | Return of expression
  | Nop
  
type function_info = {
  signature: function_signature;
  code: instruction;
  locals: typ Symb_Tbl.t;
}
  
type program = {
  main: instruction;
  globals: typ Symb_Tbl.t;
  functions: function_info Symb_Tbl.t
}

(* let rec strip_expression_to_Goto = function
  | IGto.Literal(l) -> Gto.Literal(l)
  | IGto.Location(l) -> Gto.Location(strip_location_to_Goto l)
  | IGto.UnaryOp(u, e) -> Gto.UnaryOp(u, strip_expression_to_Goto e)
  | IGto.BinaryOp(b, e1, e2) -> Gto.BinaryOp(b, strip_expression_to_Goto e1, strip_expression_to_Goto e2)
  | IGto.NewBlock(e) -> Gto.NewBlock(strip_expression_to_Goto e)
  | IGto.FunCall(id, l) -> Gto.FunCall(id, List.map strip_expression_to_Goto l)
     
and let strip_location_to_Goto = function
  | IGto.Identifier(id) -> Gto.Identifier(id)
  | IGto.BlockAccess(e1, e2) -> Gto.BlockAccess(strip_expression_to_Goto e1, strip_expression_to_Goto e2)
*)

let rec strip_indexed_instruction = function
  | _, Sequence(i1, i2)      -> Gto.Sequence(strip_instruction i1, strip_instruction i2)
  | _, Set(l, e)             -> Gto.Set(l, e)
  | _, Label(l)              -> Gto.Label(l)
  | _, Goto(l)               -> Gto.Goto(l)
  | _, ConditionalGoto(l, e) -> Gto.ConditionalGoto(l, e)
  | _, ProCall(id, l)        -> Gto.ProCall(id, l)
  | _, Return(e)             -> Gto.Return(e)
  | _, Nop                   -> Gto.Nop

let strip_instruction i =  
  let cpt = ref (-1) in
  let rec index_instruction i = (incr cpt; !cpt), create_instruction i
  and create_instruction = function
    | Gto.Sequence(i1, i2)      -> Sequence(index_instruction i1, index_instruction i2)
    | Gto.Set(l, e)             -> Set(l, e)
    | Gto.Label(l)              -> Label(l)
    | Gto.Goto(l)               -> Goto(l)
    | Gto.ConditionalGoto(l, e) -> ConditionalGoto(l, e)
    | Gto.ProCall(id, l)        -> ProCall(id, l)
    | Gto.Return(e)             -> Return(e)
    | Gto.Nop                   -> Nop
  in index_instruction i
       
let index_program p =
  { main = strip_instruction Gto.(p.main);
    globals = Gto.(p.globals);
    functions = Symb_Tbl.fold (
      fun key value acc ->
	Symb_Tbl.add key {signature = Gto.(value.signature);
			  code = strip_instruction Gto.(value.code);
			  locals = Gto.(value.locals)}
	  acc
    )
      Gto.(p.functions)
      Symb_Tbl.empty;
  }
    
let strip_program p =
  Gto.({ main = strip_indexed_instruction IGto.(p.main);
    globals = IGto.(p.globals);
    functions = Symb_Tbl.fold (
      fun key value acc ->
	Symb_Tbl.add key {signature = IGto.(value.signature);
			  code = strip_indexed_instruction IGto.(value.code);
			  locals = IGto.(value.locals)}
	  acc
    )
      IGto.(p.functions)
      Symb_Tbl.empty;
  })
