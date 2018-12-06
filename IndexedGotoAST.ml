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


let rec strip_indexed_instruction = function
  | _, Sequence(i1, i2)      -> Gto.Sequence(strip_indexed_instruction i1, strip_indexed_instruction i2)
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
  and create_instruction i =
    match i with
    | Gto.Sequence(i1, i2)      -> Sequence(index_instruction i1, index_instruction i2)
    | Gto.Set(l, e)             -> Set(l, e)
    | Gto.Label(l)              -> Label(l)
    | Gto.Goto(l)               -> Goto(l)
    | Gto.ConditionalGoto(l, e) -> ConditionalGoto(l, e)
    | Gto.ProCall(id, l)        -> ProCall(id, l)
    | Gto.Return(e)             -> Return(e)
    | Gto.Nop                   -> Nop
  in index_instruction i

(* Fonction qui permet d'afficher une instruction avec sa numérotation *)
let rec print_instr i =
  match i with
  | n, Sequence(i1, i2)      ->
     begin
       Printf.printf "Sequence %d\n" n;
       print_instr i1; print_instr i2
     end
  | n, Set(l, e)             -> Printf.printf "Set %d\n" n
  | n, Label(l)              -> Printf.printf "Label %d\n" n
  | n, Goto(l)               -> Printf.printf "Goto %d\n" n
  | n, ConditionalGoto(l, e) -> Printf.printf "CondGoto %d\n" n
  | n, ProCall(id, l)        -> Printf.printf "ProCall %d\n" n
  | n, Return(e)             -> Printf.printf "Return %d\n" n
  | n, Nop                   -> Printf.printf "Nop %d\n" n

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
  { Gto.main = strip_indexed_instruction p.main;
    Gto.globals = p.globals;
    Gto.functions = Symb_Tbl.fold (
      fun key value acc ->
	Symb_Tbl.add key {Gto.signature = value.signature;
			  Gto.code = strip_indexed_instruction value.code;
			  Gto.locals = value.locals}
	  acc
    )
      p.functions
      Symb_Tbl.empty;
  }
