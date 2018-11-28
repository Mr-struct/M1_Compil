module Imp = ImpAST
module Gto = GotoAST
open CommonAST

let (++) = Gto.(++)
  
let new_label =
  let cpt = ref 0 in
  fun () -> incr cpt; CommonAST.Lab (Printf.sprintf "_label_%i" !cpt)

let rec translate_expression = function
  | Imp.Literal(l) -> Gto.Literal(l)
  | Imp.Location(l)-> Gto.Location(strip_location l)
  | Imp.UnaryOp(u,e)-> Gto.UnaryOp(u,translate_expression e)
  | Imp.BinaryOp(b, Literal e1, Literal e2)-> Gto.BinaryOp(b, Gto.Literal(e1), Gto.Literal(e2))
  | Imp.BinaryOp(b, e1, e2) -> Gto.BinaryOp(b,translate_expression e1,translate_expression e2)
  | Imp.NewBlock e -> Gto.NewBlock(translate_expression e)
  | Imp.FunCall(id, params) -> Gto.FunCall(id, List.map translate_expression params)
     
     
and strip_location = function
  | Imp.Identifier id -> Gto.Identifier id
  | Imp.BlockAccess(e1, e2) -> Gto.BlockAccess(translate_expression e1, translate_expression e2)
     
     
let rec translate_instruction_loop i begin_label end_label =
  match i with
  | Imp.Conditional (e, i1, i2) ->
     let then_label = new_label()
     and new_end_label = new_label()
     in
     Gto.ConditionalGoto(then_label, translate_expression e)
     ++ translate_instruction_loop i2 begin_label end_label
     ++ Gto.Goto(new_end_label)
     ++ Gto.Label(then_label) (*Bloc then*)
     ++ translate_instruction_loop i1 begin_label end_label
     ++ Gto.Label(new_end_label)
  | Imp.Sequence(i1, i2) -> Gto.Sequence(translate_instruction_loop i1 begin_label end_label,
					 translate_instruction_loop i2 begin_label end_label) 
  | Imp.Set(l, e) -> Gto.Set(strip_location l, translate_expression e)
  | Imp.Loop(e, new_i) ->
     let new_begin_label = new_label()
     and then_label= new_label()
     and new_end_label= new_label()
     in
     Gto.Label(new_begin_label)
     ++Gto.ConditionalGoto(then_label,translate_expression e)
     ++Gto.Goto(new_end_label)
     ++Gto.Label(then_label)
     ++translate_instruction_loop new_i new_begin_label new_end_label
     ++Gto.Goto(new_begin_label)
     ++Gto.Label(new_end_label)
       
  | Imp.LoopFor(s1, c, s2,  new_i) ->
     let new_begin_label = new_label()
     and then_label= new_label()
     and new_end_label= new_label()
     in
     translate_instruction_loop s1 begin_label end_label
     ++Gto.Label(new_begin_label)
     ++Gto.ConditionalGoto(then_label, translate_expression c)
     ++Gto.Goto(new_end_label)
     ++Gto.Label(then_label)
     ++translate_instruction_loop new_i new_begin_label new_end_label
     ++translate_instruction_loop s2 begin_label end_label
     ++Gto.Goto(new_begin_label)
     ++Gto.Label(new_end_label)

  | Imp.ProCall(id, params) -> Gto.ProCall(id, List.map translate_expression params)
  | Imp.Break -> Gto.Goto(end_label)
  | Imp.Continue -> Gto.Goto(begin_label)
  | Imp.Return(e) -> Gto.Return(translate_expression e)
  | Imp.Nop -> Gto.Nop
     
     
let rec translate_instruction = function
  | Imp.Conditional (e, i1, i2) ->
     let then_label = new_label()
     and end_label = new_label()
     in
     Gto.ConditionalGoto(then_label,translate_expression e)
     ++ translate_instruction i2
     ++ Gto.Goto(end_label) (*Fin bloc else go to end*)
     ++ Gto.Label(then_label) (*Bloc then*)
     ++ translate_instruction i1
     ++ Gto.Label(end_label)

  |Imp.Sequence (i1,i2) -> Gto.Sequence(translate_instruction i1, translate_instruction i2)
  |Imp.Set(l,e) -> Gto.Set(strip_location l,translate_expression e)
  |Imp.Loop(e,i) ->
     let begin_label = new_label()
     and then_label= new_label()
     and end_label= new_label()
     in
     Gto.Label(begin_label)
     ++Gto.ConditionalGoto(then_label,translate_expression e)
     ++Gto.Goto(end_label)
     ++Gto.Label(then_label)
     ++translate_instruction_loop i begin_label end_label
     ++Gto.Goto(begin_label)
     ++Gto.Label(end_label)

  | Imp.LoopFor(s1, c, s2, i) ->
     let begin_label = new_label()
     and then_label= new_label()
     and end_label= new_label()
     in
     translate_instruction s1
     ++Gto.Label(begin_label)
     ++Gto.ConditionalGoto(then_label, translate_expression c)
     ++Gto.Goto(end_label)
     ++Gto.Label(then_label)
     ++translate_instruction_loop i begin_label end_label
     ++translate_instruction s2
     ++Gto.Goto(begin_label)
     ++Gto.Label(end_label)

  | Imp.ProCall(id, params) -> Gto.ProCall(id, List.map translate_expression params)
  | Imp.Nop -> Gto.Nop
  | Imp.Return(e) -> Gto.Return(translate_expression e) 
  | Imp.Break -> failwith "Not in loop"
  | Imp.Continue -> failwith "Not in loop"
     
let translate_program p = Gto.({
  main = translate_instruction Imp.(p.main);
  globals = Imp.(p.globals);
  functions = Symb_Tbl.fold (
    fun key value acc ->
      Symb_Tbl.add key {signature = Imp.(value.signature);
			code = translate_instruction Imp.(value.code);
			locals = Imp.(value.locals)} acc
  )
    Imp.(p.functions)
    Symb_Tbl.empty;
})
