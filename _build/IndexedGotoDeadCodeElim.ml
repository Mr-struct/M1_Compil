open CommonAST
open IndexedGotoLiveness
open IndexedGotoAST

let step root_instr =
  let l = liveness root_instr in
  let b = ref false in
  let rec aux i =
    match i with
    | n, Sequence(i1, i2) -> n, Sequence(aux i1, aux i2)
    | n, Set(loc, e) ->
       (* récupérer la variable loc *)
       let var = List.hd (IndexedGotoLiveness.loc_var loc []) in
       (* regarder live_out[n] *)
       let belongs = List.mem var l.live_out.(n) in
       (* si loc n'appartient pas à live_out[n] -> Nop sinon i *)
       if belongs then n, Set(loc, e) else begin b := true; n, Nop end
    | n, _ -> i
  in (!b, aux root_instr)

let rec dead_code_elim i =
  let instr = step i in
  if fst instr then dead_code_elim (snd instr)
  else snd instr

    
let prog p =
  { main = dead_code_elim p.main;
    globals = p.globals;
    functions = Symb_Tbl.fold (
      fun key value acc ->
	Symb_Tbl.add key {signature = value.signature;
			  code = dead_code_elim value.code;
			  locals = value.locals}
	  acc
    )
      p.functions
      Symb_Tbl.empty;
  }
