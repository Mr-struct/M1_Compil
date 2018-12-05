open CommonAST
open IndexedGotoAST

type succ_table = int list array
type liveness_info = { live_in: string list array;
                       live_out: string list array }

let instr_size i =
  let cpt = ref 0 in
  let rec aux i =
    match i with
    | _, Sequence(i1, i2) -> incr cpt; aux i1; aux i2;
    | _, _ -> incr cpt;
  in aux i; !cpt
     
let find_succ_goto l i =
  let succ = ref (-1) in
  let rec find_instr i = 
    match i with
    | n, Sequence(i1, i2) -> find_instr i1; find_instr i2;
    | n, Label(lbl) -> if lbl = l then succ := n else ()
    | n, _ -> ()
  in find_instr i; !succ
  
let find_succ current_instr root_instr size =
  match current_instr with
  | n, Goto(l) -> if n < (size-1) then [find_succ_goto l root_instr] else []
  | n, ConditionalGoto(l, e) -> [n+1; find_succ_goto l root_instr]
  | n, _ -> if n < (size-1) then [n+1] else []
             
let mk_succ_table root_instr =
  let size = instr_size root_instr in
  let tab = Array.make size [] in
  let rec aux i =
    match i with
    | n, Sequence(i1, i2) ->
       begin
	 Array.set tab n (find_succ i root_instr size);
	 aux i1;
	 aux i2;
       end
    | n, instr -> Array.set tab n (find_succ i root_instr size);
  in aux root_instr; tab

let make_gen_kill i =  
  match i with
  (*| n, Sequence(i1, i2)        ->*) 
  | n, Set(loc, e)             -> 
  | n, ConditionalGoto(lbl, e) -> 
  | n, ProCall(id, list)       -> 
  | n, Return(e)               -> 
  (* Label, Goto et Nop *)
  | n, _                       -> 
    
    
let liveness i =
  let size = instr_size i in
  let tab = mk_succ_table i in
  let l = { live_in = Array.make size [];
	    live_out = Array.make size []; } in
  Array.set l.live_in 0 [(* Toutes les variables *)];
  Array.set l.live_out (size-1) [(* Aucune variable *)];
  let rec aux i =
    match i with
    | n, Sequence(i1, i2)      -> 
    | n, Set(loc, e)             -> 
    | n, ConditionalGoto(lbl, e) -> 
    | n, ProCall(id, list)        -> 
    | n, Return(e)             -> 
    (* Label, Goto et Nop *)
    | n, _                  -> 
  in aux i
	 
let prog p =
  let main = Symb_Tbl.find "main_int" p.functions in
  mk_succ_table main.code
