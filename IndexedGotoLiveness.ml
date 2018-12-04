open CommonAST
module Gto = IndexedGotoAST

type succ_table = int list array
type liveness_info = { live_in: string list array;
                       live_out: string list array }
  
type succ =
    None
  | Some of int
  
let rec find_succ_goto l i =
  match i with
  | n, Sequence(i1, i2) -> find_succ_goto l i1; find_succ_goto l i2
  | n, Label(lbl) -> if lbl = l then Some n
  | n, _ -> None
    
let find_succ current_instr root_instr =
  match current_instr with
  | n, Goto(l) -> [find_succ_goto l root_instr]
  | n, ConditionalGoto(l, e) -> [n+1; find_succ_goto l root_instr]
  (*| n, Sequence(i1, i2) -> [n+1]*)
  | n, _ -> [n+1]
             
let rec mk_succ_table i =
  let rec aux i t =
    let t1 = Array.make 1 [] in  
    match i with
    | n, Sequence(i1, i2) -> 
    | _, i                -> 
  in aux i (Array.make 0 [])
  
let liveness i =
  i
    
