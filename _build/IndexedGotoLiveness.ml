open CommonAST
module Gto = IndexedGotoAST

type succ_table = int list array
type liveness_info = { live_in: string list array;
                       live_out: string list array }
  

let create_succ i =
  let rec aux i =
    match i with
    | n, Sequence(i1, i2) -> [n]@(aux i1)@(aux i2)
    | n, _ -> [n]
  in aux i
       
let rec mk_succ_table i =
  let rec aux i t =
    let t1 = Array.make 1 [] in  
    match i with
    | n, Sequence(i1, i2) -> let newtab = Array.set t1 0 (create_succ i1) in
			     let temp_tab = Array.append t newtab in
			     let newtab = Array.set t1 0 (create_succ i2) in
			     let temp_tab = Array.append temp_tab newtab in
			     let tab = Array.append (aux t1) temp_tab in
			     let tab = Array.append (aux t2) tab
    | _, i                -> let newtab = Array.set t1 0 (create_succ i) in Array.append t newtab
  in aux i (Array.make 0 [])
  
let liveness i =
  i
    
