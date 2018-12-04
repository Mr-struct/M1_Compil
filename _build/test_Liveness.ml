let display_list l =
  Printf.printf "[";
  match l with
  | [] -> Printf.printf "]\n";
  | hd::tl -> Printf.printf "%d, " hd
     
let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = SourceParser.prog SourceLexer.token lb in
  close_in c;
  let type_context = SourceTypeChecker.typecheck_program prog in
  let prog = SourceToImp.strip_program prog type_context in
  let prog = ImpToGoto.translate_program prog in
  let indexed_prog = IndexedGotoAST.index_program prog in
  let succ_table = IndexedGotoLiveness.mk_succ_table indexed_prog in
  Array.iter display_list succ_table; 
