open Format

let usage = "usage: ./compilo [options] file.cid"

let display_list l =
  Printf.printf "[";
  match l with
  | [] -> Printf.printf "]\n";
  | hd::tl -> Printf.printf "%d, " hd
  
let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".cid") then
        raise (Arg.Bad "no .cid extension");
      file := Some s
    in
    Arg.parse [("-pp", Arg.String (fun s -> file := Some (Preprocessor.make s)), "Preprocessing function for macro")] set_file usage;
    match !file with Some f -> f | None -> Arg.usage [] usage; exit 1

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = SourceParser.prog SourceLexer.token lb in
  close_in c;
  let type_context = SourceTypeChecker.typecheck_program prog in
  let prog = SourceToImp.strip_program prog type_context in
  let prog = ImpToGoto.translate_program prog in
  let indexed_prog = IndexedGotoAST.index_program prog in
  Symb_Tbl.iter (
    fun key value acc ->
      let succ_table = IndexedGotoLiveness.mk_succ_table value.code in
      Array.iter display_list succ_table;
  )
    indexed_prog.functions;
  let asm = GotoToMips.translate_program prog in
  let output_file = (Filename.chop_suffix file ".cid") ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
