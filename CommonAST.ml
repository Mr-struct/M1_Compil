type identifier = Id  of string
type label      = Lab of string

module Symb_Tbl = Map.Make(String)
    
type typ =
  | NotFunc
  | TypInt
  | TypBool
  | TypVoid
  | TypArray of typ
  | TypStruct of string
      
type struct_type = {
  fields: (string * typ * bool) list;
}

type function_signature = {
  return: typ;
  formals: (string * typ) list;
}
  
type type_context = {
  identifier_types: typ Symb_Tbl.t;
  struct_types: struct_type Symb_Tbl.t;
  function_signatures : function_signature Symb_Tbl.t;
  return_type : typ
}

type literal =
  | Int  of int
  | Bool of bool

type unaryOp = Minus | Not
    
type binaryOp = Add | Sub | Mult | Div | Mod
                | Eq | Neq | Lt | Le | Gt | Ge
                | And | Or

let change_func_name func_name params =
  let rec aux typ = 
    match typ with
    | TypInt -> "int"
    | TypBool -> "bool"
    | TypArray t -> "array_of_" ^ (aux t)
    | TypStruct name -> "struct_" ^ name
    | TypVoid -> failwith "TypVoid n'est pas un type d'argument"
    | NotFunc -> failwith "NotFunc n'est pas un type d'argument"
  in
  List.fold_left (fun acc arg -> acc ^ "_" ^ (aux arg)) func_name params
