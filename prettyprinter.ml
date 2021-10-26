open ASD


let rec pretty_print_id (i : id) : string = i
                                          
and pretty_print_type (t : type_) : string =
  match t with
  CharType -> "char"
  | StringType -> "string"
  | Int8Type -> "int8"
  | Int16Type -> "int16"
  | Int32Type -> "int32"
  | Int64Type -> "int64"
  | UInt8Type -> "uint8"
  | UInt16Type -> "uint16"
  | UInt32Type -> "uint32"
  | UInt64Type -> "uint64"
  | FloatType -> "float"
  | BoolType -> "bool"
  | ListType t2 -> (pretty_print_type t2) ^ " list"
  | ArrayType t2 -> (pretty_print_type t2) ^ "[]"
  | TupleType l -> (pretty_print_tuple_type l)
  | UnionType name -> "union " ^ name
  | AliasType name -> name
  | StructType name -> "struct " ^ name

and pretty_print_tuple_type (l : type_ list) : string =
  match l with
  | [] -> failwith "Tuple should not be empty"
  | [e] -> pretty_print_type e
  | hd::tl -> (pretty_print_type hd) ^ " * " (pretty_print_tuple_type tl)

and pretty_print_function_prototype (f : function_prototype) : string =
  let FunctionPrototype (t, name, args) = f in
  (pretty_print_type t) ^ " " ^ name ^ "(" ^ (pretty_print_argument_list args) ^ ")"

and pretty_print_argument_list (l : argument list) : string =
  match l with
  | [] -> ""
  | [e] -> (pretty_print_argument e)
  | hd::tl -> (pretty_print_argument hd) ^ ", " ^ (pretty_print_argument_list tl)
  
and pretty_print_void_function_prototype (f : void_function_prototype) : string =
  let VoidFunctionPrototype (name, args) = f in
  "void " ^ name ^ "(" ^ (pretty_print_argument_list args) ^ ")"

and pretty_print_argument (a : argument) : string =
  let Argument (t, name) = a in
  (pretty_print_type t) ^ " " ^ name

and pretty_print_value (v : value) : string =
  match v with
  | True -> "true"
  | False -> "false"
  | Integer i -> (string_of_int i)
  | Float f -> (string_of_float f)
  | Char c -> "'" ^ (String.make 1 c) ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | TupleValue l -> "(" ^ (pretty_print_tuple_value l) ^ ")"
  | BracketValue l -> "[" ^ (pretty_print_bracket_value l) ^ "]"
  | StructValue l -> "{\n" ^ (pretty_print_struct_value l) ^ "}"

and pretty_print_tuple_value (l : expression list) : string =
  match l with
  | [] -> ""
  | [e] -> pretty_print_expression e
  | hd::tl -> (pretty_print_expression e) ^ ", " ^ (pretty_print_tuple_value tl)
            
and pretty_print_bracket_value (l : expression list) : string =
  match l with
  | [] -> ""
  | [e] -> pretty_print_expression e
  | hd::tl -> (pretty_print_expression e) ^ "; " ^ (pretty_print_bracket_value tl)
            
and pretty_print_struct_value (l : (id * expression) list) : string =
  match l with
  | [] -> ""
  | [e] -> (pretty_print_struct_item_value e) ^ "\n"
  | hd::tl -> (pretty_print_struct_item_value hd) ^ ";\n" ^ (pretty_print_struct_value tl)

and pretty_print_struct_item_value ((name, expr) : (id * expression)) : string =
  name ^ " = " ^ (pretty_print_expression expr)

and pretty_print_header_item (h : header_item) : string =
  match h with
  | HeaderTypeDef type_def -> pretty_print_type_definition type_def
  | FunctionDeclaration (fun_def, fun_doc) ->
     (pretty_print_function_declaration fun_def) ^ (pretty_print_function_documentation fun_doc)
  | VoidFunctionDeclaration (void_fun_def, fun_doc) ->
     (pretty_print_void_function_declaration void_fun_def) ^ (pretty_print_function_documentation fun_doc)

and pretty_print_function_documentation (d : function_documentation) : string =
  let FunctionDocumentation s = d in
  "[\"" ^ s ^ "\"]"

and pretty_print_type_definition (t : type_definition) : string =
  match t with
  | UnionDefinition _ -> ""
  | StructDefinition _ -> ""
  | AliasDefinition _ -> ""
