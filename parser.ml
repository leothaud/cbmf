open Token
open ASD

let rec parse_header : token Stream.t -> cbmf_header = parser
                                                     | [< p = parse_header_item_list >] -> Header p

and parse_header_item_list : token Stream.t -> header_item list = parser
                                                                | [< hd = parse_header_item; 'SEMICOLON _; tl = parse_header_item_list ?? "1">] -> hd::tl
                                                                | [<>] -> []

and parse_header_item : token Stream.t -> header_item = parser
                                                      | [< td = parse_type_definition >] -> HeaderTypeDef td
                                                      | [< p = parse_function_prototype; doc = parse_function_documentation ?? "2">] -> FunctionDeclaration (p, doc)
                                                      | [< p = parse_void_function_prototype; doc = parse_function_documentation ?? "3">] -> VoidFunctionDeclaration (p, doc)

and parse_type_definition : token Stream.t -> type_definition = parser
                                                              | [< t = parse_union_definition >] -> t
                                                              | [< t = parse_struct_definition >] -> t
                                                              | [< t = parse_alias_definition >] -> t

and parse_function_prototype : token Stream.t -> function_prototype = parser
                                                                    | [< t = parse_type; 'IDENT (name, _)?? "4"; 'LEFT_PAREN _?? "5"; args = parse_argument_list?? "6"; 'RIGHT_PAREN _?? "7" >] -> 
                                                                        FunctionPrototype (t, name, args)
                                                                      

and parse_function_documentation : token Stream.t -> function_documentation = parser
                                                                            | [< 'LEFT_BRACKET _; 'STRING_VALUE (s, _)?? "8"; 'RIGHT_BRACKET _?? "9" >] -> FunctionDocumentation s
                                                                            | [<>] -> FunctionDocumentation ""
and parse_void_function_prototype : token Stream.t -> void_function_prototype = parser
                                                                              | [< 'VOID _; 'IDENT (name, _)?? "10"; 'LEFT_PAREN _?? "11"; args = parse_argument_list ?? "12"; 'RIGHT_PAREN _?? "13" >] -> VoidFunctionPrototype (name, args)

and parse_union_definition : token Stream.t -> type_definition = parser
                                                               | [< 'UNION _; 'IDENT (name, _)?? "14"; 'LEFT_CURLY _?? "15"; uil = parse_union_item_list?? "16"; 'RIGHT_CURLY _?? "17" >] -> UnionDefinition (name, uil)

and parse_struct_definition : token Stream.t -> type_definition = parser
                                                                | [< 'STRUCT _; 'IDENT (name, _)?? "18"; 'LEFT_CURLY _?? "19"; sil = parse_struct_item_list?? "20"; ool = parse_operator_overload_list?? "21"; 'RIGHT_CURLY _?? "22" >] -> StructDefinition (name, sil, ool)

and parse_alias_definition : token Stream.t -> type_definition = parser
                                                               | [< 'ALIAS _; 'IDENT (name, _)?? "23"; 'EQUAL _?? "24"; t = parse_type?? "25" >] -> AliasDefinition (name, t)

and parse_union_item_list : token Stream.t -> union_item list = parser
                                                              | [< hd = parse_union_item; 'SEMICOLON _?? "26"; tl = parse_union_item_list ?? "27">] -> hd::tl
                                                              | [<>] -> []

and parse_union_item : token Stream.t -> union_item = parser
                                                    | [< t = parse_type; 'COLON _?? "28"; 'IDENT (name, _)?? "29" >] -> UnionItem (t, name)

and parse_struct_item_list : token Stream.t -> struct_item list = parser
                                                                | [< hd = parse_struct_item; 'SEMICOLON _?? "30"; tl = parse_struct_item_list ?? "31" >] -> hd::tl
                                                                | [<>] -> []

and parse_struct_item : token Stream.t -> struct_item = parser
                                                      | [< t = parse_type; 'IDENT (name,_) ?? "32" >] -> StructItem (t, name)
                                                                                               
and parse_operator_overload_list : token Stream.t -> operator_overload list = parser
                                                                            | [< hd = parse_operator_overload; 'SEMICOLON _ ?? "33"; tl = parse_operator_overload_list ?? "34">] -> hd::tl
                                                                            | [<>] -> []
                                                                        
and parse_operator_overload : token Stream.t -> operator_overload = parser
                                                                            | [< 'OPERATOR _; op = parse_operator ?? "35"; 'EQUAL _?? "36"; 'IDENT (s, _)?? "37" >] -> OperatorOverload (op, s)


and parse_type : token Stream.t -> type_ = parser
                                         | [< t = parse_base_type; f = parse_type_modificator ?? "38">] -> f t
                                                                                                  
and parse_base_type : token Stream.t -> type_ = parser
                                         | [< 'CHAR _ >] -> CharType
                                         | [< 'STRING _ >] -> StringType
                                         | [< 'INT8 _ >] -> Int8Type
                                         | [< 'INT16 _ >] -> Int8Type
                                         | [< 'INT32 _ >] -> Int8Type
                                         | [< 'INT64 _ >] -> Int8Type
                                         | [< 'UINT8 _ >] -> UInt8Type
                                         | [< 'UINT16 _ >] -> UInt8Type
                                         | [< 'UINT32 _ >] -> UInt8Type
                                         | [< 'UINT64 _ >] -> UInt8Type
                                         | [< 'FLOAT _ >] -> FloatType
                                         | [< 'BOOL _ >] -> BoolType
                                         | [< 'STRUCT _; 'IDENT (s,_) >] -> StructType s
                                         | [< 'UNION _; 'IDENT (s,_) >] -> UnionType s
                                         | [< 'IDENT (s,_) >] -> AliasType s
                                         | [< 'LEFT_PAREN _; l = parse_type_tuple?? "39"; 'RIGHT_PAREN _?? "40" >] -> TupleType l

and parse_type_modificator : token Stream.t -> (type_ -> type_) = parser
                                                                | [< 'LIST _ >] -> (fun x -> ListType x)
                                                                | [< 'LEFT_BRACKET _; 'RIGHT_BRACKET _?? "41" >] -> (fun x -> ArrayType x)
                                                                | [<>] -> (fun x -> x)

and parse_type_tuple : token Stream.t -> type_ list = parser
                                                    | [< hd = parse_type; tl = parse_type_tuple_aux?? "42" >] -> hd::tl

and parse_type_tuple_aux : token Stream.t -> type_ list = parser
                                                        | [< 'COMMA _; l = parse_type_tuple?? "43" >] -> l
                                                        | [<>] -> []
                                                                                                      
and parse_operator : token Stream.t -> operator = parser
                                                | [< o = parse_arithmetic_operator >] -> ArithmOp o
                                                | [< o = parse_boolean_operator >] -> BoolOp o
                                                | [< o = parse_comparison_operator >] -> CompOp o
                                                | [< o = parse_bit_by_bit_operator >] -> BbBOp o

                                                                                       
and parse_arithmetic_operator : token Stream.t -> arithmetic_operator = parser
                                                                      | [< 'PLUS _ >] -> PLUS
                                                                      | [< 'MINUS _ >] -> MINUS
                                                                      | [< 'STAR _ >] -> TIMES
                                                                      | [< 'SLASH _ >] -> DIV
                                                                      | [< 'PERCENT _ >] -> MODULO
                                                                      | [< 'DOUBLE_SLASH _ >] -> INT_DIV
                                                                      | [< 'DOUBLE_LESS_THAN _ >] -> LEFT_SHIFT
                                                                      | [< 'DOUBLE_MORE_THAN _ >] -> RIGHT_SHIFT
                                                                      | [< 'CARET _ >] -> CONCAT_STRING
                                                                      | [< 'AT _ >] -> CONCAT_LIST
                                                                      | [< 'DOUBLE_COLON _ >] -> APPEND_LIST

and parse_boolean_operator : token Stream.t -> boolean_operator = parser
                                                                | [< 'DOUBLE_AMPERSAND _ >] -> AND
                                                                | [< 'DOUBLE_VERTICAL_BAR _ >] -> OR

and parse_comparison_operator : token Stream.t -> comparison_operator = parser
                                                                      | [< 'LESS_THAN _ >] -> LESS_THAN
                                                                      | [< 'MORE_THAN _ >] -> MORE_THAN
                                                                      | [< 'EQUAL _ >] -> EQUAL
                                                                      | [< 'LESS_OR_EQUAL _ >] -> LESS_OR_EQUAL
                                                                      | [< 'MORE_OR_EQUAL _ >] -> MORE_OR_EQUAL
                                                                      | [< 'NOT_EQUAL _ >] -> NOT_EQUAL
and parse_bit_by_bit_operator : token Stream.t -> bit_by_bit_operator = parser
                                                                      | [< 'AMPERSAND _ >] -> AND
                                                                      | [< 'VERTICAL_BAR _ >] -> OR
                                                                          
and parse_argument_list : token Stream.t -> (argument list) = parser
                                                           | [< hd = parse_argument;tl = parse_argument_list_aux ?? "44" >] -> hd::tl
                                                           | [<>] -> []

and parse_argument_list_aux : token Stream.t -> (argument list) = parser
                                                                | [< 'COMMA _; l = parse_argument_list ?? "45" >] -> l
                                                                | [<>] -> []
                                                                   
and parse_argument : token Stream.t -> argument = parser
                                                     | [< t = parse_type; 'IDENT (name, _)?? "46" >] -> Argument (t, name)

and parse_program : token Stream.t -> cbmf_program = parser
                                                   | [< i = parse_include_list; f = parse_function_definition_list ?? "47" >] -> Program (i, f)

and parse_include_list : token Stream.t -> include_ list = parser
                                                         | [< hd = parse_include; tl = parse_include_list ?? "48">] -> hd::tl
                                                         | [<>] -> []

and parse_include : token Stream.t -> include_ = parser
                                               | [< 'MORE_THAN _; 'INCLUDE _ ?? "49"; 'STRING_VALUE (s,_) ?? "50" >] -> Include s
                                                                                                      
and parse_function_definition_list : token Stream.t -> function_definition list = parser
                                                                                | [< hd = parse_function_definition; tl = parse_function_definition_list ?? "50" >] -> hd::tl
                                                                                | [<>] -> []

and parse_function_definition : token Stream.t -> function_definition = parser
                                                                      | [< p = parse_non_void_function_definition >] -> p
                                                                      | [< p = parse_void_function_definition >] -> p

and parse_non_void_function_definition : token Stream.t -> function_definition = parser
                                                                               | [< p = parse_function_prototype; 'LEFT_CURLY _ ?? "51"; b = parse_function_body ?? "52"; 'RIGHT_CURLY _?? "53" >] -> FunctionDefinition (p, b)

and parse_void_function_definition : token Stream.t -> function_definition = parser
                                                                           | [< p = parse_void_function_prototype; 'LEFT_CURLY _ ?? "54"; b = parse_function_body ?? "173"; 'RIGHT_CURLY _ ?? "55">] -> VoidFunctionDefinition (p, b)

and parse_function_body : token Stream.t -> function_body = parser
                                                          | [< l = parse_function_item_list >] -> FunctionBody l

and parse_function_item_list : token Stream.t -> function_item list = parser
                                                                    | [< hd = parse_function_item; 'SEMICOLON _ ?? "56"; tl = parse_function_item_list ?? "172" >] -> hd::tl
                                                                    |  [<>] -> []

and parse_function_item : token Stream.t -> function_item = parser
                                                          | [< s = parse_statement >] -> ItemStatement s
                                                          | [< v = parse_variables_declaration >] -> ItemVarDecl v


and parse_variables_declaration : token Stream.t -> variables_declaration = parser
                                                                          | [< t = parse_type; d = parse_declaration_list ?? "172" >] -> VarsDecl (t, d)

and parse_declaration_list : token Stream.t -> declaration list = parser
                                                                | [< hd = parse_declaration; tl = parse_declaration_list_aux ?? "171" >] -> hd::tl
                                                                | [<>] -> []

and parse_declaration_list_aux : token Stream.t -> declaration list = parser
                                                                    | [< 'COMMA _; l = parse_declaration_list ?? "170" >] -> l
                                                                    | [<>] -> []

and parse_declaration : token Stream.t -> declaration = parser
                                                      | [< n = parse_variable_name; f = parse_initialisation ?? "169">] -> f n

and parse_variable_name : token Stream.t -> variable_name = parser
                                                          | [< 'IDENT (name,_); f = parse_variable_name_decoration ?? "168" >] -> f name

and parse_variable_name_decoration : token Stream.t -> (string -> variable_name) = parser
                                                                                 | [< 'LEFT_BRACKET _; e = parse_expression?? "167"; 'RIGHT_BRACKET _ ?? "57">] -> (fun s -> Array (s,e))
                                                                                 | [<>] -> (fun s -> Var s)

and parse_initialisation : token Stream.t -> (variable_name -> declaration) = parser
                                                                            | [< 'EQUAL _; v = parse_expression ?? "166">] -> (fun n -> InitializedDecl (n, v))
                                                                            | [<>] -> (fun n -> NonInitializedDecl n)
                                                                               
and parse_statement : token Stream.t -> statement = parser
                                                  | [< f = parse_ident_started_statement >] -> f
                                                  | [< s = parse_if_statement >] -> s

                                                  | [< s = parse_while_statement >] -> s
                                                  | [< s = parse_do_while_statement >] -> s
                                                  | [< s = parse_repeat_statement >] -> s
                                                  | [< s = parse_for_statement >] -> s
                                                  | [< s = parse_foreach_statement >] -> s
                                                  | [< s = parse_match_statement >] -> s
                                                  | [< s = parse_return_statement >] -> ReturnStmt s

and parse_ident_started_statement : token Stream.t -> statement = parser
                                                                | [< 'IDENT (name, _); f = parse_ident_started_statement_aux >] -> f name

and parse_ident_started_statement_aux : token Stream.t -> (string -> statement) = parser
                                                                                | [< 'LEFT_BRACKET _; index = parse_expression; 'RIGHT_BRACKET _; 'EQUAL _; e = parse_expression >] -> (fun s -> AffectStmt (ArrayValue (s,index) ,e))
                                                                                | [< 'EQUAL _; e = parse_expression >] -> (fun s -> AffectStmt (VarValue s, e))
                                                                                | [< 'LEFT_PAREN _; args = parse_expression_arguments; 'RIGHT_PAREN _ >] -> (fun s -> FunCallStmt (FunCall (s, args)))
                                                                                      
and parse_fun_call : token Stream.t -> function_call = parser
                                                     | [< 'IDENT (name,_); 'LEFT_PAREN _?? "103"; args = parse_expression_arguments ?? "133"; 'RIGHT_PAREN _?? "104" >] -> FunCall (name, args)

                                                 
and parse_if_statement : token Stream.t -> statement = parser
                                                     | [< 'IF _; 'LEFT_PAREN _?? "59"; e = parse_expression ?? "162"; 'RIGHT_PAREN _?? "60"; 'LEFT_CURLY _?? "61"; then_part = parse_statement_list?? "163"; 'RIGHT_CURLY _ ?? "62"; else_part = parse_else?? "164" >] -> IfStmt (e, then_part, else_part)


and parse_statement_list : token Stream.t -> statement list = parser
                                                            | [< hd = parse_statement; 'SEMICOLON _?? "63"; tl = parse_statement_list?? "161" >] -> hd::tl
                                                            | [<>] -> []


and parse_else : token Stream.t -> statement list = parser
                                                  | [< 'ELSE _; 'LEFT_CURLY _?? "64"; l = parse_statement_list?? "160"; 'RIGHT_CURLY _ ?? "65">] -> l
                                                  | [<>] -> []
                                                          
and parse_while_statement : token Stream.t -> statement = parser
                                                        | [< 'WHILE _; 'LEFT_PAREN _?? "66"; c = parse_expression?? "158"; 'RIGHT_PAREN _?? "67"; 'LEFT_CURLY _?? "68"; l = parse_statement_list?? "159"; 'RIGHT_CURLY _ ?? "69">] -> WhileStmt (c, l)
                                                                                                                                                                                      
and parse_do_while_statement : token Stream.t -> statement = parser
| [< 'DO _; 'LEFT_CURLY _?? "70"; l = parse_statement_list?? "156"; 'RIGHT_CURLY _ ?? "71"; 'WHILE _?? "72"; 'LEFT_PAREN _?? "73"; c = parse_expression?? "157"; 'RIGHT_PAREN _ ?? "74" >] -> DoWhileStmt (l, c)
                                                                                                                                                                                                  
and parse_repeat_statement : token Stream.t -> statement = parser
                                                         | [< 'REPEAT _ ; 'LEFT_CURLY _?? "75"; l = parse_statement_list?? "154"; 'RIGHT_CURLY _?? "76"; 'UNTIL _?? "77"; 'LEFT_PAREN _?? "78"; c = parse_expression?? "155"; 'RIGHT_PAREN _ ?? "79">] -> RepeatStmt (l, c)

                                                            
and parse_for_statement : token Stream.t ->  statement = parser
  | [< 'FOR _; 'LEFT_PAREN _?? "80"; vd = parse_variables_declaration_option?? "150"; 'SEMICOLON _?? "81"; c = parse_expression_option?? "151"; 'SEMICOLON _?? "82"; s = parse_statement_option?? "152"; 'RIGHT_PAREN _?? "83"; 'LEFT_CURLY _?? "84"; l = parse_statement_list?? "153"; 'RIGHT_CURLY _?? "85" >] -> ForStmt (vd, c, s, l)

and parse_variables_declaration_option : token Stream.t -> variables_declaration option = parser
                                                                                        | [< vd = parse_variables_declaration >] -> Some vd
                                                                                        | [<>] -> None
                                                                                                
and parse_expression_option : token Stream.t -> expression option = parser
                                                                  | [< e = parse_expression >] -> Some e
                                                                  | [<>] -> None
                                                                          
and parse_statement_option : token Stream.t -> statement option = parser
                                                                | [< s = parse_statement >] -> Some s
                                                                | [<>] -> None
      
and parse_foreach_statement : token Stream.t -> statement = parser
                                                          | [< 'FOREACH _; 'LEFT_PAREN _ ?? "86"; t = parse_type ?? "142"; 'IDENT (name, _) ?? "87"; 'COLON _ ?? "88"; e = parse_expression?? "143"; 'RIGHT_PAREN _ ?? "89"; 'LEFT_CURLY _ ?? "90"; l = parse_statement_list?? "144"; 'RIGHT_CURLY _ ?? "91">] -> ForeachStmt (t, name, e, l)


  
and parse_match_statement : token Stream.t -> statement = parser
                                                        | [< 'MATCH _; 'LEFT_PAREN _?? "92"; e = parse_expression ?? "139"; 'RIGHT_PAREN _?? "93"; 'LEFT_CURLY _?? "94"; l = parse_match_case_list ?? "140"; d = parse_match_default_option ?? "141"; 'RIGHT_CURLY _ ?? "95" >] -> MatchStmt (e, l, d)

and parse_match_case_list : token Stream.t -> match_case list = parser
                                                              | [< hd = parse_match_case; 'SEMICOLON _?? "96"; tl = parse_match_case_list  ?? "138">] -> hd::tl
                                                              | [<>] -> []

and parse_match_case : token Stream.t -> match_case = parser
                                                    | [< 'CASE _; 'LEFT_PAREN _?? "97"; e = parse_expression  ?? "136" ; 'RIGHT_PAREN _?? "98"; 'LEFT_CURLY _?? "99"; l = parse_statement_list  ?? "137"; 'RIGHT_CURLY _?? "100" >] -> MatchCase (e, l)

                                                              
and parse_match_default_option : token Stream.t -> match_default option = parser
                                                                        | [< d = parse_match_default >] -> Some d
                                                                        | [<>] -> None

and parse_match_default : token Stream.t -> match_default = parser
                                                          | [< 'DEFAULT _; 'LEFT_CURLY _ ?? "101"; l = parse_statement_list  ?? "135"; 'RIGHT_CURLY _?? "102" >] -> MatchDefault l

                                                               
and parse_return_statement : token Stream.t -> return_statement = parser
                                                                | [< 'RETURN _; r = parse_return_statement_decoration  ?? "134" >] -> r

and parse_return_statement_decoration : token Stream.t -> return_statement = parser
                                                                           | [< e = parse_expression >] -> ValueReturn e
                                                                             | [<>] -> VoidReturn




and parse_expression_arguments : token Stream.t -> expression list = parser
                                                                  | [< hd = parse_expression; tl = parse_expression_arguments_aux  ?? "132">] -> hd::tl
                                                                    | [<>] -> []

and parse_expression_arguments_aux : token Stream.t -> expression list = parser
                                                                       | [< 'COMMA _; l = parse_expression_arguments  ?? "131">] -> l
                                                                       | [<>] -> []

                                                                            
  
and parse_variable_expression : token Stream.t -> variable_expression = parser
                                                                      | [< 'IDENT (name,_); f = parse_variable_expression_aux  ?? "130">] -> f name

and parse_variable_expression_aux : token Stream.t -> (string -> variable_expression) = parser
                                                                                      | [< 'LEFT_BRACKET _; e = parse_expression  ?? "129"; 'RIGHT_BRACKET _?? "108" >] -> (fun s -> ArrayValue (s, e))
                                                                                      | [<>] -> (fun s -> VarValue s)

and parse_expression : token Stream.t -> expression = parser
                                                    | [< e = parse_expression_aux; f = parse_binary_expression  ?? "128" >] -> f e

and parse_expression_aux : token Stream.t -> expression = parser
                                                        | [< 'LEFT_PAREN _; e = parse_expression_paren  ?? "128" >] -> e
                                                        | [< v = parse_value >] -> ConstExpr v
                                                        | [< v = parse_variable_expression >] -> VarExpr v
                                                        | [< 'EXCLAMATION_MARK _; e = parse_expression  ?? "127">] -> NotExpr e
                                                        | [< f = parse_fun_call >] -> FunCallExpr f

and parse_expression_paren : token Stream.t -> expression = parser
                                                          | [< t = parse_type; 'RIGHT_PAREN _?? "109"; e = parse_expression  ?? "126">] -> CastExpr (t, e)
                                                          | [< e = parse_expression; 'RIGHT_PAREN _?? "110" >] -> ParenExpr e
                                                             


and parse_binary_expression : token Stream.t -> (expression -> expression) = parser
                                                                           | [< op = parse_operator; e = parse_expression  ?? "125" >] -> (fun x -> BinExpr (x,op,e))
                                                                           | [<>] -> (fun x -> x)

and parse_value : token Stream.t -> value = parser
                                          | [< 'TRUE_VALUE _ >] -> True
                                          | [< 'FALSE_VALUE _ >] -> False
                                          | [< 'INTEGER_VALUE (i,_) >] -> Integer i
                                          | [< 'FLOAT_VALUE (f,_) >] -> Float f
                                          | [< 'CHAR_VALUE (c,_) >] -> Char c
                                          | [< 'STRING_VALUE (s,_) >] -> String s
                                          | [< 'LEFT_PAREN _; l = parse_value_tuple  ?? "124"; 'RIGHT_PAREN _ ?? "111">] -> TupleValue l
                                          | [< 'LEFT_BRACKET _; l = parse_value_bracket ?? "1233"; 'RIGHT_BRACKET _ ?? "112">] -> BracketValue l
                                          | [< 'LEFT_CURLY _; l = parse_struct_value ?? "122"; 'RIGHT_CURLY _?? "113">] -> StructValue l



and parse_value_tuple : token Stream.t -> expression list = parser
                                                     | [< hd = parse_expression; tl = parse_value_tuple_aux  ?? "121">] -> hd::tl
                                                     | [<>] -> []

and parse_value_tuple_aux : token Stream.t -> expression list = parser
                                                         | [< 'COMMA _; l = parse_value_tuple ?? "120">] -> l
                                                         | [<>] -> []
                                                                 
and parse_value_bracket : token Stream.t -> expression list = parser
                                                       | [< hd = parse_expression; tl = parse_value_bracket_aux ?? "119">] -> hd::tl
                                                       | [<>] -> []

and parse_value_bracket_aux : token Stream.t -> expression list = parser
                                                           | [< 'SEMICOLON _; l = parse_value_bracket ?? "118">] -> l
                                                           | [<>] -> []
                                                                   
and parse_struct_value : token Stream.t -> (id * expression) list = parser
                                                             | [< hd = parse_struct_value_field; tl = parse_struct_value_aux ?? "117">] -> hd::tl
                                                             | [<>] -> []

and parse_struct_value_aux : token Stream.t -> (id * expression) list = parser
                                                                 | [< 'SEMICOLON _; l = parse_struct_value ?? "116" >] -> l
                                                                 | [<>] -> []

and parse_struct_value_field : token Stream.t -> id * expression = parser
                                                            | [< 'IDENT (name, _); 'EQUAL _ ?? "114"; v = parse_expression ?? "115">] -> (name, v)

and parse_file (s : token Stream.t) : cbmf_file =
  try
    HeaderFile (parse_header s)
  with
    e ->
     try
       ProgramFile (parse_program s)
     with
       e -> NOP
