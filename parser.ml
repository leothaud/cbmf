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
                                                                    | [< t = parse_type; 'IDENT (name, _)?? "4"; 'LEFT_PAREN _?? "5"; args = parse_argument_list?? "6"; 'RIGHT_PAREN _?? "7" >] -> begin
                                                                        Printf.printf "%s\n" name;
                                                                        FunctionPrototype (t, name, args)
                                                                      end

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
                                                   | [< i = parse_include_list; f = parse_funcion_definition_list >] -> Program (i, f)

and parse_include_list : token Stream.t -> include_ list = parser
                                                         | [< hd = parse_include; tl = parse_include_list >] -> hd::tl
                                                         | [<>] -> []

and parse_include : token Stream.t -> include_ = parser
                                               | [< 'MORE_THAN _; 'INCLUDE _; 'STRING_VALUE (s,_) >] -> Include s
                                                                                                      
and parse_function_definition_list : token Stream.t -> function_definition list = parser
                                                                                | [< hd = parse_function_definition; tl = parse_function_definition_list >] -> hd::tl
                                                                                | [<>] -> []

and parse_function_definition : token Stream.t -> function_definition = parser
                                                                      | [< p = parse_non_void_function_definition >] -> p
                                                                      | [< p = parse_void_function_definition >] -> p

and parse_non_void_function_definition : token Stream.t -> function_definition = parser
                                                                               | [< p = parse_function_prototype; 'LEFT_CURLY _; b = parse_function_body; 'RIGHT_CURLY _ >] -> FunctionDefinition (p, b)

and parse_void_function_definition : token Stream.t -> function_definition = parser
                                                                           | [< p = parse_void_function_prototype; 'LEFT_CURLY _; b = parse_function_body; 'RIGHT_CURLY _ >] -> VoidFunctionDefinition (p, b)

and parse_function_body : token Stream.t -> function_body = parser
                                                          | [< l = parse_function_item_list >] -> FunctionBody l

and parse_function_item_list : token Stream.t -> function_item list = parser
                                                                    | [< hd = parse_function_item; 'SEMICOLON _; tl = parse_function_item_list >] -> hd::tl
                                                                    |  [<>] -> []

and parse_function_item : token Stream.t -> function_item = parser
                                                          | [< s = parse_statement >] -> ItemStatement s
                                                          | [< v = parse_variables_declaration >] -> ItemVarDecl v


and parse_variables_declaration : token Stream.t -> variables_declaration = parser
                                                                          | [< t = parse_type; d = parse_declaration_list >] -> VarsDecl (t, d)

and parse_declaration_list : token Stream.t -> declaration list = parser
                                                                | [< hd = parse_declaration; tl = parse_declaration_list_aux >] -> hd::tl
                                                                | [<>] -> []

and parse_declaration_list_aux : token Stream.t -> declaration list = parser
                                                                    | [< 'COMMA _; l = parse_declaration_list >] -> l
                                                                    | [<>] -> []

and parse_declaration : token Stream.t -> declaration = parser
                                                      | [< n = parse_variable_name; f = parse_initialisation >] -> f n

and parse_variable_name : token Stream.t -> variable_name = parser
                                                          | [< 'IDENT (name,_); f = parse_variable_name_decoration >] -> f name

and parse_variable_name_decoration : token Stream.t -> (string -> variable_name) = parser
                                                                                 | [< 'LEFT_BRACKET _; e = parse_expression; 'RIGHT_BRACKET _ >] -> (fun s -> Array (s,e))
                                                                                 | [<>] -> (fun s -> Var s)

and parse_initialisation : token Stream.t -> (variable_name -> declaration) = parser
                                                                            | [< 'EQUAL _; v = parse_value >] -> (fun n -> InitializedDecl (n, v))
                                                                            | [<>] -> (fun n -> NonInitializedDecl n)
                                                                               
and parse_statement : token Stream.t -> statement = parser
                                                  | [< s = parse_affect_statement >] -> s
                                                  | [< s = parse_if_statement >] -> s

                                                  | [< s = parse_while_statement >] -> s
                                                  | [< s = parse_do_while_statement >] -> s
                                                  | [< s = parse_repeat_statement >] -> s
                                                  | [< s = parse_for_statement >] -> s
                                                  | [< s = parse_foreach_statement >] -> s
                                                  | [< s = parse_match_statement >] -> s
                                                  | [< s = parse_return_statement >] -> ReturnStmt s
                                                  | [< f = parse_fun_call >] -> FunCallStmt f

and parse_affect_statement : token Stream.t -> statement = parser
                                                         | [< v = parse_variable_expression; 'EQUAL _; e = parse_expression >] -> AffectStmt (v,e)
                                                 
and parse_if_statement : token Stream.t -> statement = parser
                                                     | [< 'IF _; 'LEFT_PAREN _; e = parse_expression; 'RIGHT_PAREN _; 'LEFT_CURLY _; then_part = parse_statement_list; 'RIGHT_CURLY _; else_part = parse_else >] -> IfStmt (e, then_part, else_part)


and parse_statement_list : token Stream.t -> statement list = parser
                                                            | [< hd = parse_statement; 'SEMICOLON _; tl = parse_statement_list >] -> hd::tl
                                                            | [<>] -> []


and parse_else : token Stream.t -> statement list = parser
                                                  | [< 'ELSE _; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY _ >] -> l
                                                  | [<>] -> []
                                                          
and parse_while_statement : token Stream.t -> statement = parser
                                                        | [< 'WHILE _; 'LEFT_PAREN _; c = parse_expression; 'RIGHT_PAREN _; 'LEFT_CURLY; l = parse_statement_list; 'RIGHT_CURLY _ >] -> WhileStmt (c, l)
                                                                                                                                                                                      
and parse_do_while_statement : token Stream.t -> statement = parser
                                                           | [< 'DO _; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY _; 'WHILE _; 'LEFT_PAREN _; c = parse_expression; 'RIGHT_PAREN _ >] -> DoWhileStmt (l, c)
                                                                                                                                                                                                  
and parse_repeat_statement : token Stream.t -> statement = parser
                                                         | [< 'REPEAT _ ; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY; 'UNTIL _; 'LEFT_PAREN _; c = parse_expression; 'RIGHT_PAREN _ >] -> RepeatStmt (l, c)

                                                            
and parse_for_statement
  | [< 'FOR _; 'LEFT_PAREN; vd = parse_variables_declaration_option; 'SEMICOLON _; c = parse_expression_option; 'SEMICOLON _; s = parse_statement_option; 'RIGHT_PAREN _; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY _ >] -> ForStmt (vd, c, s, l)

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
                                                          | [< 'FOREACH _; 'LEFT_PAREN _; t = parse_type; 'IDENT (name, _); 'COLON _; e = parse_expression; 'RIGHT_PAREN _; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY _ >] -> ForeachStmt (t, name, e, l)


  
and parse_match_statement : token Stream.t -> statement = parser
                                                        | [< 'MATCH _; 'LEFT_PAREN _; e = parse_expression; 'RIGHT_PAREN _; 'LEFT_CURLY _; l = parse_match_case_list; d = parse_match_default_option; 'RIGHT_CURLY _ >] -> MatchStmt (e, l, d)

and parse_match_case_list : token Stream.t -> match_case list = parser
                                                              | [< hd = parse_match_case; 'SEMICOLON _; tl = parse_match_case_list >] -> hd::tl
                                                              | [<>] -> []

and parse_match_case : token Stream.t -> match_case = parser
                                                    | [< 'CASE _; 'LEFT_PAREN _; e = parse_expression; 'RIGHT_PAREN _; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY _ >] -> MatchCase (e, l)

                                                              
and parse_match_default_option : token Stream.t -> match_default option = parser
                                                                        | [< d = parse_match_default >] -> Some d
                                                                        | [<>] -> None

and parse_match_default : token Stream.t -> match_default = parser
                                                          | [< 'DEFAULT _; 'LEFT_CURLY _; l = parse_statement_list; 'RIGHT_CURLY _ >] -> MatchDefault l

                                                               
and parse_return_statement : token Stream.t -> return_statement = parser
                                                                | [< 'RETURN _; r = parse_return_statement_decoration >] -> r

and parse_return_statement_decoration : token Stream.t -> return_statement = parser
                                                                           | [< e = parse_expression >] -> ValueReturn e
                                                                             | [<>] -> VoidReturn


and parse_fun_call : token Stream.t -> function_call = parser
                                                     | [< 'IDENT (name,_); 'LEFT_PAREN _; args = parse_expresion_arguments; 'RIGHT_PAREN _ >] -> FunCall (name, args)


and parse_expression_arguments : token Stream.t -> expression list = parser
                                                                  | [< hd = parse_expression; tl = parse_expression_arguments_aux >] -> hd::tl
                                                                    | [<>] -> []

and parse_expression_arguments_aux : token Stream.t -> expression list = parser
                                                                       | [< 'COMMA _; l = parse_expression_arguments >] -> l
                                                                       | [<>] -> []

                                                                            
  
and parse_variable_expression : token Stream.t -> variable_expression = parser
                                                                      | [< 'IDENT (name,_); f = parse_variable_expression_aux >] -> f name

and parse_variable_expression_aux : token Stream.t -> (string -> variable_expression) = parser
                                                                                      | [< 'LEFT_BRACKET _; e = parse_expression; 'RIGHT_BRACKET _ >] -> (fun s -> ArrayValue (s, e))
                                                                                      | [<>] -> (fun s -> VarValue s)

and parse_expression : token Stream.t -> expression = failwith "TODO"
