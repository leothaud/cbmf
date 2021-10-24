type id = string
        
        
type type_ =
  CharType
  | StringType
  | Int8Type
  | Int16Type
  | Int32Type
  | Int64Type
  | UInt8Type
  | UInt16Type
  | UInt32Type
  | UInt64Type
  | FloatType
  | BoolType
  | ListType of type_
  | ArrayType of type_
  | TupleType of type_ list
  | UnionType of id
  | AliasType of id
  | StructType of id

and function_prototype = FunctionPrototype of type_ * id * (argument list)

and void_function_prototype = VoidFunctionPrototype of id * (argument list)

and argument = Argument of type_ * id

and value =
  | True
  | False
  | Integer of int
  | Float of float
  | Char of char
  | String of string
  | TupleValue of value list
  | BracketValue of value list (* list or array *)
  | StructValue of (id * value) list

and header_item =
  | HeaderTypeDef of type_definition
  | FunctionDeclaration of function_prototype * function_documentation
  | VoidFunctionDecaration of void_function_prototype * function_documentation

and function_documentation = FunctionDocumentation of string

and type_definition =
  | UnionDefiniton of id * (union_item list)
  | StructDefinition of id * (struct_item list) * (operator_overload list)
  | AliasDefinition of id * type_

and union_item = UnionItem of type_ * id

and struct_item = StructItem of type_ * id

and operator_overload = OperatorOverload of operator * id

and include_ = Include of string

and function_definition =
  | FunctionDefinition of function_prototype * function_body
  | VoidFunctionDefinition of void_function_prototype * function_body

and function_body = FunctionBody of function_item list

and function_item =
  | ItemStatement of statement
  | ItemVarDecl of variables_declaration

and statement =
  | AffectStmt of variable_expression * expression
  | IfStmt of expression * (statement list) * (statement list)
  | WhileStmt of expression * (statement list)
  | DoWhileStmt of (statement list) * expression
  | RepeatStmt of (statement list) * expression
  | ForStmt of
      (variables_declaration option) * (expression option) * (statement option) * (statement list)
  | ForeachStmt of type_ * id * expression * (statement list)
  | MatchStmt of expression * (match_case list) * (match_default option)
  | ReturnStmt of return_statement
  | FunCallStmt of function_call

and variables_declaration = VarsDecl of type_ * (declaration list)

and declaration =
  | NonInitializedDecl of variable_name
  | InitializedDecl of variable_name * value

and variable_name =
  | Var of id
  | Array of id * expression

and expression =
  | BinExpr of expression * operator * expression
  | ParenExpr of expression
  | ConstExpr of value
  | VarExpr of variable_expression
  | CastExpr of type_ * expression
  | NotExpr of expression
  | FunCallExpr of function_call

and variable_expression =
  | VarValue of id
  | ArrayValue of id * expression

and operator =
  | ArithmOp of arithmetic_operator
  | BoolOp of boolean_operator
  | CompOp of comparison_operator
  | BbBOp of bit_by_bit_operator

and arithmetic_operator =
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MODULO
  | INT_DIV
  | LEFT_SHIFT
  | RIGHT_SHIFT
  | CONCAT_STRING
  | CONCAT_LIST

and boolean_operator =
  | AND
  | OR

and comparison_operator =
  | LESS_THAN
  | MORE_THAN
  | EQUAL
  | LESS_OR_EQUAL
  | MORE_OR_EQUAL
  | NOT_EQUAL

and bit_by_bit_operator =
  | AND
  | OR

and function_call = FunCall of id * (expression list)

and match_case = MatchCase of expression * (statement list)

and match_default = MatchDefault of (statement list)

and return_statement =
  | VoidReturn
  | ValueReturn of expression


and header = Header of header_item list

and program = Program of (include_ list) * (function_definition list)
