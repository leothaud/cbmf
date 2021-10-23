# cbmf

cbmf is a langage that will be 


## grammar

> <u>__globals__</u>

	type ::=
		| 'void'
		| 'char'
		| 'string'
		| 'int8'
		| 'int16'
		| 'int32'
		| 'int64'
		| 'uint8'
		| 'uint16'
		| 'uint32'
		| 'uint64'
		| 'float'
		| <type>'[]'
		| <user_defined_type>
		| <tuple>
	user_defined_type ::=
		| <union>
		| <struct>
		| <alias>
	union ::= 'union' ident
	struct ::= 'struct' ident
	alias ::= ident
	tuple ::=
		| <type>
		| <type> '*' <tuple>
	function_prototype ::= <type> ident '(' (<argument> (',' <argument>)*)? ')'
	argument ::= <type> ident
	value ::=
		| int
		| float
		| char
		| string
		| '(' <value> (',' <value>)+ ')'
		| '[' (<value> (';' <value>)*)? ']'
		| '{' ident '=' <value> (';' ident '=' <value>)* '}'
	
> <u>__header file__</u>

	header ::= (<header_item> ';')+
	header_item ::=
		| <function_prototype> <function_documentation>?
		| <type_definition>
	function_documentation ::=  '[' string ']'
	type_definition ::=
		| <union_definition>
		| <struct_definition>
		| <alias_definition>
	union_definition ::= 'union' ident '{' (<union_item> ';')+ '}'
	union_item ::= <type> ':' ident
	struct_definition ::= 'struct' ident '{' (<struct_item> ';')+ (<operator_overload> ';')* '}'
	struct_item ::= <type> ident
	operator_overload ::= 'operator' <operator> = ident
	alias_definition ::= 'alias' ident <type>


> <u>__code file__</u>

	program ::=
	   	| <include>*
	   	| <function_definition>+
	include ::= '>include' string 
	function_definition ::= <function_prototype> '{' (<function_item> ';')+ '}'
	function_item ::=
		| <statement>
		| <variables_declaration>
	statement ::=
		| <affectation_statement>
		| <if_statement>
		| <while_statement>
		| <do_while_statement>
		| <repeat_until_statement>
		| <for_statement>
		| <foreach_statement>
		| <function_call>
	variables_declaration ::= <type> <declaration> (',' <declaration>)*
	declaration ::=
		| <variable_name>
		| <variable_name> '=' <value>
	variable_name ::=
		| ident
		| ident '[' <expression> ']'
	expression ::=
		| <binary_expression>
		| '(' expression ')'
		| <constant_expression>
		| <variable_expression>
		| <cast_expression>
		| <not_expression>
		| <function_call>
	binary_expression ::= <expression> <operator> <expression>
	constant_expression ::= <value>
	variable_expression ::=
		| ident
		| ident '[' <expression> ']'
	cast_expression ::= '(' <type> ')' <expression>
	not_expression ::= '!' <expression>
	operator ::=
		| <arithmetic_operator>
		| <boolean_operator>
		| <comparison_operator>
		| <bit_by_bit_operator>
	arithmetic_operator ::=
		| '+'
		| '-'
		| '*'
		| '/'
		| '%'
		| '//'
		| '<<'
		| '>>'
	boolean_operator ::=
		| '&&'
		| '||'
	comparison_operator ::=
		| '<'
		| '>'
		| '=='
		| '<='
		| '>='
		| '!='
	bit_by_bit_operator ::=
		| '&'
		| '|'
	function_call ::= ident '(' (<expresssion> (',' <expression>)*)? ')'
