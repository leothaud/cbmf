# cbmf

cbmf is a langage that will be 


## grammar

> <u>__globals__</u>

	type ::=
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
		| 'bool'
		| <type> list
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
	void_function_prototype ::= 'void' ident '(' (<argument> (',' <argument>)*)? ')' 
	argument ::= <type> ident
	value ::=
		| 'true'
		| 'false'
		| int
		| float
		| char
		| string
		| '(' <value> (',' <value>)+ ')'
		| '[' (<value> (';' <value>)*)? ']'
		| '{' ident '=' <value> (';' ident '=' <value>)* '}'
	header_item ::=
		| <type_definition>
		| <function_prototype> <function_documentation>?
		| <void_function_prototype> <function_documentation>?
	function_documentation ::=  '[' string ']'
	type_definition ::=
		| <union_definition>
		| <struct_definition>
		| <alias_definition>
	union_definition ::= 'union' ident '{' (<union_item> ';')+ '}'
	union_item ::= <type> ':' ident
	struct_definition ::= 'struct' ident '{' (<struct_item> ';')+ (<operator_overload> ';')* '}'
	struct_item ::= <type> ident
	operator_overload ::= 'operator' <operator> '=' ident
	alias_definition ::= 'alias' ident <type>
	include ::= '>include' string 
	function_definition ::=
		| <function_prototype> '{' (<function_item> ';')+ '}'
		| <void_function_prototype> '{' (<function_item> ';')+ '}'
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
		| <match_statement>
		| <return_statement>
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
		| '(' <expression> ')'
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
		| '^'
		| '::'
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
	affectation_statement ::= <variable_expression> '=' <expression>
	if_statement ::= 'if' '(' <expression> ')' '{' <statement>* '}' ('else' '{' <statement> '}')?
	while_statement ::= 'while' '(' <expression> ')' '{' <statement>* '}'
	do_while_statement ::= 'do' '{' <statement>* '}' 'while' '(' <expression> ')'
	repeat_until_statement ::= 'repeat' '{' <statement>* '}' 'until' '(' <expression> ')'
	for_statement ::= 'for' '(' <variables_declaration>? ';' <expression>? ';' <statement>? ')' '{' <statement>* '}'
	foreach_statement ::= 'foreach' '(' <type> ident ':' <expression> ')' '{' <statement>* '}'
	match_statement ::= 'match' '(' <expression> ')' '{' <match_case>* <match_default>? '}'
	match_case ::= 'case' '(' <expression> ')' '{' <statement>* '}'
	match_default ::= 'default' '{' <statement>* '}'
	return_statement ::= 'return' <expression>?

> <u>__header file__</u>

	header ::= (<header_item> ';')+

> <u>__program file__</u>

	program ::=
	   	| <include>*
	   	| <function_definition>+
