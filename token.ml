type line = int

type col = int

type position = line * col

type token =
  | IDENT of string * position
  | COMMENT of string * position
  | EOF of position
  | VOID of position
  | CHAR of position
  | STRING of position
  | INT8 of position
  | INT16 of position
  | INT32 of position
  | INT64 of position
  | UINT8 of position
  | UINT16 of position
  | UINT32 of position
  | UINT64 of position
  | FLOAT of position
  | BOOL of position
  | LIST of position
  | UNION of position
  | STRUCT of position
  | ALIAS of position
  | LEFT_PAREN of position (* '(' *)
  | RIGHT_PAREN of position (* ')' *)
  | LEFT_BRACKET of position (* '[' *)
  | RIGHT_BRACKET of position (* ']' *)
  | LEFT_CURLY of position (* '{' *)
  | RIGHT_CURLY of position (* '}' *)
  | PLUS of position (* '+' *)
  | MINUS of position (* '-' *)
  | SLASH of position (* '/' *)
  | PERCENT of position (* '%' *)
  | DOUBLE_SLASH of position (* '//' *)
  | DOUBLE_LESS_THAN of position
  | DOUBLE_MORE_THAN of position
  | DOUBLE_COLON of position
  | DOUBLE_AMPERSAND of position
  | AMPERSAND of position
  | DOUBLE_VERTICAL_BAR of position (* '||' *)
  | VERTICAL_BAR of position (* '|' *)
  | STAR of position (* '*' *)
  | DOUBLE_EQUAL of position (* '==' *)
  | LESS_OR_EQUAL of position (* '<=' *)
  | MORE_OR_EQUAL of position (* '>=' *)
  | NOT_EQUAL of position (* '!=' *)
  | EQUAL of position (* '=' *)
  | COMMA of position (* ',' *)
  | SEMICOLON of position (* ';' *)
  | COLON of position (* ':' *)
  | DOT of position (* '.' *)
  | MORE_THAN of position (* '>' *)
  | LESS_THAN of position (* '<' *)
  | CHAR_VALUE of char * position
  | STRING_VALUE of string * position
  | INTEGER_VALUE of int * position
  | FLOAT_VALUE of float * position
  | TRUE_VALUE of position
  | FALSE_VALUE of position
  | OPERATOR of position
  | INCLUDE of position
  | EXCLAMATION_MARK of position (* '!' *)
  | IF of position
  | ELSE of position
  | WHILE of position
  | DO of position
  | FOR of position
  | FOREACH of position
  | MATCH of position
  | CASE of position
  | DEFAULT of position
  | RETURN of position


let print_token (token : token) : unit =
  match token with
  | IDENT (s,_) -> Printf.printf "IDENT %s" s
  | COMMENT (s,_) -> Printf.printf "COMMENT %s" s
  | EOF _ -> ()
  | VOID _ -> Printf.printf "VOID"
  | CHAR _ -> Printf.printf "CHAR"
  | STRING _ -> Printf.printf "STRING"
  | INT8 _ -> Printf.printf "INT8"
  | INT16 _ -> Printf.printf "INT16"
  | INT32 _ -> Printf.printf "INT32"
  | INT64 _-> Printf.printf "INT64"
  | UINT8 _ -> Printf.printf "UINT8"
  | UINT16 _ -> Printf.printf "UINT16"
  | UINT32 _ -> Printf.printf "UINT32"
  | UINT64 _-> Printf.printf "UINT64"
  | FLOAT _ -> Printf.printf "FLOAT"
  | BOOL _ -> Printf.printf "BOOL"
  | LIST _ -> Printf.printf "LIST"
  | UNION _ -> Printf.printf "UNION"
  | STRUCT _ -> Printf.printf "STRUCT"
  | ALIAS _ -> Printf.printf "ALIAS"
  | LEFT_PAREN _ -> Printf.printf "("
  | RIGHT_PAREN _ -> Printf.printf ")"
  | LEFT_BRACKET _ -> Printf.printf "["
  | RIGHT_BRACKET _ -> Printf.printf "]"
  | LEFT_CURLY _ -> Printf.printf "{"
  | RIGHT_CURLY _ -> Printf.printf "}"
  | PLUS _ -> Printf.printf "+" (* '+' *)
  | MINUS _ -> Printf.printf "-" (* '-' *)
  | SLASH _ -> Printf.printf "/"
  | PERCENT _ -> Printf.printf "%%"
  | DOUBLE_SLASH _ -> Printf.printf "//"
  | DOUBLE_LESS_THAN _ -> Printf.printf "<<"
  | DOUBLE_COLON _ -> Printf.printf "::"
  | DOUBLE_AMPERSAND _ -> Printf.printf "&&"
  | AMPERSAND _ -> Printf.printf ""
  | DOUBLE_VERTICAL_BAR _ -> Printf.printf "||"
  | VERTICAL_BAR _ -> Printf.printf "|"
  | STAR _ -> Printf.printf "*"
  | DOUBLE_EQUAL _ -> Printf.printf "==" (* '==' *)
  | LESS_OR_EQUAL _ -> Printf.printf "<="
  | MORE_OR_EQUAL _ -> Printf.printf ">="
  | NOT_EQUAL _ -> Printf.printf "!="
  | EQUAL _ -> Printf.printf "="
  | COMMA _ -> Printf.printf ","
  | SEMICOLON _ -> Printf.printf ";"
  | COLON _ -> Printf.printf ":"
  | DOT _ -> Printf.printf "."
  | MORE_THAN _ -> Printf.printf ">"
  | LESS_THAN _ -> Printf.printf "<"
  | CHAR_VALUE (c,_) -> Printf.printf "CHAR \'%c\'" c
  | STRING_VALUE (s,_) -> Printf.printf "STRING \"%s\"" s
  | INTEGER_VALUE (i,_) -> Printf.printf "INTEGER %d" i
  | FLOAT_VALUE (f, _) -> Printf.printf "FLOAT %f" f
  | TRUE_VALUE _ -> Printf.printf "TRUE"
  | FALSE_VALUE _ -> Printf.printf "FALSE"
  | OPERATOR _ -> Printf.printf "OPERATOR"
  | INCLUDE _ -> Printf.printf "INCLUDE"
  | EXCLAMATION_MARK _ -> Printf.printf "!"
  | IF _ -> Printf.printf "IF"
  | ELSE _ -> Printf.printf "ELSE"
  | WHILE _ -> Printf.printf "WHILE"
  | DO _ -> Printf.printf "DO"
  | FOR _ -> Printf.printf "FOR"
  | FOREACH _ -> Printf.printf "FOREACH"
  | MATCH _ -> Printf.printf "MATCH"
  | CASE _ -> Printf.printf "CASE"
  | DEFAULT _ -> Printf.printf "DEFAULT"
  | RETURN _ -> Printf.printf "RETURN"
  | _ -> Printf.printf ">>"
