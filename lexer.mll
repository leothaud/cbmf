{
  open Lexing
  open Token

  type error = {
    character: char;
    line: int;
    pos: int;
  }

  let get_pos lexbuf =
    let line = lexbuf.lex_curr_p.pos_lnum in
    let column  = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    (line, column)
;;


  exception Unexpected_character of error
}

(**********************************************************)

let letter = ['a'-'z']
let LETTER = ['A'-'Z']
let digit  = ['0'-'9']
let ascii  = _ # ['\n' '"']
let blanks = [' ' '\n' '\t']

rule tokenize = parse
  (* skip new lines and update line count (useful for error location) *)
  | '\n'
      { let _ = new_line lexbuf in tokenize lexbuf }

  (* skip other blanks *)
  | blanks
      { tokenize lexbuf }

  (* comments *)
  | "//" ( [^'\n']* as s) '\n'
      { (COMMENT (s, get_pos lexbuf)) :: tokenize lexbuf }
  | "//" ( [^'\n']* as s) eof
      { (COMMENT (s, get_pos lexbuf)) :: [EOF (get_pos lexbuf)] }
  | "/*"
      { (COMMENT (comment lexbuf, get_pos lexbuf)) :: tokenize lexbuf }

  | "void"
      { VOID (get_pos lexbuf) :: tokenize lexbuf }
  | "string"
      { STRING (get_pos lexbuf) :: tokenize lexbuf }
  | "char"
      { CHAR (get_pos lexbuf) :: tokenize lexbuf }
  | "int8"
      { INT8 (get_pos lexbuf) :: tokenize lexbuf }
  | "int16"
      { INT16 (get_pos lexbuf) :: tokenize lexbuf }
  | "int32"
      { INT32 (get_pos lexbuf) :: tokenize lexbuf }
  | "int64"
      { INT64 (get_pos lexbuf) :: tokenize lexbuf }
  | "uint8"
      { UINT8 (get_pos lexbuf) :: tokenize lexbuf }
  | "uint16"
      { UINT16 (get_pos lexbuf) :: tokenize lexbuf }
  | "uint32"
      { UINT32 (get_pos lexbuf) :: tokenize lexbuf }
  | "uint64"
      { UINT64 (get_pos lexbuf) :: tokenize lexbuf }
  | "bool"
      { BOOL (get_pos lexbuf) :: tokenize lexbuf }
  | "list"
      { LIST (get_pos lexbuf) :: tokenize lexbuf }
  | "union"
      { UNION (get_pos lexbuf) :: tokenize lexbuf }
  | "struct"
      { STRUCT (get_pos lexbuf) :: tokenize lexbuf }
  | "alias"
      { ALIAS (get_pos lexbuf) :: tokenize lexbuf }

  | '^'
      { CARET (get_pos lexbuf) :: tokenize lexbuf }
  | '@'
      { AT (get_pos lexbuf) :: tokenize lexbuf }

  | '('
      { LEFT_PAREN (get_pos lexbuf) :: tokenize lexbuf }
  | ')'
      { RIGHT_PAREN (get_pos lexbuf) :: tokenize lexbuf }
  | '{'
      { LEFT_CURLY (get_pos lexbuf) :: tokenize lexbuf }
  | '}'
      { RIGHT_CURLY (get_pos lexbuf) :: tokenize lexbuf }
  | '['
      { LEFT_BRACKET (get_pos lexbuf) :: tokenize lexbuf }
  | ']'
      { RIGHT_BRACKET (get_pos lexbuf) :: tokenize lexbuf }
  | '+'
      { PLUS (get_pos lexbuf) :: tokenize lexbuf }
  | '-'
      { MINUS (get_pos lexbuf) :: tokenize lexbuf }
  | '*'
      { STAR (get_pos lexbuf) :: tokenize lexbuf }
  | "//"
      { DOUBLE_SLASH (get_pos lexbuf) :: tokenize lexbuf }
  | '/'
      { SLASH (get_pos lexbuf) :: tokenize lexbuf }
  | '%'
      { PERCENT (get_pos lexbuf) :: tokenize lexbuf }
  | "::"
      { DOUBLE_COLON (get_pos lexbuf) :: tokenize lexbuf }
  | ':'
      { COLON (get_pos lexbuf) :: tokenize lexbuf }
  | "<<"
      { DOUBLE_LESS_THAN (get_pos lexbuf) :: tokenize lexbuf }
  | '<'
      { LESS_THAN (get_pos lexbuf) :: tokenize lexbuf }
  | ">>"
      { DOUBLE_MORE_THAN (get_pos lexbuf) :: tokenize lexbuf }
  | '>'
      { MORE_THAN (get_pos lexbuf) :: tokenize lexbuf }
  | "&&"
      { DOUBLE_AMPERSAND (get_pos lexbuf) :: tokenize lexbuf }
  | '&'
      { AMPERSAND (get_pos lexbuf) :: tokenize lexbuf }
  | "||"
      { DOUBLE_VERTICAL_BAR (get_pos lexbuf) :: tokenize lexbuf }
  | '|'
      { VERTICAL_BAR (get_pos lexbuf) :: tokenize lexbuf }
  | "=="
      { DOUBLE_EQUAL (get_pos lexbuf) :: tokenize lexbuf }
  | '='
      { EQUAL (get_pos lexbuf) :: tokenize lexbuf }
  | "<="
      { LESS_OR_EQUAL (get_pos lexbuf) :: tokenize lexbuf }
  | ">="
      { MORE_OR_EQUAL (get_pos lexbuf) :: tokenize lexbuf }
  | "!="
      { NOT_EQUAL (get_pos lexbuf) :: tokenize lexbuf }
  | ','
      { COMMA (get_pos lexbuf) :: tokenize lexbuf }
  | ';'
      { SEMICOLON (get_pos lexbuf) :: tokenize lexbuf }
  | '.'
      { DOT (get_pos lexbuf) :: tokenize lexbuf }
  | "false"
      { FALSE_VALUE (get_pos lexbuf) :: tokenize lexbuf }
  | "true"
      { TRUE_VALUE (get_pos lexbuf) :: tokenize lexbuf }
  | "operator"
      { OPERATOR (get_pos lexbuf) :: tokenize lexbuf }
  | "include"
      { INCLUDE (get_pos lexbuf) :: tokenize lexbuf }
  | '!'
      { EXCLAMATION_MARK (get_pos lexbuf) :: tokenize lexbuf }
  | "if"
      { IF (get_pos lexbuf) :: tokenize lexbuf }
  | "else"
      { ELSE (get_pos lexbuf) :: tokenize lexbuf }
  | "while"
      { WHILE (get_pos lexbuf) :: tokenize lexbuf }
  | "do"
      { DO (get_pos lexbuf) :: tokenize lexbuf }
  | "foreach"
      { FOREACH (get_pos lexbuf) :: tokenize lexbuf }
  | "for"
      { FOR (get_pos lexbuf) :: tokenize lexbuf }
  | "match"
      { MATCH (get_pos lexbuf) :: tokenize lexbuf }
  | "case"
      { CASE (get_pos lexbuf) :: tokenize lexbuf }
  | "repeat"
      { REPEAT (get_pos lexbuf) :: tokenize lexbuf }
  | "until"
      { UNTIL (get_pos lexbuf) :: tokenize lexbuf }
  | "default"
      { DEFAULT (get_pos lexbuf) :: tokenize lexbuf }
  | "return"
      { RETURN (get_pos lexbuf) :: tokenize lexbuf }


  | (letter (letter | LETTER | digit | '_')*) as s
      { IDENT (s, get_pos lexbuf) :: tokenize lexbuf }
  | '"' ([^'"']* as s) '"'
      { STRING_VALUE (s, get_pos lexbuf)  :: tokenize lexbuf }
  | (digit+ '.' digit+) as f
      { FLOAT_VALUE (float_of_string f, get_pos lexbuf) :: tokenize lexbuf }
  | (digit+) as i
      { INTEGER_VALUE (int_of_string i, get_pos lexbuf) :: tokenize lexbuf }
  | "'" ([^'\''] as c) '\''
      { CHAR_VALUE (c, get_pos lexbuf) :: tokenize lexbuf }


  (* end-of-file : end up with the empty stream *)
  | eof
      { [EOF (get_pos lexbuf)] }

  (* catch errors *)
  | _ as c
    {
      let e = {
          character = c;
          line = lexbuf.lex_curr_p.pos_lnum;
          pos  = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol;
        }
      in raise (Unexpected_character e)
    }

and comment = parse
  | "*/"  { "" }
  | _ as s  { (String.make 1 s) ^ (comment lexbuf) }
  | eof { failwith "Unexpected End-Of-File in comment" }