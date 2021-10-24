let _ =
    (* lexical and syntactic analysis *)
  let lexbuf = Lexing.from_channel stdin in
  let tok_list = Lexer.tokenize lexbuf in
  let f = fun x -> begin
        Token.print_token x;
        Printf.printf "\n"
      end in
  List.map f tok_list
