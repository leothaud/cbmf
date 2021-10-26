open ASD
open Parser
   


   
type extension = HEADER | PROGRAM


let find_ext (s : string) : extension =
  let rec aux (l : string list) : extension =
    match l with
    | [] -> failwith "OhOh"
    | [e] -> begin
        if String.equal e "cbmf" then
          PROGRAM
        else
          begin
            if String.equal e "hbmf" then
              HEADER
            else
              failwith "files should be cmbf or hbmf file"
          end
      end
    | hd::tl -> aux tl
  in aux (String.split_on_char '.' s)

let print_file_array (f : cbmf_file) : unit =
  match f with
  | ProgramFile _ -> Printf.printf "Program\n"
  | HeaderFile _ -> Printf.printf "Header\n"
  | NOP -> Printf.printf "NOP\n"
         
let functionnal_main () =
  let num_file = (Array.length Sys.argv) - 1 in
  if num_file == 0 then failwith "No input file"
  else begin
      let files = Array.make num_file NOP in
      begin
        for i = 0 to num_file - 1 do
          let s = Sys.argv.(i+1) in
          let e = find_ext s in
          let ch = open_in s in
          let file = really_input_string ch (in_channel_length ch) in
          let lexbuf = Lexing.from_string file in
          let tok_list = Lexer.tokenize lexbuf in
          let tok_stream = Stream.of_list tok_list in
          begin
            match e with
            | HEADER -> files.(i) <- HeaderFile (parse_header tok_stream)
            | PROGRAM -> files.(i) <- ProgramFile (parse_program tok_stream)
          end;
          close_in ch;
        done;
        (fun x -> ()) (Array.map print_file_array files);
      end
    end

                
let other_main () =
  let num_file = (Array.length Sys.argv) - 1 in
  if num_file == 0 then failwith "No input file"
  else begin
      let files = Array.make num_file NOP in
      begin
        for i = 0 to num_file -1 do
          let s = Sys.argv.(i+1) in
          let ch = open_in s in
          let file = really_input_string ch (in_channel_length ch) in
          let lexbuf = Lexing.from_string file in
          let tok_list = Lexer.tokenize lexbuf in
          let tok_stream = Stream.of_list tok_list in
          files.(i) <- parse_file tok_stream;
          close_in ch;
        done;
        (fun x -> ()) (Array.map print_file_array files);
      end;
    end
      
let _ = other_main ()
