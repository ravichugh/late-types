
open Lang
open Lexing


let string_of_position (p, e) = 
  Format.sprintf "%s:%d:%d-%d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol) (e.pos_cnum - e.pos_bol)

(* rkc: handling position info similar to Lambdajs.parse_lambdajs and
   Lambdajs_env.parse_env *)
let doParse start_production name =
  let lexbuf = Lexing.from_channel (open_in name) in
  let strPos () = string_of_position (lexbuf.lex_curr_p, lexbuf.lex_curr_p) in
    try begin
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                               pos_fname = name; pos_lnum = 1 };
      start_production LangLexer.token lexbuf
    end with
      | Failure "lexing: empty token" ->
          printParseErr (spr "lexical error\n\nat %s" (strPos ()))
      | Failure s when strPrefix s "Lex: bad char" ->
          printParseErr (spr "%s\n\nat %s" s (strPos ()))
      | Failure s when strPrefix s "parse error" ->
          printParseErr s
      | Failure s when strPrefix s "lexical error" ->
          printParseErr s
      | Parsing.Parse_error _  (* thrown when using ocamlyacc *)
      | LangParser.Error ->    (* thrown when using menhir *)
          printParseErr
            (spr "unexpected token [%s]\n\nat %s" (lexeme lexbuf) (strPos ()))

let _ =
  match Array.to_list Sys.argv with
    | [_; f] -> begin
        let e = doParse LangParser.program f in
        Tc.tcExp e;
        Semantics.run e;
        ()
      end
    | _ -> failwith "usage: ./isolate source_file"

