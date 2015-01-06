{
open LangParser
}

let letter  = ['A'-'Z''a'-'z''_']
let digit   = ['0'-'9']
let ident   = ['a'-'z''_''&'] (letter|digit|''')*
let tyvar   = ['A'-'Z'] (letter|digit|''')*
let locvar  = ['L'] (letter|digit)*
let white   = [' ' '\t' '\r']
let newline = ['\n']

let str =
  (letter
    | digit
    | [' ' '+' '-' '*' '/' '=' '(' ')' '&' '|'
       '.' ',' '{' '}' ':' ';' '#' '!' ''' '$'])*

rule token = parse
  | eof { EOF }

  | "print"      { PRINT }
  | "type"       { TYPE }
  | "int"        { TBASE Lang.TInt }
  | "bool"       { TBASE Lang.TBool }
  | "str"        { TBASE Lang.TStr }
  | "mu"         { MU }
  | "true"       { VBOOL true }
  | "false"      { VBOOL false }
  | "let"        { LET }
  | "in"         { IN }
  | "<:"         { SUB }
  | "-"          { PRIMOP "-" }
  | "+"          { PRIMOP "+" }
  | "*"          { PRIMOP "*" }
  | "=="         { PRIMOP "==" }
  | "^"          { PRIMOP "^" }
  | "<"          { PRIMOP "<" }
  | ">"          { PRIMOP ">" }
  | "<="         { PRIMOP "<=" }
  | ">="         { PRIMOP ">=" }
  | "=>"         { EQARROW }
  | "="          { EQ }
  | "->"         { ARROW }
  | "fun"        { FUN }
  | "pre"        { PRE }
  | "if"         { IF }
  | "fi"         { FI }
  | "then"       { THEN }
  | "else"       { ELSE }
  | "close"      { CLOSE }
  | "fold"       { FOLD }
  | "unfold"     { UNFOLD }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "{"          { LBRACE }
  | "}"          { RBRACE }
  | "["          { LBRACK }
  | "]"          { RBRACK }
  | "#"          { HASH }
  | "."          { DOT }
  | ","          { COMMA }
  | ";"          { SEMI }
  | ":"          { COLON }
  | "$"          { DOLLAR }

  | digit+ as s         { INT (int_of_string s) }
  | ident as s          { VAR s }
  | '"' (str as s) '"'  { STR s}
  | tyvar as s          { TVAR s }

  | white       { token lexbuf }
  | newline     { Lexing.new_line lexbuf; token lexbuf }

  | "(*"        { comments 0 lexbuf }

  | _  { raise (Failure ("Lex: bad char ["^(Lexing.lexeme lexbuf)^"]")) }

and comments level = parse
  | "*)"    { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "(*"    { comments (level+1) lexbuf }
  | newline { Lexing.new_line lexbuf; comments level lexbuf }
  | _	  	  { comments level lexbuf }
  | eof		  { Printf.printf "comments are not closed\n"; raise End_of_file }

