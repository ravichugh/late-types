%{

open Lang

%}

%token <int> INT
%token <string> STR
%token <bool> VBOOL
%token <string> VAR
%token <Lang.base_type> TBASE
%token <string> TVAR
%token <string> PRIMOP
%token
  IF THEN ELSE COMMA COLON LBRACE RBRACE SEMI LPAREN RPAREN LBRACK RBRACK
  FUN LET IN EQ EQARROW ARROW DOT TYPE MU PRINT HASH CLOSE FOLD UNFOLD 
  PRE SUB FI DOLLAR
  EOF 

%type <Lang.exp> program
%start program

%%

program : e=exp EOF { e }

typ :
 | t=TBASE { TBase t }
 | a=TVAR { TVar a }
 | LBRACE l=separated_list(SEMI,field_typ) RBRACE { TRecd l }
 | LPAREN s=typ ARROW t=typ RPAREN { TArrow (s, t) }
 | LPAREN a=TVAR COLON s=typ RPAREN EQARROW t=typ { TPre (a, s, t) }
 | MU a=TVAR DOT LBRACE l=separated_list(SEMI,field_typ) RBRACE { TMu (a, TRecd l) }
 | m=VAR LPAREN l=separated_list(COMMA,typ) RPAREN { TMacroApp (m, l) }

value :
 | i=INT   { VInt i }
 | b=VBOOL { VBool b }
 | s=STR   { VStr s }

exp_ :
 | v=value { EVal v }
 | x=VAR { EVar x }
 | e=exp DOT f=VAR { EProj (e, f) }
 | e1=exp LPAREN e2=exp RPAREN { EApp (e1, e2) }
 | e=exp LBRACK t=typ RBRACK { ETApp (e, t) }
 | LBRACE l=separated_list(SEMI,field_exp) RBRACE { (eRecd l).exp }
 | IF e1=exp THEN e2=exp ELSE e3=exp FI { EIf (e1, e2, e3) }
 | e1=exp op=PRIMOP e2=exp { EPrimOp (op, [e1; e2]) }
 | UNFOLD e=exp { EUnfold e }
 | FOLD LBRACK t=typ RBRACK e=exp { EFold (t, e) }
 | CLOSE e=exp { EClose e }
 | LPAREN e=exp_ RPAREN { e }
 | PRINT e=paren_exp { EPrint (None, e) }
 | PRINT s=STR e=paren_exp { EPrint (Some s, e) }
 | LET x=VAR EQ e1=exp IN e2=exp { ELet (x, e1, e2) }
 | FUN x=VAR ARROW e=exp { EFun (None, x, e) }
 | FUN a=TVAR ARROW e=exp { ETFun (a, e) }
 | FUN LPAREN x=VAR COLON t=typ RPAREN ARROW e=exp { EFun (Some t, x, e) }
 | PRE LPAREN x=VAR COLON a=TVAR SUB t=typ RPAREN ARROW e=exp
     { EPremethod (Some (a,t), x, e) }
 | TYPE m=VAR LPAREN xs=separated_list(COMMA,TVAR) RPAREN EQ t=typ e=exp
     { EMacroDef ((m, xs, t), e) }
 | e=exp HASH f=VAR LPAREN e2=exp RPAREN
     { let obj = eClose e in EApp (eApp (eProj (eUnfold obj) f) obj, e2) }
 | e=exp DOLLAR f=VAR LPAREN e2=exp RPAREN
     { EApp (eApp (eProj (eUnfold e) f) e, e2) }

exp : e=exp_ { wrapE e }

paren_exp :
 | LPAREN e=exp RPAREN { e }

field_exp :
 | f=VAR EQ e=exp { (f, e) }

field_typ :
 | f=VAR COLON t=typ { (f, t) }

(******************************************************************************)

%%
