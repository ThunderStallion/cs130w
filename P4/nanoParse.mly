%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token TRUE FALSE
%token <string> Id
%token LET
%token REC
%token EQ
%token IN
%token FUN ARROW
%token IF THEN ELSE
%token EOF
%token PLUS 
%token MINUS 
%token MUL 
%token DIV
%token LT LE NE
%token AND OR
%token LPAREN RPAREN
%token LBRAC SEMI RBRAC
%token COLONCOLON

%start exp 
%type <Nano.expr> exp

%%    

exp: LET Id EQ exp IN exp      { Let($2,$4,$6) }
    | LET REC Id EQ exp IN exp  { Letrec($3,$5,$7) }
    | FUN Id ARROW exp          { Fun($2,$4) }
    | IF exp THEN exp ELSE exp  { If($2,$4,$6) }
    | exp1                      {$1}

exp1: exp1 OR exp1              { Bin($1,Or,$3) }   
    | exp2                      { $1 }

exp2: exp2 AND exp2             { Bin($1,And,$3) }
    | exp3                      { $1 }

exp3: exp3 EQ exp3              { Bin($1,Eq,$3) }
    | exp3 LT exp3              { Bin($1,Lt,$3) }
    | exp3 LE exp3              { Bin($1,Le,$3) }
    | exp3 NE exp3              { Bin($1,Ne,$3) }
    | extra                      { $1 }

extra: extra COLONCOLON extra  { Bin($1,Cons,$3) }
    | LBRAC extra SEMI extra { Bin($2,Cons,$4) }
    | extra SEMI extra { Bin($1,Cons,$3) }
    | extra RBRAC { Bin($1,Cons,NilExpr) }
    | LBRAC RBRAC { NilExpr }
    | linked                    { $1 }

linked: LBRAC RBRAC { NilExpr }
    | exp4              { $1 }

exp4: exp4 PLUS exp5            { Bin($1,Plus,$3) }
    | exp4 MINUS exp5           { Bin($1,Minus,$3) }
    | exp5                      { $1 }

exp5: exp5 MUL exp6             { Bin($1,Mul,$3) }
    | exp5 DIV exp6             { Bin($1,Div,$3) }
    | exp6                      { $1 }

exp6: exp6 exp7 { App($1,$2) }
    | exp7      { $1 }

exp7: LPAREN exp RPAREN        { $2 }   
    | Num                       { Const $1 }
    | TRUE                       { True }
    | FALSE                      { False }
    | Id                         { Var($1) }
