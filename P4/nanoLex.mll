{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
    eof         { EOF }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | "<" { LT }
  | "<=" { LE }
  | "!=" { NE }
  | "&&" { AND }
  | "||" { OR }
  | "::" { COLONCOLON }
  | "[" { LBRAC }
  | ";" { SEMI }
  | "]" { RBRAC }
  | "true" { TRUE }
  | "false" { FALSE }
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQ }
  | "in" { IN }
  | "fun" { FUN }
  | "->" { ARROW } 
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE } 
  | ['0'-'9']* as num { Num((int_of_string num)) }
  | ['A'-'Z' 'a'-'z' '0'-'9' '_']* as str { Id(str) }
  | [' ' '\n' '\r' '\t'] {token lexbuf} 

  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }

