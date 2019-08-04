(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
let variable = ['A'-'Z']['a'-'z''A'-'Z''0'-'9']*
let name = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*
let white_space = [' ' '\t' '\n']+
let int = ['0'-'9']+


rule token = parse
  | "is"            {IS}
  | "fail"          {FAIL}
  | "type"          {TYPEDEF}
  | "pred"          {PRED}
  | "int"           {INTTYP}
  | '='             {EQUALS}
  | '('             {LBRAC }
  | ')'             {RBRAC }
  | eof             {END }
  | ','             {COMMA }
  | '!'             {CUT}
  | ":-"            {ARR}
  | '+'             {PLUS}
  | '.'             {FSTOP}
  | "?-"            {QUES}
  | white_space     {token lexbuf}
  | '-'             {MINUS}
  | name as x       {NAME(x)}
  | variable as x   {VAR(x)}
  | int as x        {INT(int_of_string x)}
