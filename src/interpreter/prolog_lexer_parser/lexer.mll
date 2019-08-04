(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
let variable = ['A'-'Z']['a'-'z''A'-'Z''0'-'9']*
let name = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*
let white_space = [' ' '\t' '\n']+

rule token = parse
  | name as x       {NAME(x) }
  | variable as x   {VAR(x) }
  | '('             {LBRAC }
  | ')'             {RBRAC }
  | eof             {END }
  | ','             {COMMA }
  | ":-"            {ARR}
  | '.'             {FSTOP}
  | "?-"            {QUES}
  | white_space     {token lexbuf}
