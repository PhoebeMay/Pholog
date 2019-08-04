type token =
  | INT of (int)
  | PLUS
  | MINUS
  | END

open Parsing;;
let _ = parse_error;;
# 5 "parser.mly"
  open ParseTree
# 12 "parser.ml"
let yytransl_const = [|
  258 (* PLUS *);
  259 (* MINUS *);
  260 (* END *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\005\000\000\000\000\000\000\000\001\000\
\000\000\000\000"

let yydgoto = "\002\000\
\004\000\005\000"

let yysindex = "\006\000\
\007\255\000\000\000\000\000\000\254\254\007\255\007\255\000\000\
\001\255\001\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\255\006\255"

let yygindex = "\000\000\
\000\000\255\255"

let yytablesize = 10
let yytable = "\006\000\
\007\000\008\000\006\000\007\000\009\000\010\000\001\000\003\000\
\003\000\004\000"

let yycheck = "\002\001\
\003\001\004\001\002\001\003\001\006\000\007\000\001\000\001\001\
\004\001\004\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  END\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 19 "parser.mly"
                            (_1)
# 73 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 22 "parser.mly"
                            ( Int(_1) )
# 80 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 23 "parser.mly"
                            ( Plus(_1,_3) )
# 88 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 24 "parser.mly"
                            ( Minus(_1, _3) )
# 96 "parser.ml"
               : ParseTree.parseTree))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ParseTree.parseTree)
