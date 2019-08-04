/* File parser.mly */

/* Header */
%{
  open ParseTree
%}

%token <string> NAME, VAR
%token LBRAC, RBRAC, COMMA, END, ARR, FSTOP, QUES
%start main             /* the entry point */
%type <string ParseTree.program> main

/* Grammar rules */
%%
main:
    program END {$1}
;
program:
  sentence QUES resolvant {Program($1, $3)}
  | sentence {Program($1, Resolvant([]))}
  | QUES resolvant {Program(Sentence([]),$2)}
;
resolvant:
  atom_list       {Resolvant($1)}
;
sentence:
    clause sentence {addClause $2 $1}
  | clause          {Sentence([$1])}
;
clause:
    atom ARR atom_list FSTOP {Clause($1,$3)}
  | atom FSTOP {Clause($1,[])}
;
atom_list:
    atom {[$1]}
  | atom COMMA atom_list {$1::$3}
atom:
    NAME LBRAC term_list RBRAC {Atom($1, $3)}
    | NAME {Atom($1, [])}
;
term:
    VAR {TVar($1)}
  | NAME {TFun($1,[])}
  | NAME LBRAC term_list RBRAC {TFun($1, $3)}
;
term_list:
    term {[$1]}
  | term COMMA term_list {$1::$3}
;
