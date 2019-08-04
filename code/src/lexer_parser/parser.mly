/* File parser.mly */

/* Header */
%{
  open ParseTree
%}

%token <string> NAME, VAR
%token <int> INT
%token LBRAC, RBRAC, COMMA, END, ARR, FSTOP, QUES, IS, PLUS, MINUS, CUT, FAIL, TYPEDEF, EQUALS, PRED, INTTYP

%left PLUS MINUS

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
  clause_body       {Resolvant($1)}
;
sentence:
    sentence_item sentence {addClause $2 $1}
  | sentence_item          {Sentence([$1])}
;

sentence_item:
  clause              {C($1)}
  | new_type          {D($1)}
  | pred_def          {P($1)}
;

pred_def:
  PRED NAME LBRAC typedef_body_list RBRAC FSTOP  {PredDef($2,$4)}
  | PRED NAME FSTOP                              {PredDef($2,[])}
;
new_type:
  TYPEDEF NAME var_list EQUALS typedef_list FSTOP  {TypeDef($2,$3,$5)}
  | TYPEDEF NAME EQUALS typedef_list   FSTOP       {TypeDef($2,[],$4)}
;
var_list:
  VAR                   {[$1]}
  | VAR COMMA var_list  {$1::$3}
;

typedef_list:
  typedef_list_elem                               {[$1]}
  | typedef_list_elem COMMA  typedef_list         {$1::$3}

typedef_list_elem:
  NAME                                {TypeDefRight($1,[])}
| NAME LBRAC typedef_body_list RBRAC  {TypeDefRight($1, $3)}
;

typedef_body_list:
  typedef_body                              {[$1]}
| typedef_body COMMA typedef_body_list      {$1::$3}
;

typedef_body:
  VAR                                       {TypeVar($1)}
| NAME                                      {TypeCons($1,[])}
| NAME LBRAC typedef_body_list RBRAC        {TypeCons($1, $3)}
| INTTYP                                    {IntTyp}
;

clause:
    atom ARR clause_body FSTOP {Clause($1,$3)}
  | atom FSTOP {Clause($1,[])}
;
clause_body:
    clause_body_one         {[$1]}
  | clause_body_one COMMA clause_body {$1::$3}
;
clause_body_one:
    atom {CAT($1)}
  | is_expr {CAR($1)}
  | CUT {Cut}
  | FAIL {Fail}
;
atom:
    NAME LBRAC term_list RBRAC {Atom($1, $3)}
    | NAME {Atom($1, [])}
;
is_expr:
   VAR IS arith {IsExpr($1,$3)}
;

arith:
 arith PLUS arith     {Plus($1,$3)}
 | arith MINUS arith  {Subtract($1,$3)}
 | arith_base         {Base($1)}
;

arith_base:
  INT  {Int($1)}
  | MINUS INT {Int($2 * (-1))}
  | VAR  {Var($1)}
;

term:
    VAR {TVar($1)}
  | INT {TInt($1)}
  | NAME {TFun($1,[])}
  | NAME LBRAC term_list RBRAC {TFun($1, $3)}
;
term_list:
    term {[$1]}
  | term COMMA term_list {$1::$3}
;
