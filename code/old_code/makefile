useParser: prolog_lexer_parser/parseTree.ml prolog_lexer_parser/lexer.mll prolog_lexer_parser/parser.mly prolog_lexer_parser/useParser.ml
	ocamlc prolog_lexer_parser/parseTree.ml
	ocamllex prolog_lexer_parser/lexer.mll
	menhir prolog_lexer_parser/parser.mly
	ocamlfind ocamlc -linkpkg prolog_lexer_parser/parseTree.ml prolog_lexer_parser/parser.mli -o prolog_lexer_parser/parser.cmi
	ocamlfind ocamlc -linkpkg  prolog_lexer_parser/parseTree.ml prolog_lexer_parser/parser.ml prolog_lexer_parser/lexer.ml prolog_lexer_parser/useParser.ml -o run_parser.out
