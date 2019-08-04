useParser: parseTree.ml lexer.mll parser.mly useParser.ml
	ocamlc parseTree.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c parser.mli -o parser.cmi
	ocamlfind ocamlc -linkpkg  parseTree.ml parser.ml lexer.ml useParser.ml -o run_parser.out
	#rm lexer.cmi lexer.cmo parser.cmi parser.cmo parseTree.cmi parseTree.cmo useParser.cmi useParser.cmo a.out


parserTest: parseTree.ml lexer.mll parser.mly parserTest.ml
	ocamlc parseTree.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c parser.mli
	ocamlfind opt  -linkpkg parseTree.ml parser.ml lexer.ml -package oUnit parserTest.ml -o parserTest
