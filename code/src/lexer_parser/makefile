updateParser: parseTree.ml lexer.mll parser.mly
	ocamlc parseTree.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c parser.mli -o parser.cmi
	#rm lexer.cmi lexer.cmo parser.cmi parser.cmo parseTree.cmi parseTree.cmo useParser.cmi useParser.cmo a.out


parserTest: parseTree.ml lexer.mll parser.mly parserTest.ml
	ocamlc parseTree.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c parser.mli
	ocamlfind opt  -linkpkg parseTree.ml parser.ml lexer.ml -package oUnit parserTest.ml -o parserTest
