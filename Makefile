all:
	ocamlopt -c zipper.mli
	ocamlopt -c zipper.ml
	ocamlopt -c conf.ml
	ocamlfind ocamlopt -package core -package cow -thread zipper.cmx -c types.ml
	ocamlfind ocamlopt -syntax camlp4o -package core -package cow -package cow.syntax -thread types.cmx conf.cmx -c page.ml
	ocamlfind ocamlopt -syntax camlp4o -package core -package cow -package cow.syntax -linkpkg -thread zipper.cmx types.cmx conf.cmx page.cmx gen_html.ml -g -o gen_html 

clean:
	rm *.cmi *.cmo *.cmx *.o

