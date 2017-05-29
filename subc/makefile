all:
	ocaml setup.ml -configure --enable-tests > /dev/null
	ocaml setup.ml -build
	ocaml setup.ml -test

clean:
	rm -rf _build myocamlbuild.ml setup.data setup.ml setup.log _tags
	rm -f src/lib/parser.automaton src/lib/parser.conflicts src/lib/parser.ml src/lib/parser.mli
	oasis setup
