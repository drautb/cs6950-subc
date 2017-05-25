all:
	ocaml setup.ml -configure --enable-tests
	ocaml setup.ml -build
	ocaml setup.ml -test

clean:
	rm -rf _build myocamlbuild.ml setup.data setup.ml setup.log _tags
	oasis setup
