RUN=hgui

all:: run

build::
	dune build

test::
	dune build
	dune runtest

run::
	dune build
	dune runtest
	dune exec ./bin/$(RUN).exe axioms

www::
	dune runtest
	dune build ./www/hg.bc.js

clean::
	dune clean
