all:: test run

build::
	dune build

test::
	dune build
	dune runtest

run::
	dune build
	dune runtest
	dune exec ./bin/locate.exe axioms

www::
	dune runtest
	dune build ./www/hg.bc.js

clean::
	dune clean
