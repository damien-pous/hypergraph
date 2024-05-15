MAIN=locate

all:: test run

build::
	dune build

test::
	dune build
	dune runtest

run::
	dune build
	dune runtest
	dune exec ./bin/$(MAIN).exe

www::
	dune runtest
	dune build ./www/hg.bc.js

clean::
	dune clean
