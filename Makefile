MAIN=locate

all:: test run

test::
	dune runtest

run::
	dune runtest
	dune exec ./bin/$(MAIN).exe

www::
	dune runtest
	dune build ./www/hg.bc.js

clean::
	dune clean
