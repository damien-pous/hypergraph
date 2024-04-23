MAIN=gui

all:: test run

test::
	dune runtest

run::
	dune runtest
	dune exec ./bin/$(MAIN).exe

clean::
	dune clean
