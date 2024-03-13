MAIN=gui

all:
	dune build
	dune runtest

run: 
	dune runtest
	dune exec ./bin/$(MAIN).exe

clean::
	dune clean
