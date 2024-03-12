MAIN=gui

all:
	dune build

run: 
	dune exec ./bin/$(MAIN).exe

clean::
	dune clean
