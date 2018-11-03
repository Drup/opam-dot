all:
	dune build

test:
	dune build @runtest

clean:
	dune clean

.PHONY: all test clean
