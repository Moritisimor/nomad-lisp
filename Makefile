.PHONY: all build test clean

all: build

build:
	dune build @all

test:
	dune runtest --force

clean:
	dune clean
