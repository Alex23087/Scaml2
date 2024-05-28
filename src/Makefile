build:
	dune build

test:
	dune test

test-rerun:
	dune test --force

test-parser: build
	_build/default/bin/main.exe test/syntax.scaml

setup:
	opam update
	opam upgrade
	opam install dune ppx_jane

.PHONY: build test test-rerun doc docopen setup