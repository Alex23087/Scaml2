build:
	dune build

test:
	dune test

test-rerun:
	dune test --force

test-parser: build
	_build/default/bin/main.exe test/syntax.scaml

test-interpreter: build
	@COUNT=$$(ls -1 test/interpreter/*.scaml | wc -l) ;\
	CURRENT=0 ;\
	for f in test/interpreter/*.scaml ; \
	do \
		CURRENT=$$((CURRENT+1)) ;\
		echo "[$$CURRENT/$$COUNT] Testing $$(basename $${f%.*})" ; \
		EXPECTED="$${f%.*}.expected" ;\
		RES=$$(_build/default/bin/main_interpreter.exe $$f) ; \
		if [ $$? -eq 0 ]; then \
			if [ -e "$$EXPECTED" ]; then \
				echo $$RES | diff $$EXPECTED - ;\
			else \
				echo "$$RES" ;\
				echo "Succeeded";\
			fi;\
		else \
			echo "An error occurred:" ;\
			_build/default/bin/main_interpreter.exe $$f --print-expr ;\
		fi ;\
	done

setup:
	opam update
	opam upgrade
	opam install dune ppx_jane

.PHONY: build test test-rerun doc docopen setup