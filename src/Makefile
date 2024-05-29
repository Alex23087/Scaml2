MAKEFLAGS+=-j1

build:
	dune build

test:
	dune test

test-rerun:
	dune test --force

test-parser: build
	_build/default/bin/main.exe test/syntax.scaml

test-interpreter-normal: build
	@COUNT=$$(ls -1 test/interpreter/normalexec/*.scaml | wc -l) ;\
	CURRENT=0 ;\
	for f in test/interpreter/normalexec/*.scaml ; \
	do \
		CURRENT=$$((CURRENT+1)) ;\
		echo "[$$CURRENT/$$COUNT] Testing $$(basename $${f%.*})" ; \
		EXPECTED="$${f%.*}.expected" ;\
		RES=$$(_build/default/bin/main_interpreter.exe $$f) ; \
		if [ $$? -eq 0 ]; then \
			if [ -e "$$EXPECTED" ]; then \
				echo "$$RES" | diff "$$EXPECTED" - ;\
			else \
				echo "$$RES" ;\
				echo "Succeeded";\
			fi;\
		else \
			echo "An error occurred:" ;\
			_build/default/bin/main_interpreter.exe $$f --print-expr ;\
		fi ;\
	done

test-interpreter-except: build
	@COUNT=$$(ls -1 test/interpreter/except/*.scaml | wc -l) ;\
	CURRENT=0 ;\
	for f in test/interpreter/except/*.scaml ; \
	do \
		CURRENT=$$((CURRENT+1)) ;\
		echo "[$$CURRENT/$$COUNT] Testing Exn $$(basename $${f%.*})" ; \
		RES=$$(_build/default/bin/main_interpreter.exe $$f > /dev/null 2> /dev/null) ; \
		if [ $$? -eq 0 ]; then \
			echo Did not throw an exception! ;\
			_build/default/bin/main_interpreter.exe $$f --print-expr ;\
		fi ;\
	done

test-interpreter: test-interpreter-normal test-interpreter-except

setup:
	opam update
	opam upgrade
	opam install dune ppx_jane

.PHONY: build test test-interpreter test-interpreter-normal test-interpreter-except test-rerun doc docopen setup