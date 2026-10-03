F =  # nothing by default
src = # nothing by default

.PHONY: init tests ctests compile clean clean-tests check check-tools check-ocaml

# The `execute` group runs pmars/pmars, a Linux x86-64 binary (d-7d2612-3d04ba); elsewhere
# check-ocaml runs the groups that need no pMARS.
PLATFORM := $(shell uname -s)-$(shell uname -m)

init:
	dune build @check

tests:
	dune exec execs/run_test.exe -- test '$(F)'

ctests:
	dune exec execs/run_test.exe -- test '$(F)' -c

compile: 
	dune exec execs/run_compile.exe $(src)

%.s: %.src 
	dune exec execs/run_compile.exe $< > $@

%.exe:
	dune build execs/$@

# The gate (AGENTS.md). check-tools needs only Python 3.11+ and a C compiler; check-ocaml needs
# the opam switch described in REFERENCE.md.
check: check-tools check-ocaml

check-tools:
	python3 tools/audit.py
	python3 tools/behave.py
	python3 .agents/tools/bundle.py verify
	python3 .agents/tools/bundle.py ids docs/decisions.md docs/roadmap.md .claude/logs/agent-changelog.md

check-ocaml:
	dune build
ifeq ($(PLATFORM),Linux-x86_64)
	dune exec execs/run_test.exe
else
	dune exec execs/run_test.exe -- test '^([^e]|e[^x]|ex[^e]).*$$'  # every group but execute
	@echo "check-ocaml: the execute group did not run ($(PLATFORM) cannot run pmars/pmars)"
endif

clean: clean-tests
	dune clean

clean-tests:
	rm -f bbctests/*.s bbctests/*.o bbctests/*.run bbctests/*.result bbctests/*~
	rm -f bbctests/*/*.s bbctests/*/*.o bbctests/*/*.run bbctests/*/*.result bbctests/*/*~
	rm -rf bbctests/*dSYM bbctests/*/*dSYM
