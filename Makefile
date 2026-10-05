F =  # nothing by default
src = # nothing by default

.PHONY: init tests ctests compile clean clean-tests check check-tools check-ocaml bench

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
	# a warrior with a long name dying before an opponent's ;break overflowed a 60-byte buffer in
	# pMARS's sim.c (a trap, exit 133, on macOS); tools/pmars-host.sh patches the build
	tools/pmars-host.sh >/dev/null && _build/pmars-host/pmars -r 1 -b tools/pmars-trap/loser.red tools/pmars-trap/breaker.red </dev/null >/dev/null
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
	# End to end: the CLI exports (expect ...) as a behaviour spec, and pMARS runs it.
	dune exec execs/run_compile.exe -- --emit-beh _build/prog7_expect.beh examples/prog7_expect.src > /dev/null
	python3 tools/behave.py _build/prog7_expect.beh

# Scores the archetypes against downloaded benchmarks; needs network, so not part of check
# (tools/bench.py says how; --update rewrites tools/bench_baseline.json).
bench:
	python3 tools/bench.py

clean: clean-tests
	dune clean

clean-tests:
	rm -f bbctests/*.s bbctests/*.o bbctests/*.run bbctests/*.result bbctests/*~
	rm -f bbctests/*/*.s bbctests/*/*.o bbctests/*/*.run bbctests/*/*.result bbctests/*/*~
	rm -rf bbctests/*dSYM bbctests/*/*dSYM
