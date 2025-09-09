F =  # nothing by default
src = # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec test/run_test.exe -- test '$(F)'

testc:
	dune exec test/run_test.exe -- test '$(F)' -c

teste:
	dune exec test/run_test.exe -- test '$(F)' -ce

parse:
	dune exec execs/run_parse.exe $(src)

type:
	dune exec execs/run_type.exe $(src)

compile:
	dune exec execs/run_compile.exe $(src)

%.exe:
	dune build execs/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	find bbctests/ -type f -regex '.*\.\(o\|s\|run\|result\)' -delete