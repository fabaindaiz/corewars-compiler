F =  # nothing by default
src = # nothing by default

.PHONY: test

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

clean: clean-tests
	dune clean

clean-tests:
	rm -f bbctests/*.s bbctests/*.o bbctests/*.run bbctests/*.result bbctests/*~
	rm -rf bbctests/*dSYM
