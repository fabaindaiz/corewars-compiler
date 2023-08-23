# Picked from https://stackoverflow.com/questions/714100/os-detecting-makefile
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	BIN_FORMAT = elf64
	CFLAGS ?= -z notext -g
endif
ifeq ($(UNAME_S),Darwin) # for mac
	BIN_FORMAT = macho64
	CFLAGS ?= -Wl,-no_pie
endif

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
	find bbctests/ -type f -regex '.*\.\(o\|s\|run\|result\)' -delete
