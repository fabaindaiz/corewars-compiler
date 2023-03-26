# Reference Code for Deliverable 1

Starter code for compilers homework, [deliverable 1](https://users.dcc.uchile.cl/~etanter/CC5116/hw_1_enunciado.html)

## Requirements & Setup

See the [detailed description](https://users.dcc.uchile.cl/~etanter/CC5116-2020/#(part._materials)) on the page of the course.

## Organization of the repository

The organization of the repository is as follows:

- `dev/`: main OCaml files for the project submodules (ast, parser, interpreter, asm instructions, compiler)
- `execs/`: OCaml files for top-level executables (interpreter, compiler, tester)
- `bbctests/`: folder for black-box compiler tests (uses the BBCTester library, see below)
- `examples/`: folder for example source code files you may wish to interpret or compile directly
- `rt/sys.c`: the runtime system implemented in C

Additionally, the root directory contains configuration files for the dune package manager (`dune-workspace`, `dune-project`), and each OCaml subdirectory also contains `dune` files in order to setup the project structure.

Dune will build everything inside the `_build/` directory.

## Makefile targets

The root directory contains a `Makefile` that provides shortcuts to build and test the project. These are mostly aliases to `dune`.

- `make init`: builds the project
  
- `make clean`: cleans everything (ie. removes the `_build/` directory)
  
- `make clean-tests`: cleans the tests output in the `bbctests` directory 

- `make test`: execute the tests for the compiler defined in `execs/test.ml` (see below).
  Variants include: 
  * `make ctest` for compact representation of the tests execution
  * you can also add `F=<pat>` where `<pat>` is a pattern to filter which tests should be executed (eg. `make test F=arith` to run only test files whose name contains `arith`)
  * a few alcotest environment variable can also be set, e.g. `ALCOTEST_QUICK_TESTS=1 make test` to only run the quick tests (see the help documentation of alcotest for more informations)

- you can build the executables manually with `make <executable_name>.exe`. For instance, `make run_compile.exe` builds the compiler executable.

- you can run the executables manually as follows:
  * `make interp src=examples/prog.src`: builds/runs the interpreter on the source file `examples/prog.src`, outputs the result
  * `make compile src=examples/prog.src`: builds/runs the compiler on the source file `examples/prog.src`, outputs the generated assembly code
  * `make compile-run src=examples/prog.src`: builds/runs the compiler on the source file `examples/prog.src`, generates the program binary (`examples/prog.run`), and runs it.

- you can also ask specific files to be built, eg.:
  * `make examples/prog.s`: looks up `examples/prog.src`, compiles it, and generates the assembly file `examples/prog.s`
  * `make examples/prog.o`: looks up `examples/prog.src`, compiles it, and generates the binary module `examples/prog.o` (unlinked)
  * `make examples/prog.run`: looks up `examples/prog.src`, compiles it, assembles, links, and generates the binary executable `examples/prog.run`

You can look at the makefile to see the underlying `dune` commands that are generated, and of course you can use `dune` directly if you find it more convenient.

## Writing tests

Tests are written using the [alcotest](https://github.com/mirage/alcotest) unit-testing framework. 

There are two categories of tests:
- OCaml tests: these are plain alcotests for testing your OCaml functions. 
- Black-box compiler tests: these are whole-pipeline tests for your compiler.

The executable `run_test.exe` first runs all OCaml tests, and then the black-box compiler tests.

#### OCaml tests

Alcotests executes a battery of unit-tests through the `run` function that takes a name (a string) and a list of items to be tested. 
Each such item is composed itself from an identifier (a string) together with a list of unit-test obtained with the `test_case` function.
`test_case` takes a description of the test, a mode (either ``` `Quick ``` or ``` `Slow ```---use ``` `Quick ``` by default) and the test itself as a function `unit -> unit`.

A test is built with the `check` function which takes the following parameters:
- a way to test results of type `result_type testable`,
- an error message to be displayed when the test fails,
- the program to be tested, and the expected value (both of type `result_type`)

Once written, tests can be executed with the relevant call to the Makefile (see above), or by calling
 `dune exec bin/tests.exe` potentially followed by `--` and arguments (for instance `dune exec bin/tests.exe -- --help` to access the documentation).

There are a few example tests for the parser and interpreter in `execs/run_test.ml`. *You need to add your additional OCaml tests to this file (or define them in an auxiliar file/module, and import the corresponding module and add your tests to the `ocaml_tests` variable).*


#### Black-box compiler tests

In order to test your whole compiler pipeline, from a source file down to the execution of the assembly file after linking with the runtime system, we provide a dedicated library: [BBCTester](https://github.com/pleiad/BBCTester).

You should follow the instructions from that repo to install `bbctester`, and look at the documentation for how to write `.bbc` files (to be placed in the `bbctests` directory).

## Interactive execution

Remember that to execute your code interactively, use `dune utop` in a terminal, and then load the modules you want to interact with (e.g. `open Dev.Interp;;`).

## Resources

Documentation for ocaml libraries:
- [containers](http://c-cube.github.io/ocaml-containers/last/) for extensions to the standard library
- [alcotest](https://mirage.github.io/alcotest/alcotest/index.html) for unit-tests
- [BBCTester](https://github.com/pleiad/BBCTester) for blac-box compiler tests

