# Requirements & Setup

To develop and run the compiler, you will need to use the following:

- [OCaml](https://ocaml.org/), version 4.12 (or newer), a programming language well-suited for implementing compilers (see below for the specific installation instructions).
- [Opam](https://opam.ocaml.org/doc/Install.html), version 2.0 (or newer), a package manager for ocaml libraries and tools.

In order to setup your ocaml environment, you should first [install opam](https://opam.ocaml.org/doc/Install.html), following the instructions for your distribution. Then create a switch with the right ocaml version and install the tools and libraries used in the course with the following invocations from the command line. 

```bash
opam init
opam update
opam switch create compilation 5.0.0

# adapt according to your shell -- this is shown for bash
eval `opam env`
opam install dune utop merlin containers alcotest
```

A brief description of the installed tools and libraries:

- [dune](https://dune.build/), version 2.9 (or newer), a build manager for ocaml.
- [utop](https://github.com/ocaml-community/utop), a rich REPL (Run-Eval-Print-Loop) for ocaml with autocompletion and syntax coloring.
- [merlin](https://github.com/ocaml/merlin), provides contextual information on ocaml code to various IDEs.
- [containers](http://c-cube.github.io/ocaml-containers/), an extension to the standard library.
- [alcotest](https://github.com/mirage/alcotest), a simple and colourful unit test framework.

There is no specific IDE for OCaml. A time-tested solution is to use Emacs (with tuareg and merlin). I’m using the [OCaml Platform for VS Code](https://github.com/ocamllabs/vscode-ocaml-platform), which works pretty well and is under active development. There’s also some community-backed support for [IntelliJ](https://plugins.jetbrains.com/plugin/9440-reasonml), although I haven’t tried it.

For VS Code, you first need to install [OCaml LSP](https://github.com/ocaml/ocaml-lsp):

```bash
opam install ocaml-lsp-server
```

Then simply go to VS Code, lookup for the extension named VSCode OCaml Platform, and you should be good to go.

Hint for VS Code: run this in the VS Code integrated terminal for automatic rebuild when a file changes:

```bash
dune build --watch --terminal-persistence=clear-on-rebuild
```

We recommend using Linux or macOS, if possible. If you use Windows, then install the [Windows Subsystem for Linux](https://learn.microsoft.com/en-us/windows/wsl/install). Past experience from students with WSL indicates that:

- When installing opam with add-apt-repository, it’s also necessary to apt install gcc, binutils-dev, make and pkg-config, and

- Call opam init with the switch --disable-sandboxing, as [explained here](https://stackoverflow.com/questions/54987110/installing-ocaml-on-windows-10-using-wsl-ubuntu-problems-with-bwrap-bubblewr).

## Organization of the repository

The organization of the repository is as follows:

- `src/`: main OCaml files for the project submodules (ast, parser, red instructions, compiler)
- `execs/`: OCaml files for top-level executables (compiler, tester)
- `bbctests/`: folder for black-box compiler tests (uses the BBCTester library, see below)
- `examples/`: folder for example source code files you may wish to interpret or compile directly

Additionally, the root directory contains configuration files for the dune package manager (`dune-workspace`, `dune-project`), and each OCaml subdirectory also contains `dune` files in order to setup the project structure.

Dune will build everything inside the `_build/` directory.

## Makefile targets

The root directory contains a `Makefile` that provides shortcuts to build and test the project. These are mostly aliases to `dune`.

- `make init`: builds the project
  
- `make clean`: cleans everything (ie. removes the `_build/` directory)
  
- `make clean-tests`: cleans the tests output in the `bbctests` directory 

- `make tests`: execute the tests for the compiler defined in `execs/test.ml` (see below).
  Variants include: 
  * `make ctest` for compact representation of the tests execution
  * you can also add `F=<pat>` where `<pat>` is a pattern to filter which tests should be executed (eg. `make test F=arith` to run only test files whose name contains `arith`)
  * a few alcotest environment variable can also be set, e.g. `ALCOTEST_QUICK_TESTS=1 make test` to only run the quick tests (see the help documentation of alcotest for more informations)

- you can build the executables manually with `make <executable_name>.exe`. For instance, `make run_compile.exe` builds the compiler executable.

- you can run the executables manually as follows:
  * `make compile src=examples/prog.src`: builds/runs the compiler on the source file `examples/prog.src`, outputs the generated redcode

- you can also ask specific files to be built, eg.:
  * `make examples/prog.s`: looks up `examples/prog.src`, compiles it, and generates the redcode file `examples/prog.s`

You can look at the makefile to see the underlying `dune` commands that are generated, and of course you can use `dune` directly if you find it more convenient.

## Tests

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

Instead of using `bbctester`, in this compiler we will use [BBCStepTester](https://github.com/fabaindaiz/BBCStepTester)

## Interactive execution

Remember that to execute your code interactively, use `dune utop` in a terminal, and then load the modules you want to interact with (e.g. `open Dev.Interp;;`).

## Resources

Documentation for ocaml libraries:
- [containers](http://c-cube.github.io/ocaml-containers/last/) for extensions to the standard library
- [alcotest](https://mirage.github.io/alcotest/alcotest/index.html) for unit-tests
- [BBCStepTester](https://github.com/fabaindaiz/BBCStepTester) for blac-box compiler tests

## Acknowledgements

- Document based on [CC5116](https://users.dcc.uchile.cl/~etanter/CC5116/)
