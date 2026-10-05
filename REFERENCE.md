# Requirements & Setup

To develop and run the compiler, you will need to use the following:

- [OCaml](https://ocaml.org/), version 4.12 (or newer), a programming language well-suited for implementing compilers (see below for the specific installation instructions).
- [Opam](https://opam.ocaml.org/doc/Install.html), version 2.0 (or newer), a package manager for ocaml libraries and tools.

In order to setup your ocaml environment, you should first [install opam](https://opam.ocaml.org/doc/Install.html), following the instructions for your distribution. Then create a switch with the right ocaml version and install the tools and libraries used in the course with the following invocations from the command line. 

A switch local to this repository also works, and keeps everything under `_opam/` (ignored by git):

```bash
opam init --bare -n
opam switch create . ocaml-base-compiler.5.5.1 --no-install
opam install --switch=. dune containers alcotest
opam exec --switch=. -- make check
```

Or the original course setup:

```bash
opam init
opam update
opam switch create compilation 5.0.0

# adapt according to your shell -- this is shown for bash
eval `opam env`
opam install dune utop merlin containers alcotest
```

The tests also need `bbctester`, from the [BBCStepTester](https://github.com/fabaindaiz/BBCStepTester) fork (pleiad/BBCTester has a different API under the same library name). It has no opam package; install it from source:

```bash
git clone https://github.com/fabaindaiz/BBCStepTester.git
cd BBCStepTester && git checkout 2cb3669   # the commit CI uses
dune build && dune install
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
- `bbctests/`: folder for black-box compiler tests (uses the bbctester library, see below); `bbctests/known-bugs/` records the current output of programs that trigger a known defect
- `behtests/`: behaviour specs, run by `tools/behave.py` (see below)
- `examples/`: folder for example source code files you may wish to interpret or compile directly
- `pmars/`: the pMARS simulator: a Linux x86-64 binary used by the tests, its configurations, and its source
- `tools/`: the repository's checks (`audit.py`, `behave.py`) and `pmars-host.sh`
- `docs/`: architecture, semantics, decisions, roadmap and references

Additionally, the root directory contains configuration files for the dune package manager (`dune-workspace`, `dune-project`), and each OCaml subdirectory also contains `dune` files in order to setup the project structure.

Dune will build everything inside the `_build/` directory.

## Makefile targets

The root directory contains a `Makefile` that provides shortcuts to build and test the project. These are mostly aliases to `dune`.

- `make init`: builds the project
  
- `make clean`: cleans everything (ie. removes the `_build/` directory)
  
- `make clean-tests`: cleans the tests output in the `bbctests` directory 

- `make bench`: score the archetypes (RED and hand-written) against the Wilkies benchmark and the top 20 of Koenigstuhl's 94nop hill, downloaded into `_build/bench/` on first use, and compare with `tools/bench_baseline.json` (`python3 tools/bench.py --update` rewrites it; `--hill` also places each warrior on the whole hill). Needs network; not part of `make check`.
- `make tests`: execute the tests for the compiler defined in `execs/run_test.ml` (see below).
  Variants include: 
  * `make ctests` for compact representation of the tests execution
  * you can also add `F=<pat>` where `<pat>` is a pattern to filter which test groups should be executed (eg. `make tests F=compare`; the groups are `parse`, `interp`, `errors`, `compare` and `execute`)
  * a few alcotest environment variable can also be set, e.g. `ALCOTEST_QUICK_TESTS=1 make tests` to only run the quick tests (see the help documentation of alcotest for more informations)

- `make check`: the whole gate. `make check-tools` runs the part that needs only Python 3.11+ and a C compiler (the structural audit, the behaviour specs, a check that the host pMARS survives `tools/pmars-trap/`, the agent-guides bundle checks); `make check-ocaml` builds and runs the OCaml tests

- you can build the executables manually with `make <executable_name>.exe`. For instance, `make run_compile.exe` builds the compiler executable.

- you can run the executables manually as follows:
  * `make compile src=examples/prog.src`: builds/runs the compiler on the source file `examples/prog.src`, outputs the generated redcode
  * `dune exec execs/run_compile.exe -- [--optimize o1,o2] [--report[=json]] [--expect=warn] [--warn=all|none] [--hill KEY] [--emit-beh FILE] <file>`: `--warn` gives all the cost warnings, or none, instead of those the policy's objectives ask for; `--hill` compiles for another hill (`94nop`, `94`, `94x`, `tiny`, `nano`) than the header's or 94b; `--report` prints the measured metrics and predictions on standard error (`=json`: JSON on standard output instead of the redcode); `--optimize` sets the policy; `--expect=warn` turns failed expectations into warnings; `--emit-beh FILE.beh` writes the execution expectations as a behaviour spec and the redcode beside it as `FILE.red` (see LANGUAGE.md). A compile error prints `file:line:column: error: ...` and exits with code 1 (an internal compiler error: code 2). The command line itself is `Cored.Driver` (`src/driver.ml`), tested in the `driver` group

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
 `dune exec execs/run_test.exe` potentially followed by `--` and arguments (for instance `dune exec execs/run_test.exe -- --help` to access the documentation).

There are a few example tests for the parser in `execs/run_test.ml`. *You need to add your additional OCaml tests to this file (or define them in an auxiliar file/module, and import the corresponding module and add your tests to the `ocaml_tests` variable).*


#### Black-box compiler tests

In order to test your whole compiler pipeline, from a source file down to the execution of the assembly file after linking with the runtime system, we provide a dedicated library: [BBCTester](https://github.com/pleiad/BBCTester).

You should follow the instructions from that repo to install `bbctester`, and look at the documentation for how to write `.bbc` files (to be placed in the `bbctests` directory).

Instead of the original BBCTester, this compiler uses its fork [BBCStepTester](https://github.com/fabaindaiz/BBCStepTester) (library name `bbctester`; installation above).

`run_test.exe` runs every `.bbc` file under `bbctests/` (recursively) in two groups:
- `compare`: the emitted redcode must equal `EXPECTED` byte for byte.
- `execute`: `pmars/pmars -A -@ pmars/config/94b.opt` must accept it. `-A` only assembles: nothing is executed. `pmars/pmars` is a Linux x86-64 binary, so this group runs on Linux only.

#### Behaviour specs

What compiled code *does* is checked by `python3 tools/behave.py`, which runs each `behtests/*.beh` spec in pMARS's debugger and checks probes such as "alive after N instructions" or "cell 1 holds `DAT.F #10, #10`". It builds a pMARS for your machine from `pmars/pmars-0.9.4.zip` on first use (`tools/pmars-host.sh`). See `docs/architecture.md` for the spec format.

## Interactive execution

Remember that to execute your code interactively, use `dune utop` in a terminal, and then load the modules you want to interact with (e.g. `open Cored.Compile;;`).

## Resources

Documentation for ocaml libraries:
- [containers](http://c-cube.github.io/ocaml-containers/last/) for extensions to the standard library
- [alcotest](https://mirage.github.io/alcotest/alcotest/index.html) for unit-tests
- [BBCStepTester](https://github.com/fabaindaiz/BBCStepTester) for blac-box compiler tests

## Acknowledgements

- Document based on [CC5116](https://users.dcc.uchile.cl/~etanter/CC5116/)
