# corewars-compiler
#### By fabaindaiz

This software aims to be an easier way to write and optimize code for corewars. corewars-compiler uses its own mini functional language called RED which can then be compiled into optimized redcode.

#### work in progress.


### Instructions of use
- See REFERENCE.md for develop and run.
- See docs/manual/ for the user manual: getting started, a tutorial, a cookbook of classic warriors and the tools.
- See LANGUAGE.md for RED language reference.
- See docs/semantics.md for what each construct means and what the compiler preserves.
- See docs/roadmap.md for known defects and planned work, and docs/references.md for the research behind them.
- Working with an AI assistant: AGENTS.md is its entry point (CLAUDE.md imports it).

#### Intepreters

To execute the resulting redcode you can use one of these redcode interpreters.

- [pMARS](https://corewar.co.uk/pmars.htm)
- [other MARS implementations](https://corewar.co.uk/mars.htm)
- [python MARS](https://github.com/rodrigosetti/corewar)

#### TODO

- Create a RED language tutorial
- Improve control flow instructions and conditions
- Reduce duplicate code and improve compiled redcode

## Acknowledgements

- Pleiad for [BBCTester](https://github.com/pleiad/BBCTester), through its fork [BBCStepTester](https://github.com/fabaindaiz/BBCStepTester), which the tests use


## References

#### Getting started

- [the beginners' guide](https://corewar.co.uk/karonen/guide.htm)
- [my first corewars book](https://www.corewars.org/docs/book1.html)

#### Redcode learning

- [corewars tips & tricks](https://www.corewars.org/docs/tips.html)

#### Redcode wariors

- [redcode warriors](https://github.com/n1LS/redcode-warriors)
- [warriors sorted by type](http://moscova.inria.fr/~doligez/corewar/by-types/idx.htm)
- [warriors sorted by name](http://moscova.inria.fr/~doligez/corewar/by-name/complete.htm)

#### Redcode reference

- [1994 Core War Standard](https://corewar.co.uk/standards/icws94.txt)
- [REDCODE REFERENCE](https://corewa.rs/reference/pmars-redcode-94.txt)
- [ICWS94 validate](http://www.koth.org/planar/post/Validate1.1R.txt)

#### Corewars koth

- [KotH](http://www.koth.org/koth.html)
- [SAL hills](https://sal.discontinuity.info)
- [hills](https://corewar.co.uk/hills.htm)
