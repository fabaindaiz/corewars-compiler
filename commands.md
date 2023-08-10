## Interactive execution
```bash
dune utop
```

#### red.ml
```bash
open Dev.Red;
```

#### compile.ml
```bash
open Dev.Ast;
open Dev.Compile;
```


## Test all
```bash
make test
make clean-test
```

## Compilation example
```bash
make compile src=examples/prog0.src
make compile src=examples/prog1.src
make compile src=examples/prog2.src
make compile src=examples/prog3.src
make compile src=examples/prog4.src
```
