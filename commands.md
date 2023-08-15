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


## bbctests & examples
```bash
make tests
make clean-tests

make compile src=examples/prog0.src
```
