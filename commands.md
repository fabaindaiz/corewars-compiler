## Interactive execution
```bash
dune utop
```

#### red.ml
```bash
open Cored.Red;;

```

#### compile.ml
```bash
open Cored.Ast;;
open Cored.Compile;;

```


## bbctests & examples
```bash
make tests
make clean-tests

make compile src=examples/prog0.src
```
