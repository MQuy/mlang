## Mini Haskell

### Overview

Mini Haskell is the dead simple functional language (in ML family). It supports basic fp features like:

- Currying
- Lazy evaluation
- Higher order functions
- Lambda lifting
- Pattern matching
- Type inference

**WIP features**:

- [ ] Proper type inference
- [ ] Indentation sensitive grammar
- [ ] Typeclasses
- [ ] Algebraic datatypes
- [ ] Type annotations
- [ ] Do-notation
- [ ] Monadic IO
- [ ] Where statements
- [ ] List and tuple sugar
- [ ] List comprehensions

### Setup & run

```bash
$ cabal build
$ ./dist/build/mhaskell/mhaskell run "examples/xxx.mhs"
```

### Example

```haskell
data Land = Utopia a | Lalaland a b;

main = let x = stay (Utopia 3) in x - 5;

stay (Lalaland x y) = x + y;
stay (Utopia x) = x * x;
```
