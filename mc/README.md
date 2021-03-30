### Steps

- [x] Tokenization
  - [x] Replace trigraph sequences
  - [x] Combine two physical lines when blashslash appears at the end of line
  - [x] Tokenize
  - [x] Comments are replace by whitespace
- [-] Preprocessor
  - [x] Preprocess ([Dave Prosser Algorithm](https://www.spinellis.gr/blog/20060626/))
  - [x] Concatenate adjacent string literals
  - [ ] Support #include_next
  - [ ] Support #pragma
- [x] AST

✍🏼 mc is C99-compliant

### AST

![diagram](https://i.imgur.com/tqpvDdb.png)

### LLVM

```cpp
class TransitionUnit {
  private:
    std::vector<FunctionProtoStmtAST> protos;
    std::vector<Typedef> typedefs;
    std::vector<AggregateType> aggregates;
    std::vector<FunctionDefStmtAST> functions;
}

std::vector<BuiltinType> builtin_types;
```
