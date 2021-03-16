### Steps

- [ ] Preprocessing
  - [ ] Replace trigraph sequences (Optional)
  - [ ] Combine two physical lines when blashslash appears at the end of line (Optional)
  - [ ] Comments are replace by whitespace
  - [ ] Execute preprocessor ([Dave Prosser Algorithm](https://www.spinellis.gr/blog/20060626/))
  - [ ] Concatenate adjacent string literals
- [ ] Tokenization
- [ ] AST Construction

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
