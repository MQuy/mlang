### Steps

- [x] Tokenization
  - [x] Replace trigraph sequences
  - [x] Combine two physical lines when blashslash appears at the end of line
  - [x] Tokenize
  - [x] Comments are replace by whitespace
  - [ ] IEEE Standard
- [-] Preprocessor
  - [x] Preprocess ([Dave Prosser Algorithm](https://www.spinellis.gr/blog/20060626/))
  - [x] Concatenate adjacent string literals
  - [ ] Support #include_next
  - [ ] Support #pragma
- [x] Parser
  - [x] Abstract Syntax Tree
  - [ ] Semantic Analyzer
- [ ] LLVM
  - [ ] Unknown array size
  - [ ] Enum values
  - [ ] Nested flat array initializer
  - [ ] Forward declaration

✍🏼 mc is C99-compliant

### AST

![diagram](https://i.imgur.com/tqpvDdb.png)

### Semantic Analyzer

- throw exceptions if there is any invalid expression
- validate syntax, semantic and flatten anonymous struct

### LLVM
