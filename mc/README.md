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
  - [x] Anonymous struct and union
  - [ ] Variable attributes
- [ ] Semantic Analyzer
  - [ ] Syntax and semantic validation
  - [x] Type inference
- [ ] LLVM
  - [x] Unknown array size
  - [x] Nested array subscripts
  - [ ] Nested flat array initializer
  - [x] Forward declaration
  - [ ] Function without a prototype
  - [x] Union member access
  - [-] CVR qualified
    - [x] volatile
  - [x] Static declaration
  - [x] Codegen

✍🏼 mc is C99-compliant

### AST

![diagram](https://i.imgur.com/tqpvDdb.png)

- Not support old-style function definition

### Semantic Analyzer

- throw exceptions if there is any invalid expression
- validate syntax, semantic and flatten anonymous struct

### LLVM
