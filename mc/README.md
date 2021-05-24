## mC (miniC)

mC is a toy C compiler that aims to C99-compliant, is written in C++ and use LLVM as backend.

### Internals

mC has 5 phases (try staying as close as possible to [C translation phases](https://en.cppreference.com/w/c/language/translation_phases)):

- Tokenize: decompose an input string into tokens.
- Preprocess: include header files, expand macros, and conditional compile.
- Parse: construct abstract syntax tree from the output of the preprocessed tokens.
- Semantic Analyze: add type for each AST node.
- Codegen: use LLVM to emit an object file.

### Features

- [x] Tokenization
  - [x] Replace trigraph sequences
  - [x] Combine two physical lines when blashslash appears at the end of line
  - [x] Tokenize
  - [x] Comments are replace by whitespace
- [ ] Preprocessor
  - [x] Preprocess ([Dave Prosser Algorithm](https://www.spinellis.gr/blog/20060626/))
  - [x] Concatenate adjacent string literals
  - [ ] Support #include_next
  - [ ] Support #pragma
- [x] Parser
  - [x] Abstract Syntax Tree
  - [x] Anonymous struct and union
  - [ ] Variable attributes
  - [ ] ASM
  - [ ] Old-style function definition
- [ ] Semantic Analyzer
  - [ ] Syntax and semantic validation
  - [ ] Incomplete types
  - [x] Type inference
- [ ] LLVM
  - [x] Unknown array size
  - [x] Nested array subscripts
  - [ ] Nested flat array initializer
  - [ ] Flexible array member
  - [x] Forward declaration
  - [ ] Function without a prototype
  - [x] Union member access
  - [ ] CVR qualified
    - [x] volatile
  - [x] Static declaration
  - [x] Codegen

### Setup

mC heavily depends on [Visual Studio](https://visualstudio.microsoft.com/)

- Follow [LLVM get started guide](https://llvm.org/docs/GettingStartedVS.html#getting-started) to install and build llvm project.
- open `mc.sln` to build the project.
