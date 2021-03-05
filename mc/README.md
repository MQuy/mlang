### Steps

- [ ] Preprocessing
  - [ ] Replace trigraph sequences (Optional)
  - [ ] Combine two physical lines when blashslash appears at the end of line (Optional)
  - [ ] Comments are replace by whitespace
  - [ ] Execute preprocessor ([Dave Prosser Algorithm](https://www.spinellis.gr/blog/20060626/))
  - [ ] Concatenate adjacent string literals
- [ ] Tokenization
- [ ] AST Construction

### Tokenization

```cpp
enum TokenName {
  // literal constant
  TK_INTEGER,
  TK_CHARACTER,
  TK_FLOAT,
  TK_ENUM,
  TK_STRING,
  // operator
  TK_PLUS,
  TK_MINUS,
  TK_STAR,
  TK_SLASH,
  // reversed words
  TK_AUTO,
  TK_REGISTER,
  TK_STATIC,
  TK_EXTERN,
  TK_TYPEDEF,
};


enum TokenType {
  TK_KEYWORD,
  TK_OPERATOR,
  TK_SPECIAL_SYMBOL,
  TK_IDENTIFIER,
  TK_CONSTANT,
  TK_STRING,
};

struct SourcePosition {
  int line;
  int column;
}

class Token {
  protected:
    enum TokenType type;
    struct SourcePosition start;
    struct SourcePosition end;
};

class TokenSymbol : Token {
  private:
    enum TokenName name;      // keyword, opeartor and special symbol
}

class TokenIdentifer : Token {
  private:
    std::string name;         // identifer
}

template<class T>
class TokenLiteral : Token {
  private:
    T value;                  // constant, string
}
```

### AST Construction

```cpp
enum class BuiltinTypeName { void, char, short, int, long, float, double, signed, unsigned };
enum class TypeQualifier { const, volatile, restrict };
enum class StorageSpecifier { auto, register, static, extern };

class Type {
  protected:
    int size;
    int align;

    Type *pointer_to;
    Type *array_of;

    std:set<TypeQualifier> qualifiers;
}

class BuiltinType : Type {
  private:
    BuiltinTypeName name;

}

class StructType : Type {
  private:
    std:string name;
    std:pair<std::optional<std:string>, Type> members;
}

class UnionType : Type {
  private:
    std:string name;
    std:pair<std::optional<std:string>, Type> members;
}

class EnumType : Type {
  private:
    std:string name;
    std:pair<std::optional<std:string>, int> members;
}

class FunctionType : Type {
  private:
    std:pair<std::optional<std:string>, Type> members;
    Type returning;
}

class ASTNode {

}

class ExpressionAST : ASTNode {
  protected:
    Type type;
}

class LiteralExpressionAST : ASTNode {

}

class VarExpressionAST : ExpressionAST {
  private:
    StorageSpecifier storage;
}

class BinaryExpressionAST {
  private:
    ExpressionAST left;
    ExpressionAST right;
    BinaryOperator operator;
}

class DotExpressionAST {
  private:
    VarExpressionAST name;
}
```
