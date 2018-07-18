(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
  typeof define === 'function' && define.amd ? define(['exports'], factory) :
  (factory((global.mq = {})));
}(this, (function (exports) { 'use strict';

  (function (TokenType) {
      TokenType["LEFT_PAREN"] = "LEFT_PAREN";
      TokenType["RIGHT_PAREN"] = "RIGHT_PAREN";
      TokenType["LEFT_BRACE"] = "LEFT_BRACE";
      TokenType["RIGHT_BRACE"] = "RIGHT_BRACE";
      TokenType["LEFT_BRACKET"] = "LEFT_BRACKET";
      TokenType["RIGHT_BRACKET"] = "RIGHT_BRACKET";
      TokenType["DEF"] = "DEF";
      TokenType["CLASS"] = "CLASS";
      TokenType["VAR"] = "VAR";
      TokenType["NEW"] = "NEW";
      TokenType["ARROW"] = "ARROW";
      TokenType["IF"] = "IF";
      TokenType["ELSE"] = "ELSE";
      TokenType["FOR"] = "FOR";
      TokenType["WHILE"] = "WHILE";
      TokenType["BREAK"] = "BREAK";
      TokenType["CONTINUE"] = "CONTINUE";
      TokenType["RETURN"] = "RETURN";
      TokenType["COMMA"] = "COMMA";
      TokenType["SEMICOLON"] = "SEMICOLON";
      TokenType["COLON"] = "COLON";
      TokenType["DOT"] = "DOT";
      TokenType["EQUAL"] = "EQUAL";
      TokenType["BANG_EQUAL"] = "BANG_EQUAL";
      TokenType["EQUAL_EQUAL"] = "EQUAL_EQUAL";
      TokenType["GREAT"] = "GREAT";
      TokenType["GREAT_THAN"] = "GREAT_THAN";
      TokenType["LESS"] = "LESS";
      TokenType["LESS_THAN"] = "LESS_THAN";
      TokenType["AND"] = "AND";
      TokenType["OR"] = "OR";
      TokenType["PLUS"] = "PLUS";
      TokenType["MINUS"] = "MINUS";
      TokenType["STAR"] = "STAR";
      TokenType["STAR_STAR"] = "STAR_STAR";
      TokenType["SLASH"] = "SLASH";
      TokenType["PLUS_PLUS"] = "PLUS_PLUS";
      TokenType["MINUS_MINUS"] = "MINUS_MINUS";
      TokenType["BANG"] = "BANG";
      TokenType["NUMBER"] = "NUMBER";
      TokenType["STRING"] = "STRING";
      TokenType["BOOLEAN"] = "BOOLEAN";
      TokenType["NULL"] = "NULL";
      TokenType["THIS"] = "THIS";
      TokenType["SUPER"] = "SUPER";
      TokenType["IDENTIFIER"] = "IDENTIFIER";
      TokenType["COMMENT"] = "COMMENT";
      TokenType["EOF"] = "EOF";
  })(exports.TokenType || (exports.TokenType = {}));
  class Token {
      constructor(type, lexeme, literal, line) {
          this.type = type;
          this.lexeme = lexeme;
          this.literal = literal;
          this.line = line;
      }
  }

  const reservedWords = {
      def: exports.TokenType.DEF,
      class: exports.TokenType.CLASS,
      var: exports.TokenType.VAR,
      new: exports.TokenType.NEW,
      if: exports.TokenType.IF,
      else: exports.TokenType.ELSE,
      for: exports.TokenType.FOR,
      while: exports.TokenType.WHILE,
      break: exports.TokenType.BREAK,
      continue: exports.TokenType.CONTINUE,
      return: exports.TokenType.RETURN,
      and: exports.TokenType.AND,
      or: exports.TokenType.OR,
      null: exports.TokenType.NULL,
      this: exports.TokenType.THIS,
      super: exports.TokenType.SUPER,
      true: exports.TokenType.BOOLEAN,
      false: exports.TokenType.BOOLEAN,
  };
  class Lexer {
      constructor(source) {
          this.source = source;
      }
      scan() {
          const tokens = [];
          this.runner = 0;
          this.line = 1;
          while (this.notAtEnd()) {
              this.current = this.runner;
              const token = this.scanToken();
              if (token)
                  tokens.push(token);
          }
          tokens.push(new Token(exports.TokenType.EOF, ""));
          return tokens;
      }
      scanToken() {
          const char = this.advance();
          switch (char) {
              case "{":
                  return this.generateToken(exports.TokenType.LEFT_BRACE);
              case "}":
                  return this.generateToken(exports.TokenType.RIGHT_BRACE);
              case "(":
                  return this.generateToken(exports.TokenType.LEFT_PAREN);
              case ")":
                  return this.generateToken(exports.TokenType.RIGHT_PAREN);
              case "[":
                  return this.generateToken(exports.TokenType.LEFT_BRACKET);
              case "]":
                  return this.generateToken(exports.TokenType.RIGHT_BRACKET);
              case ".":
                  return this.generateToken(exports.TokenType.DOT);
              case ";":
                  return this.generateToken(exports.TokenType.SEMICOLON);
              case ",":
                  return this.generateToken(exports.TokenType.COMMA);
              case ":":
                  return this.generateToken(exports.TokenType.COLON);
              case "/":
                  if (this.match("/")) {
                      return this.comment();
                  }
                  else {
                      return this.generateToken(exports.TokenType.SLASH);
                  }
              case "*":
                  if (this.match("*")) {
                      return this.generateToken(exports.TokenType.STAR_STAR);
                  }
                  else {
                      return this.generateToken(exports.TokenType.STAR);
                  }
              case "+":
                  if (this.match("+")) {
                      return this.generateToken(exports.TokenType.PLUS_PLUS);
                  }
                  else {
                      return this.generateToken(exports.TokenType.PLUS);
                  }
              case "-":
                  if (this.match("-")) {
                      return this.generateToken(exports.TokenType.MINUS_MINUS);
                  }
                  else {
                      return this.generateToken(exports.TokenType.MINUS);
                  }
              case "=":
                  if (this.match("=")) {
                      return this.generateToken(exports.TokenType.EQUAL_EQUAL);
                  }
                  else if (this.match(">")) {
                      return this.generateToken(exports.TokenType.ARROW);
                  }
                  else {
                      return this.generateToken(exports.TokenType.EQUAL);
                  }
              case "!":
                  if (this.match("=")) {
                      return this.generateToken(exports.TokenType.BANG_EQUAL);
                  }
                  else {
                      return this.generateToken(exports.TokenType.BANG);
                  }
              case ">":
                  if (this.match("=")) {
                      return this.generateToken(exports.TokenType.GREAT_THAN);
                  }
                  else {
                      return this.generateToken(exports.TokenType.GREAT);
                  }
              case "<":
                  if (this.match("=")) {
                      return this.generateToken(exports.TokenType.LESS_THAN);
                  }
                  else {
                      return this.generateToken(exports.TokenType.LESS);
                  }
              case "\n":
                  this.line += 1;
                  return;
              case '"':
                  return this.string();
              default:
                  if (this.isDigit(char)) {
                      return this.number();
                  }
                  else if (this.isAlpha(char)) {
                      return this.identifier();
                  }
                  else if (/^\s$/.test(char)) {
                      return;
                  }
                  else {
                      throw new Error(`${this.line}: Unexpected character ${char}.`);
                  }
          }
      }
      comment() {
          while (this.peek() !== "\n" && this.notAtEnd())
              this.advance();
          return this.generateToken(exports.TokenType.COMMENT);
      }
      string() {
          while (this.peek() !== '"' && this.notAtEnd()) {
              if (this.peek() === "\n")
                  this.line += 1;
              this.advance();
          }
          if (!this.notAtEnd()) {
              throw new Error(`${this.line}: Unterminated string.`);
          }
          this.advance();
          return this.generateToken(exports.TokenType.STRING, this.getLexeme(this.current + 1, this.runner - 1));
      }
      number() {
          this.runner -= 1;
          while (/^[0-9]$/.test(this.peek()) && this.notAtEnd())
              this.advance();
          if (this.match(".")) {
              while (/[0-9]/.test(this.peek()) && this.notAtEnd())
                  this.advance();
          }
          return this.generateToken(exports.TokenType.NUMBER, parseFloat(this.getLexeme()));
      }
      identifier() {
          while (/^\w$/.test(this.peek()) && this.notAtEnd())
              this.advance();
          const lexeme = this.getLexeme();
          if (Object.keys(reservedWords).includes(lexeme)) {
              const word = reservedWords[lexeme];
              if (word === exports.TokenType.BOOLEAN) {
                  return this.generateToken(word, lexeme === "true");
              }
              else {
                  return this.generateToken(word);
              }
          }
          else {
              return this.generateToken(exports.TokenType.IDENTIFIER);
          }
      }
      generateToken(type, literal) {
          return new Token(type, this.getLexeme(), literal, this.line);
      }
      getLexeme(start = this.current, end = this.runner) {
          return this.source.substring(start, end);
      }
      isDigit(char) {
          return /^[0-9]$/.test(char);
      }
      isAlpha(char) {
          return /^\w$/.test(char);
      }
      match(char) {
          if (this.peek() !== char)
              return false;
          this.runner += 1;
          return true;
      }
      peek() {
          return this.source[this.runner];
      }
      advance() {
          return this.source[this.runner++];
      }
      notAtEnd() {
          return this.runner < this.source.length;
      }
  }

  exports.Token = Token;
  exports.Lexer = Lexer;

  Object.defineProperty(exports, '__esModule', { value: true });

})));
