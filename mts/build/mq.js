(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
  typeof define === 'function' && define.amd ? define(['exports'], factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.mts = {}));
}(this, (function (exports) { 'use strict';

  exports.TokenType = void 0;
  (function (TokenType) {
      TokenType["LEFT_PAREN"] = "LEFT_PAREN";
      TokenType["RIGHT_PAREN"] = "RIGHT_PAREN";
      TokenType["LEFT_BRACE"] = "LEFT_BRACE";
      TokenType["RIGHT_BRACE"] = "RIGHT_BRACE";
      TokenType["LEFT_BRACKET"] = "LEFT_BRACKET";
      TokenType["RIGHT_BRACKET"] = "RIGHT_BRACKET";
      TokenType["DEF"] = "DEF";
      TokenType["CLASS"] = "CLASS";
      TokenType["EXTENDS"] = "EXTENDS";
      TokenType["VAR"] = "VAR";
      TokenType["NEW"] = "NEW";
      TokenType["ARROW"] = "ARROW";
      TokenType["PRINT"] = "PRINT";
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
      constructor(type, lexeme, literal, line, column) {
          this.type = type;
          this.lexeme = lexeme;
          this.literal = literal;
          this.line = line;
          this.column = column;
      }
      toString() {
          return `Line ${this.line}:${this.column}`;
      }
  }

  const reservedWords = {
      def: exports.TokenType.DEF,
      class: exports.TokenType.CLASS,
      var: exports.TokenType.VAR,
      extends: exports.TokenType.EXTENDS,
      print: exports.TokenType.PRINT,
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
          this.column = 1;
          while (this.notAtEnd()) {
              this.current = this.runner;
              const token = this.scanToken();
              if (token)
                  tokens.push(token);
          }
          tokens.push(new Token(exports.TokenType.EOF, "", undefined, this.line, this.column));
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
                  this.newline();
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
          this.newline();
          return this.generateToken(exports.TokenType.COMMENT);
      }
      string() {
          while (this.peek() !== '"' && this.notAtEnd()) {
              if (this.peek() === "\n")
                  this.newline();
              this.advance();
          }
          if (!this.notAtEnd()) {
              throw new Error(`${this.line}: Unterminated string.`);
          }
          this.advance();
          return this.generateToken(exports.TokenType.STRING, this.getLexeme(this.current + 1, this.runner - 1));
      }
      number() {
          this.moveCursor(-1);
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
              else if (word === exports.TokenType.NULL) {
                  return this.generateToken(word, null);
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
          return new Token(type, this.getLexeme(), literal, this.line, this.column - (this.runner - this.current));
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
          this.moveCursor(1);
          return true;
      }
      newline() {
          this.line += 1;
          this.column = 1;
      }
      peek() {
          return this.source[this.runner];
      }
      advance() {
          return this.source[this.moveCursor(1)];
      }
      moveCursor(distance) {
          const current = this.runner;
          this.column += distance;
          this.runner += distance;
          return current;
      }
      notAtEnd() {
          return this.runner < this.source.length;
      }
  }

  class IRNode {
  }

  class IfStatement extends IRNode {
      constructor(condition, thenStatement, elseStatement) {
          super();
          this.condition = condition;
          this.thenStatement = thenStatement;
          this.elseStatement = elseStatement;
      }
      accept(visitor) {
          return visitor.visitIfStatement(this);
      }
  }
  class BlockStatement extends IRNode {
      constructor(statements) {
          super();
          this.statements = statements;
      }
      accept(visitor) {
          return visitor.visitBlockStatement(this);
      }
  }
  class BreakStatement extends IRNode {
      accept(visitor) {
          return visitor.visitBreakStatement(this);
      }
  }
  class ContinueStatement extends IRNode {
      accept(visitor) {
          return visitor.visitContinueStatement(this);
      }
  }
  class ForStatement extends IRNode {
      constructor(body, condition, intializer, increment) {
          super();
          this.body = body;
          this.condition = condition;
          this.initializer = intializer;
          this.increment = increment;
      }
      accept(visitor) {
          return visitor.visitForStatement(this);
      }
  }
  class VarStatement extends IRNode {
      constructor(name, initializer, type) {
          super();
          this.name = name;
          this.initializer = initializer;
          this.type = type;
      }
      accept(visitor) {
          return visitor.visitVarStatement(this);
      }
  }
  class VarsStatement extends IRNode {
      constructor(varStatements) {
          super();
          this.varStatements = varStatements;
      }
      accept(visitor) {
          return visitor.visitVarsStatements(this);
      }
  }
  class ClassStatement extends IRNode {
      constructor(name, properties, methods, superclass) {
          super();
          this.name = name;
          this.superclass = superclass;
          this.properties = properties;
          this.methods = methods;
      }
      accept(visitor) {
          return visitor.visitClassStatement(this);
      }
  }
  class FunctionStatement extends IRNode {
      constructor(name, parameters, body, returnType) {
          super();
          this.name = name;
          this.parameters = parameters;
          this.body = body;
          this.returnType = returnType;
      }
      accept(visitor) {
          return visitor.visitFunctionStatement(this);
      }
  }
  class ParameterDeclaration {
      constructor(name, type) {
          this.name = name;
          this.type = type;
      }
  }
  class ReturnStatement extends IRNode {
      constructor(value) {
          super();
          this.value = value;
      }
      accept(visitor) {
          return visitor.visitReturnStatement(this);
      }
  }
  class EmptyStatement extends IRNode {
      accept(visitor) {
          return undefined;
      }
  }
  class ExpressionStatement extends IRNode {
      constructor(expression) {
          super();
          this.expression = expression;
      }
      accept(visitor) {
          return visitor.visitExpressionStatement(this);
      }
  }
  class PrintStatement extends IRNode {
      constructor(expression) {
          super();
          this.expression = expression;
      }
      accept(visitor) {
          return visitor.visitPrintStatement(this);
      }
  }

  function error(token, errorMessage) {
      throw new Error(`Line ${token.line}:${token.column} ${errorMessage}`);
  }

  var BuiltinTypes;
  (function (BuiltinTypes) {
      BuiltinTypes["String"] = "String";
      BuiltinTypes["Number"] = "Number";
      BuiltinTypes["Null"] = "Null";
      BuiltinTypes["Boolean"] = "Boolean";
      BuiltinTypes["void"] = "void";
  })(BuiltinTypes || (BuiltinTypes = {}));
  class Functionable {
      constructor(returnType, name) {
          this.name = name;
          this.parameters = [];
          this.returnType = returnType;
      }
  }
  class Classable {
      constructor(name) {
          this.name = name;
          this.properties = {};
          this.methods = {};
      }
      get(name) {
          if (Object.keys(this.properties).includes(name)) {
              return this.properties[name];
          }
          else if (Object.keys(this.methods).includes(name)) {
              return this.methods[name];
          }
          else {
              throw new Error(`${name} is not defined`);
          }
      }
  }
  function checkTypeAssignable(type1, type2, position) {
      if (type1 === type2) {
          return true;
      }
      else if (type1 instanceof Classable && type2 instanceof Classable) {
          let superclass = type1.superclass;
          while (superclass) {
              if (superclass === type2) {
                  return true;
              }
              superclass = superclass.superclass;
          }
      }
      else if (Array.isArray(type1) &&
          Array.isArray(type2) &&
          type1.length === type2.length) {
          for (let i = 0; i < type1.length; ++i) {
              checkTypeAssignable(type1[i], type2[i], position);
          }
      }
      error(position, `Type '${getTypeName(type1)}' is not assignable to type '${getTypeName(type2)}'`);
  }
  function leastUpperBound(type1, type2) {
      if (type1 === type2) {
          return type1;
      }
      else if (type1 === BuiltinTypes.Null) {
          return type2;
      }
      else if (type2 === BuiltinTypes.Null) {
          return type1;
      }
      else if (type1 instanceof Classable && type2 instanceof Classable) {
          const type1Line = getInheritanceLine(type1);
          const type2Line = getInheritanceLine(type2);
          for (let i = 0; i < type1Line.length; ++i) {
              for (let j = 0; j < type2Line.length; ++j) {
                  if (type1Line[i] === type2Line[j]) {
                      return type1Line[i];
                  }
              }
          }
      }
  }
  function getInheritanceLine(type) {
      const line = [type];
      for (let trace = type.superclass; trace; trace = trace.superclass) {
          line.push(trace);
      }
      return line;
  }
  function getTypeName(type) {
      if (type instanceof Classable || type instanceof Functionable) {
          return type.name;
      }
      else {
          return type;
      }
  }

  class AssignmentExpression extends IRNode {
      constructor(name, expression) {
          super();
          this.name = name;
          this.expression = expression;
      }
      accept(visitor) {
          return visitor.visitAssignmentExpression(this);
      }
  }
  class LogicalExpression extends IRNode {
      constructor(left, operator, right) {
          super();
          this.left = left;
          this.operator = operator;
          this.right = right;
          this.type = BuiltinTypes.Boolean;
      }
      accept(visitor) {
          return visitor.visitLogicalExpression(this);
      }
  }
  class BinaryExpression extends IRNode {
      constructor(left, operator, right) {
          super();
          this.left = left;
          this.operator = operator;
          this.right = right;
      }
      accept(visitor) {
          return visitor.visitBinaryExpression(this);
      }
  }
  class UnaryExpression extends IRNode {
      constructor(operator, right) {
          super();
          this.operator = operator;
          this.right = right;
      }
      accept(visitor) {
          return visitor.visitUnaryExpression(this);
      }
  }
  class CallExpression extends IRNode {
      constructor(callee, args) {
          super();
          this.callee = callee;
          this.args = args;
      }
      accept(visitor) {
          return visitor.visitCallExpression(this);
      }
  }
  class SetExpression extends IRNode {
      constructor(object, name, value) {
          super();
          this.object = object;
          this.name = name;
          this.value = value;
      }
      accept(visitor) {
          visitor.visitSetExpression(this);
      }
  }
  class GetExpression extends IRNode {
      constructor(object, name) {
          super();
          this.object = object;
          this.name = name;
      }
      accept(visitor) {
          return visitor.visitGetExpression(this);
      }
  }
  class LiteralExpression extends IRNode {
      constructor(name) {
          super();
          this.name = name;
          const type = name.type.toLowerCase();
          this.type = (type.slice(0, 1).toUpperCase() + type.slice(1));
      }
      accept(visitor) {
          return visitor.visitLiteralExpression(this);
      }
  }
  class GroupExpression extends IRNode {
      constructor(expression) {
          super();
          this.expression = expression;
      }
      accept(visitor) {
          return visitor.visitGroupExpression(this);
      }
  }
  class LambdaExpression extends IRNode {
      constructor(parameters, body, returnType) {
          super();
          this.parameters = parameters;
          this.body = body;
          this.returnType = returnType;
      }
      accept(visitor) {
          return visitor.visitLambdaExpression(this);
      }
  }
  class NewExpression extends IRNode {
      constructor(name, args) {
          super();
          this.name = name;
          this.args = args;
      }
      accept(visitor) {
          return visitor.visitNewExpression(this);
      }
  }
  class ArrayExpression extends IRNode {
      constructor(elements) {
          super();
          this.elements = elements;
      }
      accept(visitor) {
          return visitor.visitArrayExpression(this);
      }
  }
  class ThisExpression extends IRNode {
      constructor(keyword) {
          super();
          this.keyword = keyword;
      }
      accept(visitor) {
          return visitor.visitThisExpression(this);
      }
  }
  class SuperExpression extends IRNode {
      constructor(keyword) {
          super();
          this.keyword = keyword;
      }
      accept(visitor) {
          return visitor.visitSuperExpression(this);
      }
  }
  class VarExpression extends IRNode {
      constructor(name) {
          super();
          this.name = name;
      }
      accept(visitor) {
          return visitor.visitVarExpression(this);
      }
  }

  class Program {
      constructor(statements) {
          this.statements = statements;
      }
  }

  class Parser {
      constructor(tokens) {
          this.tokens = tokens;
      }
      parse() {
          const statements = [];
          this.current = 0;
          while (this.notAtEnd()) {
              statements.push(this.statement());
          }
          return new Program(statements);
      }
      statement() {
          if (this.match(exports.TokenType.IF)) {
              return this.ifStatement();
          }
          else if (this.match(exports.TokenType.LEFT_BRACE)) {
              return this.blockStatement();
          }
          else if (this.match(exports.TokenType.BREAK)) {
              return this.breakStatement();
          }
          else if (this.match(exports.TokenType.CONTINUE)) {
              return this.continueStatement();
          }
          else if (this.match(exports.TokenType.FOR)) {
              return this.forStatement();
          }
          else if (this.match(exports.TokenType.WHILE)) {
              return this.whileStatement();
          }
          else if (this.match(exports.TokenType.VAR)) {
              return this.varsStatement();
          }
          else if (this.match(exports.TokenType.CLASS)) {
              return this.classStatement();
          }
          else if (this.match(exports.TokenType.RETURN)) {
              return this.returnStatement();
          }
          else if (this.match(exports.TokenType.PRINT)) {
              return this.printStatement();
          }
          else if (this.match(exports.TokenType.SEMICOLON)) {
              return this.emptyStatement();
          }
          else if (this.check(exports.TokenType.DEF) &&
              this.next().type === exports.TokenType.IDENTIFIER) {
              this.consume(exports.TokenType.DEF, "Expect def");
              return this.functionStatement();
          }
          else {
              return this.expressionStatement();
          }
      }
      ifStatement() {
          const ifToken = this.previous();
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after if");
          const condition = this.expression();
          this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after if condition");
          const thenStatement = this.statement();
          let elseStatement;
          if (this.match(exports.TokenType.ELSE)) {
              elseStatement = this.statement();
          }
          return this.generateStatement(new IfStatement(condition, thenStatement, elseStatement), ifToken);
      }
      blockStatement() {
          const blockToken = this.previous();
          const statements = [];
          while (!this.match(exports.TokenType.RIGHT_BRACE)) {
              statements.push(this.statement());
          }
          return this.generateStatement(new BlockStatement(statements), blockToken);
      }
      breakStatement() {
          const breakToken = this.previous();
          this.consume(exports.TokenType.SEMICOLON, "Expect ; after break");
          return this.generateStatement(new BreakStatement(), breakToken);
      }
      continueStatement() {
          const continueToken = this.previous();
          this.consume(exports.TokenType.SEMICOLON, "Expect ; after continue");
          return this.generateStatement(new ContinueStatement(), continueToken);
      }
      forStatement() {
          const forToken = this.previous();
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after for");
          let initializer = [];
          if (!this.match(exports.TokenType.SEMICOLON)) {
              if (this.match(exports.TokenType.VAR)) {
                  initializer = this.varStatement();
              }
              else if (this.check(exports.TokenType.IDENTIFIER)) {
                  initializer = [this.expression()];
                  while (this.match(exports.TokenType.COMMA)) {
                      initializer.push(this.expression());
                  }
                  this.consume(exports.TokenType.SEMICOLON, "Expect ; after while intializer");
              }
              else {
                  error(this.peek(), "Expect declaration or assignment");
              }
          }
          let condition;
          if (!this.match(exports.TokenType.SEMICOLON)) {
              condition = this.expression();
              this.consume(exports.TokenType.SEMICOLON, "Expect ; after condition");
          }
          let increment;
          if (!this.match(exports.TokenType.RIGHT_PAREN)) {
              const incrementExpression = this.expression();
              increment = this.generateStatement(new ExpressionStatement(incrementExpression), incrementExpression.pStart);
              this.consume(exports.TokenType.RIGHT_PAREN, "Expect )");
          }
          return this.generateStatement(new ForStatement(this.statement(), condition, initializer, increment), forToken);
      }
      whileStatement() {
          const whileToken = this.previous();
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after while");
          const condition = this.expression();
          this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after condition");
          return this.generateStatement(new ForStatement(this.statement(), condition), whileToken);
      }
      varsStatement() {
          const varToken = this.previous();
          return this.generateStatement(new VarsStatement(this.varStatement()), varToken);
      }
      varStatement() {
          const statements = [];
          while (this.notAtEnd()) {
              const varToken = this.previous().type === exports.TokenType.VAR ? this.previous() : this.peek();
              const name = this.consume(exports.TokenType.IDENTIFIER, "Expect identifier after var");
              let intializer;
              let type;
              if (this.match(exports.TokenType.COLON)) {
                  const kind = this.consume(exports.TokenType.IDENTIFIER, "Expect type after var");
                  type = kind.lexeme;
              }
              if (this.match(exports.TokenType.EQUAL)) {
                  intializer = this.expression();
              }
              statements.push(this.generateStatement(new VarStatement(name, intializer, type), varToken, this.peek().type === exports.TokenType.SEMICOLON
                  ? this.peek()
                  : this.previous()));
              if (this.match(exports.TokenType.COMMA)) {
                  continue;
              }
              else if (this.match(exports.TokenType.SEMICOLON)) {
                  break;
              }
              else {
                  error(this.peek(), "Expect ; after declaration");
              }
          }
          return statements;
      }
      classStatement() {
          const classToken = this.previous();
          const name = this.consume(exports.TokenType.IDENTIFIER, "Expect class name");
          let superclass;
          if (this.match(exports.TokenType.EXTENDS)) {
              superclass = this.consume(exports.TokenType.IDENTIFIER, "Expect super class name");
          }
          this.consume(exports.TokenType.LEFT_BRACE, "Expect {");
          let varStatements = [];
          let methodStatements = [];
          while (!this.match(exports.TokenType.RIGHT_BRACE)) {
              if (this.match(exports.TokenType.VAR)) {
                  varStatements = [...varStatements, ...this.varStatement()];
              }
              else if (this.match(exports.TokenType.DEF)) {
                  methodStatements = [...methodStatements, this.functionStatement()];
              }
              else {
                  error(this.peek(), "Expect class properties or methods");
              }
          }
          return this.generateStatement(new ClassStatement(name, varStatements, methodStatements, superclass), classToken);
      }
      functionStatement() {
          const functionToken = this.previous();
          const name = this.consume(exports.TokenType.IDENTIFIER, "Expect name");
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after name");
          const parameters = this.parameters();
          this.consume(exports.TokenType.COLON, "Expect return type");
          const kind = this.consume(exports.TokenType.IDENTIFIER, "Expect type after function");
          return this.generateStatement(new FunctionStatement(name, parameters, this.statement(), kind.lexeme), functionToken);
      }
      returnStatement() {
          const returnToken = this.previous();
          let expression;
          if (!this.match(exports.TokenType.SEMICOLON)) {
              expression = this.expression();
              this.consume(exports.TokenType.SEMICOLON, "Expect ; after return expression");
          }
          return this.generateStatement(new ReturnStatement(expression), returnToken);
      }
      printStatement() {
          const expression = this.expression();
          this.consume(exports.TokenType.SEMICOLON, "Expect ;");
          return this.generateStatement(new PrintStatement(expression), expression.pStart);
      }
      emptyStatement() {
          return this.generateStatement(new EmptyStatement(), this.previous());
      }
      expressionStatement() {
          const expression = this.expression();
          this.consume(exports.TokenType.SEMICOLON, "Expect ;");
          return this.generateStatement(new ExpressionStatement(expression), expression.pStart);
      }
      expression() {
          return this.assignmentExpression();
      }
      assignmentExpression() {
          let logical = this.logicalExpression();
          while (this.match(exports.TokenType.EQUAL)) {
              if (logical instanceof GetExpression) {
                  logical = this.generateExpression(new SetExpression(logical.object, logical.name, this.logicalExpression()), logical.pStart);
              }
              else if (logical instanceof VarExpression) {
                  logical = this.generateExpression(new AssignmentExpression(logical.name, this.logicalExpression()), logical.pStart);
              }
          }
          return logical;
      }
      logicalExpression() {
          let equality = this.equalityExpression();
          while (this.match(exports.TokenType.AND, exports.TokenType.OR)) {
              equality = this.generateExpression(new LogicalExpression(equality, this.previous(), this.equalityExpression()), equality.pStart);
          }
          return equality;
      }
      equalityExpression() {
          let compare = this.compareExpression();
          while (this.match(exports.TokenType.EQUAL_EQUAL, exports.TokenType.BANG_EQUAL)) {
              compare = this.generateExpression(new LogicalExpression(compare, this.previous(), this.compareExpression()), compare.pStart);
          }
          return compare;
      }
      compareExpression() {
          let addition = this.additionExpression();
          while (this.match(exports.TokenType.GREAT, exports.TokenType.GREAT_THAN, exports.TokenType.LESS, exports.TokenType.LESS_THAN)) {
              addition = this.generateExpression(new LogicalExpression(addition, this.previous(), this.additionExpression()), addition.pStart);
          }
          return addition;
      }
      additionExpression() {
          let multiplication = this.multiplicationExpression();
          while (this.match(exports.TokenType.PLUS, exports.TokenType.MINUS)) {
              multiplication = this.generateExpression(new BinaryExpression(multiplication, this.previous(), this.multiplicationExpression()), multiplication.pStart);
          }
          return multiplication;
      }
      multiplicationExpression() {
          let exponentiation = this.exponentiationExpression();
          while (this.match(exports.TokenType.STAR, exports.TokenType.SLASH)) {
              exponentiation = this.generateExpression(new BinaryExpression(exponentiation, this.previous(), this.exponentiationExpression()), exponentiation.pStart);
          }
          return exponentiation;
      }
      exponentiationExpression() {
          let unary = this.unaryExpression();
          while (this.match(exports.TokenType.STAR_STAR)) {
              unary = this.generateExpression(new BinaryExpression(unary, this.previous(), this.unaryExpression()), unary.pStart);
          }
          return unary;
      }
      unaryExpression() {
          if (this.match(exports.TokenType.PLUS, exports.TokenType.MINUS, exports.TokenType.PLUS_PLUS, exports.TokenType.MINUS_MINUS, exports.TokenType.BANG)) {
              const unaryToken = this.previous();
              return this.generateExpression(new UnaryExpression(this.previous(), this.callExpression()), unaryToken);
          }
          return this.callExpression();
      }
      callExpression() {
          let member = this.memberAccessExpression();
          while (this.match(exports.TokenType.LEFT_PAREN)) {
              const args = [];
              while (!this.match(exports.TokenType.RIGHT_PAREN)) {
                  args.push(this.expression());
                  while (this.match(exports.TokenType.COMMA)) {
                      args.push(this.expression());
                  }
              }
              member = this.generateExpression(new CallExpression(member, args), member.pStart);
          }
          return member;
      }
      memberAccessExpression() {
          let primary = this.primaryExpression();
          while (this.match(exports.TokenType.DOT)) {
              const name = this.consume(exports.TokenType.IDENTIFIER, "Expect identifer after .");
              primary = this.generateExpression(new GetExpression(primary, name), primary.pStart);
          }
          return primary;
      }
      primaryExpression() {
          if (this.match(exports.TokenType.BOOLEAN, exports.TokenType.NUMBER, exports.TokenType.STRING, exports.TokenType.NULL)) {
              const literalToken = this.previous();
              return this.generateExpression(new LiteralExpression(literalToken), literalToken);
          }
          else if (this.match(exports.TokenType.THIS)) {
              return this.generateExpression(new ThisExpression(this.previous()), this.previous());
          }
          else if (this.match(exports.TokenType.SUPER)) {
              return this.generateExpression(new SuperExpression(this.previous()), this.previous());
          }
          else if (this.match(exports.TokenType.LEFT_PAREN)) {
              const parenToken = this.previous();
              const expression = this.expression();
              const groupExpression = new GroupExpression(expression);
              this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after group");
              return this.generateExpression(groupExpression, parenToken);
          }
          else if (this.match(exports.TokenType.IDENTIFIER)) {
              return this.generateExpression(new VarExpression(this.previous()), this.previous());
          }
          else if (this.match(exports.TokenType.NEW)) {
              const newToken = this.previous();
              const name = this.consume(exports.TokenType.IDENTIFIER, "Expect class name after new");
              this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after class name");
              return this.generateExpression(new NewExpression(name, this.arguments()), newToken);
          }
          else if (this.match(exports.TokenType.LEFT_BRACKET)) {
              const arrayToken = this.previous();
              const elements = [];
              while (!this.match(exports.TokenType.RIGHT_BRACKET)) {
                  elements.push(this.expression());
                  while (this.match(exports.TokenType.COMMA)) {
                      elements.push(this.expression());
                  }
              }
              return this.generateExpression(new ArrayExpression(elements), arrayToken);
          }
          else if (this.match(exports.TokenType.DEF)) {
              const defToken = this.previous();
              this.consume(exports.TokenType.LEFT_PAREN, "Expect ( lambda");
              const paramenters = this.parameters();
              this.consume(exports.TokenType.COLON, "Expect : after lambda");
              const returnKind = this.consume(exports.TokenType.IDENTIFIER, "Expect lambda return type");
              this.consume(exports.TokenType.ARROW, "Expect => after lambda");
              return this.generateExpression(new LambdaExpression(paramenters, this.statement(), returnKind.lexeme), defToken);
          }
          return error(this.peek(), "Expect expression");
      }
      arguments() {
          const args = [];
          while (!this.match(exports.TokenType.RIGHT_PAREN)) {
              args.push(this.expression());
              while (this.match(exports.TokenType.COMMA)) {
                  args.push(this.expression());
              }
          }
          return args;
      }
      parameters() {
          const paramenters = [];
          while (!this.match(exports.TokenType.RIGHT_PAREN)) {
              paramenters.push(this.parameter());
              while (this.match(exports.TokenType.COMMA)) {
                  paramenters.push(this.parameter());
              }
          }
          return paramenters;
      }
      parameter() {
          const name = this.consume(exports.TokenType.IDENTIFIER, "Expect parameter name");
          this.consume(exports.TokenType.COLON, "Expect : after parameter name");
          const kind = this.consume(exports.TokenType.IDENTIFIER, "Expect type after parameter");
          return new ParameterDeclaration(name, kind.lexeme);
      }
      generateStatement(statement, start, end = this.previous()) {
          statement.pStart = { line: start.line, column: start.column };
          statement.pEnd = { line: end.line, column: end.column + end.lexeme.length };
          return statement;
      }
      generateExpression(expression, start, end = this.previous()) {
          expression.pStart = { line: start.line, column: start.column };
          expression.pEnd = {
              line: end.line,
              column: end.column + end.lexeme.length,
          };
          return expression;
      }
      notAtEnd() {
          return (this.current < this.tokens.length && this.peek().type !== exports.TokenType.EOF);
      }
      match(...types) {
          return types.some(type => {
              if (this.check(type)) {
                  this.advance();
                  return true;
              }
              return false;
          });
      }
      consume(type, errorMessage) {
          if (this.check(type)) {
              return this.advance();
          }
          return error(this.peek(), errorMessage);
      }
      check(type) {
          return this.notAtEnd() && this.peek().type === type;
      }
      advance() {
          return this.tokens[this.current++];
      }
      peek() {
          return this.tokens[this.current];
      }
      previous() {
          return this.tokens[this.current - 1];
      }
      next() {
          return this.tokens[this.current + 1];
      }
  }

  class SymbolTable {
      constructor(enclosing, symbols = {}) {
          this.enclosing = enclosing;
          this.symbols = symbols;
      }
      define(name, value) {
          this.symbols = { ...this.symbols, [name]: value };
      }
      lookup(name, depth = Number.MAX_SAFE_INTEGER) {
          let scope = this;
          if (name) {
              for (let i = 0; i <= depth && scope; ++i) {
                  if (Object.keys(scope.symbols).includes(name)) {
                      return scope.symbols[name];
                  }
                  scope = scope.enclosing;
              }
          }
          throw new Error(`Cannot find ${name}`);
      }
  }

  class TypeChecking {
      constructor(program, scope) {
          this.program = program;
          this.scope = scope || new SymbolTable(undefined, BuiltinTypes);
      }
      run() {
          this.program.statements.forEach(statement => this.execute(statement));
      }
      visitIfStatement(statement) {
          this.evaluate(statement.condition);
          this.execute(statement.thenStatement);
          this.execute(statement.elseStatement);
      }
      visitBlockStatement(statement) {
          this.beginScope();
          statement.statements.forEach(statement => this.execute(statement));
          this.endScope();
      }
      visitForStatement(statement) {
          this.beginScope();
          const initializers = statement.initializer || [];
          initializers.forEach(declaration => declaration instanceof VarStatement
              ? this.execute(declaration)
              : this.evaluate(declaration));
          this.evaluate(statement.condition);
          this.beginScope();
          this.execute(statement.body);
          this.endScope();
          this.execute(statement.increment);
          this.endScope();
      }
      visitVarStatement(statement) {
          this.checkVarStatement(statement, (name, type) => this.scope.define(name, type));
      }
      visitVarsStatements(statement) {
          statement.varStatements.forEach(varStatement => this.visitVarStatement(varStatement));
      }
      visitClassStatement(statement) {
          const klass = new Classable(statement.name.lexeme);
          this.scope.define(statement.name.lexeme, klass);
          this.beginScope();
          this.scope.define("this", klass);
          if (statement.superclass) {
              const superType = this.scope.lookup(statement.superclass.lexeme);
              if (superType instanceof Classable) {
                  klass.superclass = superType;
                  this.scope.define("super", klass.superclass);
              }
              else {
                  error(statement.superclass, "super has to be classname");
              }
          }
          if (statement.properties) {
              statement.properties.forEach(property => {
                  this.checkVarStatement(property, (name, type) => (klass.properties[name] = type));
              });
          }
          if (statement.methods) {
              statement.methods.forEach(method => {
                  this.checkFunctionStatement(method, (name, type) => (klass.methods[name] = type));
              });
          }
          this.endScope();
      }
      visitFunctionStatement(statement) {
          this.checkFunctionStatement(statement, (name, type) => this.scope.define(name, type));
      }
      visitExpressionStatement(statement) {
          this.evaluate(statement.expression);
      }
      visitReturnStatement(statement) {
          const kunction = this.scope.lookup("this");
          if (kunction instanceof Functionable) {
              const type = this.evaluate(statement.value);
              checkTypeAssignable(type, kunction.returnType, statement.pStart);
          }
          else {
              error(statement.pStart, "cannot return without function's scope");
          }
      }
      visitAssignmentExpression(expression) {
          const expressionType = this.evaluate(expression.expression);
          const variableType = this.scope.lookup(expression.name.lexeme);
          checkTypeAssignable(expressionType, variableType, expression.pStart);
          return variableType;
      }
      visitLogicalExpression(expression) {
          return expression.type;
      }
      visitBinaryExpression(expression) {
          const leftType = this.evaluate(expression.left);
          const rightType = this.evaluate(expression.right);
          if (leftType === BuiltinTypes.String &&
              rightType === BuiltinTypes.String &&
              expression.operator.type === exports.TokenType.PLUS) {
              return (expression.type = BuiltinTypes.String);
          }
          else if (leftType === BuiltinTypes.Number &&
              rightType === BuiltinTypes.Number) {
              return (expression.type = BuiltinTypes.Number);
          }
          else {
              error(expression.operator, "Only can do binary operator for number and string type");
          }
      }
      visitUnaryExpression(expression) {
          this.evaluate(expression.right);
          switch (expression.operator.type) {
              case exports.TokenType.BANG:
                  return (expression.type = BuiltinTypes.Boolean);
              case exports.TokenType.PLUS:
              case exports.TokenType.PLUS_PLUS:
              case exports.TokenType.MINUS:
              case exports.TokenType.MINUS_MINUS:
                  return (expression.type = BuiltinTypes.Number);
              default:
                  error(expression.operator, "Unknow operator");
          }
      }
      visitCallExpression(expression) {
          const type = this.evaluate(expression.callee);
          if (type instanceof Functionable) {
              type.parameters.forEach((parameterType, index) => {
                  const argType = this.evaluate(expression.args[index]);
                  checkTypeAssignable(argType, parameterType, expression.pStart);
              });
              return type.returnType;
          }
          else {
              error(expression.callee.pStart, "is not callable");
          }
      }
      visitGetExpression(expression) {
          const type = this.evaluate(expression.object);
          return type.get(expression.name.lexeme);
      }
      visitSetExpression(expression) {
          const object = this.evaluate(expression.object);
          if (object instanceof Classable) {
              const type = object.get(expression.name.lexeme);
              const setType = this.evaluate(expression.value);
              checkTypeAssignable(setType, type, expression.name);
              return type;
          }
          else {
              error(expression.name, "cannot set");
          }
      }
      visitLiteralExpression(expression) {
          return expression.type;
      }
      visitGroupExpression(expression) {
          this.evaluate(expression.expression);
          return (expression.type = expression.expression.type);
      }
      visitLambdaExpression(expression) {
          const kunction = new Functionable(this.scope.lookup(expression.returnType));
          expression.parameters.forEach(parameter => {
              const parameterType = this.scope.lookup(parameter.type);
              kunction.parameters.push(parameterType);
              this.scope.define(parameter.name.lexeme, parameterType);
          });
          this.beginScope();
          this.scope.define("this", kunction);
          this.execute(expression.body);
          this.endScope();
          return kunction;
      }
      visitNewExpression(expression) {
          return this.scope.lookup(expression.name.lexeme);
      }
      visitThisExpression(expression) {
          return this.scope.lookup(expression.keyword.lexeme);
      }
      visitSuperExpression(expression) {
          return this.scope.lookup(expression.keyword.lexeme);
      }
      visitVarExpression(expression) {
          return this.scope.lookup(expression.name.lexeme);
      }
      visitArrayExpression(expression) {
          if (expression.elements.length > 0) {
              let baseType = this.evaluate(expression.elements[0]);
              for (let i = 1; i < expression.elements.length; ++i) {
                  const otherType = this.evaluate(expression.elements[i]);
                  const tempType = leastUpperBound(otherType, baseType);
                  if (tempType) {
                      baseType = tempType;
                  }
                  else {
                      error(expression.elements[i].pStart, "Not same type");
                  }
              }
              return new Array(expression.elements.length).fill(baseType);
          }
          return [];
      }
      visitTupleExpression(expression) { }
      // NOTE: Don't need to check type for these statements
      visitPrintStatement(statement) { }
      visitBreakStatement(statement) { }
      visitContinueStatement(statement) { }
      execute(statement) {
          statement && statement.accept(this);
      }
      evaluate(expression) {
          return expression && expression.accept(this);
      }
      beginScope() {
          const enclosing = this.scope;
          this.scope = new SymbolTable(enclosing);
          return enclosing;
      }
      endScope() {
          if (this.scope.enclosing) {
              this.scope = this.scope.enclosing;
          }
      }
      checkVarStatement(statement, define) {
          const initializerType = this.evaluate(statement.initializer);
          if (statement.type) {
              const variableType = this.scope.lookup(statement.type);
              checkTypeAssignable(initializerType, variableType, statement.name);
              define(statement.name.lexeme, variableType);
          }
          else if (initializerType) {
              define(statement.name.lexeme, initializerType);
          }
          else {
              error(statement.name, "cannot declare without type");
          }
      }
      checkFunctionStatement(statement, define) {
          const kunction = new Functionable(this.scope.lookup(statement.returnType), statement.name.lexeme);
          statement.parameters.forEach(parameter => {
              const parameterType = this.scope.lookup(parameter.type);
              kunction.parameters.push(parameterType);
              this.scope.define(parameter.name.lexeme, parameterType);
          });
          define(statement.name.lexeme, kunction);
          this.beginScope();
          this.scope.define("this", kunction);
          this.execute(statement.body);
          this.endScope();
      }
  }

  const MAX_STACK = 500;
  class SymbolTable$1 {
      constructor(enclosing) {
          this.enclosing = enclosing;
          this.symbols = {};
      }
      define(name, value) {
          this.symbols[name] = value;
      }
      lookup(name, depth = MAX_STACK) {
          let scope = this;
          if (name) {
              for (let i = 0; i <= depth && scope; ++i) {
                  if (Object.keys(scope.symbols).includes(name)) {
                      return scope.symbols[name];
                  }
                  scope = scope.enclosing;
              }
          }
          throw new Error(`Cannot find ${name}`);
      }
      assign(name, value) {
          let scope = this;
          for (let i = 0; i <= MAX_STACK && scope; ++i) {
              if (Object.keys(scope.symbols).includes(name)) {
                  break;
              }
              else {
                  scope = scope.enclosing;
              }
          }
          if (scope) {
              scope.symbols[name] = value;
          }
          else {
              throw new Error(`Cannot find ${name}`);
          }
      }
  }

  class Classable$1 {
      constructor(name) {
          this.name = name;
          this.properties = {};
          this.methods = {};
      }
  }

  class BreakCall {
  }
  class ContinueCall {
  }
  class ReturnCall {
      constructor(value) {
          this.value = value;
      }
  }

  class Lambda {
      constructor(parameters, body, closure) {
          this.parameters = parameters;
          this.body = body;
          this.closure = closure;
      }
      invoke(interpreter, args) {
          const scope = new SymbolTable$1(this.closure);
          this.parameters.forEach((parameterName, index) => {
              scope.define(parameterName.name.lexeme, args[index]);
          });
          try {
              interpreter.executeFunctionBody(this.body, scope);
          }
          catch (e) {
              if (e instanceof ReturnCall) {
                  return e.value;
              }
              else {
                  throw e;
              }
          }
      }
  }

  class Functionable$1 extends Lambda {
      constructor(parameters, body, closure, name) {
          super(parameters, body, closure);
          this.name = name;
      }
  }

  class Instance {
      constructor(klass) {
          if (klass.superclass) {
              this.superInstance = new Instance(klass.superclass);
          }
          this.klass = klass;
          this.fields = Object.keys(klass.properties).reduce((acc, iter) => {
              acc[iter] = klass.properties[iter];
              return acc;
          }, {});
          this.methods = Object.keys(klass.methods).reduce((acc, iter) => {
              const klassMethod = klass.methods[iter];
              const scope = new SymbolTable$1(klassMethod.closure);
              const method = new Functionable$1(klassMethod.parameters, klassMethod.body, scope, klassMethod.name);
              scope.define("this", this);
              if (this.superInstance) {
                  scope.define("super", this.superInstance);
              }
              acc[method.name] = method;
              return acc;
          }, {});
      }
      get(name) {
          if (Object.keys(this.fields).includes(name)) {
              return this.fields[name];
          }
          else if (Object.keys(this.methods).includes(name)) {
              return this.methods[name];
          }
          else if (this.superInstance) {
              return this.superInstance.get(name);
          }
          throw new Error(`${this.klass.name} doesn't have ${name}`);
      }
      set(name, value) {
          if (Object.keys(this.fields).includes(name)) {
              this.fields[name] = value;
          }
          else if (this.superInstance) {
              this.superInstance.set(name, value);
          }
          else {
              throw new Error(`${this.klass.name} doesn't have property named ${name}`);
          }
      }
  }

  class Interpreter {
      constructor(program, scope) {
          this.evaluate = (expression) => {
              return expression ? expression.accept(this) : undefined;
          };
          this.execute = (statement) => {
              return statement ? statement.accept(this) : undefined;
          };
          this.program = program;
          this.scope = scope || new SymbolTable$1();
      }
      interpret() {
          this.program.statements.forEach(this.execute);
      }
      visitIfStatement(statement) {
          if (this.evaluate(statement.condition)) {
              return this.execute(statement.thenStatement);
          }
          else {
              return this.execute(statement.elseStatement);
          }
      }
      visitBlockStatement(statement) {
          this.beginScope();
          statement.statements.forEach(this.execute);
          this.endScope();
      }
      visitForStatement(statement) {
          this.beginScope();
          if (statement.initializer) {
              const initializers = statement.initializer || [];
              initializers.forEach(initializer => initializer instanceof VarStatement
                  ? this.execute(initializer)
                  : this.evaluate(initializer));
          }
          while (true) {
              if (!this.evaluate(statement.condition)) {
                  break;
              }
              try {
                  this.execute(statement.body);
              }
              catch (e) {
                  if (e instanceof BreakCall) {
                      break;
                  }
                  else if (e instanceof ContinueCall) {
                      continue;
                  }
                  else if (e instanceof ReturnCall) {
                      throw e;
                  }
              }
              this.execute(statement.increment);
          }
          this.endScope();
      }
      visitVarStatement(statement) {
          const value = this.evaluate(statement.initializer);
          this.scope.define(statement.name.lexeme, value);
      }
      visitVarsStatements(statement) {
          statement.varStatements.forEach(this.execute);
      }
      visitClassStatement(statement) {
          const klass = new Classable$1(statement.name.lexeme);
          this.scope.define(statement.name.lexeme, klass);
          this.beginScope();
          if (statement.superclass) {
              klass.superclass = this.scope.lookup(statement.superclass.lexeme);
          }
          if (statement.properties) {
              statement.properties.forEach(property => {
                  klass.properties[property.name.lexeme] = this.evaluate(property.initializer);
              });
          }
          if (statement.methods) {
              statement.methods.forEach(method => {
                  klass.methods[method.name.lexeme] = new Functionable$1(method.parameters, method.body, this.scope, method.name.lexeme);
              });
          }
          this.endScope();
          return klass;
      }
      visitFunctionStatement(statement) {
          const kunction = new Functionable$1(statement.parameters, statement.body, this.scope, statement.name.lexeme);
          this.scope.define(statement.name.lexeme, kunction);
          return kunction;
      }
      visitReturnStatement(statement) {
          const value = this.evaluate(statement.value);
          throw new ReturnCall(value);
      }
      visitBreakStatement(statement) {
          throw new BreakCall();
      }
      visitContinueStatement(statement) {
          throw new ContinueCall();
      }
      visitPrintStatement(statement) {
          const value = this.evaluate(statement.expression);
          console.log(value);
      }
      visitExpressionStatement(statement) {
          return this.evaluate(statement.expression);
      }
      visitAssignmentExpression(expression) {
          const value = this.evaluate(expression.expression);
          this.scope.assign(expression.name.lexeme, value);
          return value;
      }
      visitLogicalExpression(expression) {
          const left = this.evaluate(expression.left);
          const right = this.evaluate(expression.right);
          switch (expression.operator.type) {
              case exports.TokenType.AND:
                  return left && right;
              case exports.TokenType.OR:
                  return left || right;
              case exports.TokenType.EQUAL_EQUAL:
                  return left === right;
              case exports.TokenType.BANG_EQUAL:
                  return left !== right;
              case exports.TokenType.LESS:
                  return left < right;
              case exports.TokenType.LESS_THAN:
                  return left <= right;
              case exports.TokenType.GREAT:
                  return left > right;
              case exports.TokenType.GREAT_THAN:
                  return left >= right;
          }
      }
      visitBinaryExpression(expression) {
          const left = this.evaluate(expression.left);
          const right = this.evaluate(expression.right);
          switch (expression.operator.type) {
              case exports.TokenType.PLUS:
                  return left + right;
              case exports.TokenType.MINUS:
                  return left - right;
              case exports.TokenType.STAR:
                  return left * right;
              case exports.TokenType.SLASH:
                  return left / right;
              case exports.TokenType.STAR_STAR:
                  return left ** right;
          }
      }
      visitUnaryExpression(expression) {
          const value = this.evaluate(expression.right);
          switch (expression.operator.type) {
              case exports.TokenType.BANG:
                  return !value;
              case exports.TokenType.PLUS:
                  return +value;
              case exports.TokenType.MINUS:
                  return -value;
              case exports.TokenType.PLUS_PLUS:
              case exports.TokenType.MINUS_MINUS:
                  const newValue = expression.operator.type === exports.TokenType.PLUS_PLUS
                      ? value + 1
                      : value - 1;
                  if (expression.right instanceof VarExpression) {
                      this.scope.define(expression.right.name.lexeme, newValue);
                  }
                  else {
                      error(expression.right.pStart, "has to be variable name");
                  }
                  return newValue;
          }
      }
      visitCallExpression(expression) {
          const callee = this.evaluate(expression.callee);
          if (callee instanceof Functionable$1 || callee instanceof Lambda) {
              return callee.invoke(this, expression.args.map(this.evaluate));
          }
      }
      visitGetExpression(expression) {
          const instance = this.evaluate(expression.object);
          if (instance instanceof Instance) {
              return instance.get(expression.name.lexeme);
          }
          else {
              error(expression.pStart, "has to be instance");
          }
      }
      visitSetExpression(expression) {
          const instance = this.evaluate(expression.object);
          const value = this.evaluate(expression.value);
          if (instance instanceof Instance) {
              instance.set(expression.name.lexeme, value);
              return value;
          }
          else {
              error(expression.pStart, "has to be instance");
          }
      }
      visitLiteralExpression(expression) {
          return expression.name.literal;
      }
      visitGroupExpression(expression) {
          return this.evaluate(expression.expression);
      }
      visitNewExpression(expression) {
          const klass = this.scope.lookup(expression.name.lexeme);
          if (klass instanceof Classable$1) {
              return new Instance(klass);
          }
          else {
              error(expression.name, "has to be class name");
          }
      }
      visitLambdaExpression(expression) {
          return new Lambda(expression.parameters, expression.body, this.scope);
      }
      visitThisExpression(expression) {
          return this.scope.lookup("this");
      }
      visitSuperExpression(expression) {
          return this.scope.lookup("super");
      }
      visitVarExpression(expression) {
          return this.scope.lookup(expression.name.lexeme);
      }
      visitTupleExpression(expression) { }
      visitArrayExpression(expression) { }
      executeFunctionBody(statement, scope) {
          const currentScope = this.scope;
          try {
              this.scope = scope;
              statement.accept(this);
          }
          finally {
              this.scope = currentScope;
          }
      }
      beginScope() {
          const newScope = new SymbolTable$1(this.scope);
          const currentScope = this.scope;
          this.scope = newScope;
          return currentScope;
      }
      endScope() {
          if (this.scope.enclosing) {
              this.scope = this.scope.enclosing;
          }
          else {
              throw new Error("There is something wrong with scope");
          }
      }
  }

  exports.Interpreter = Interpreter;
  exports.Lexer = Lexer;
  exports.Parser = Parser;
  exports.Token = Token;
  exports.TypeChecking = TypeChecking;

  Object.defineProperty(exports, '__esModule', { value: true });

})));
