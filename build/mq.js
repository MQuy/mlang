(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
  typeof define === 'function' && define.amd ? define(['exports'], factory) :
  (factory((global.mqlang = {})));
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
      TokenType["EXTENDS"] = "EXTENDS";
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
      toString() {
          return `Line ${this.line}:`;
      }
  }

  const reservedWords = {
      def: exports.TokenType.DEF,
      class: exports.TokenType.CLASS,
      var: exports.TokenType.VAR,
      extends: exports.TokenType.EXTENDS,
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
          tokens.push(new Token(exports.TokenType.EOF, "", undefined, this.line));
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

  class IfStatement {
      constructor(condition, thenStatement, elseStatement) {
          this.condition = condition;
          this.thenStatement = thenStatement;
          this.elseStatement = elseStatement;
      }
      accept(visitor) {
          visitor.visitIfStatement(this);
      }
  }
  class BlockStatement {
      constructor(statements) {
          this.statements = statements;
      }
      accept(visitor) {
          visitor.visitBlockStatement(this);
      }
  }
  class BreakStatement {
      accept(visitor) {
          visitor.visitBreakStatement(this);
      }
  }
  class ContinueStatement {
      accept(visitor) {
          visitor.visitContinueStatement(this);
      }
  }
  class ForStatement {
      constructor(body, condition, intializer, increment) {
          this.body = body;
          this.condition = condition;
          this.initializer = intializer;
          this.increment = increment;
      }
      accept(visitor) {
          visitor.visitForStatement(this);
      }
  }
  class VarStatement {
      constructor(name, initializer) {
          this.name = name;
          this.initializer = initializer;
      }
      accept(visitor) {
          visitor.visitVarStatement(this);
      }
  }
  class VarsStatement {
      constructor(varStatements) {
          this.varStatements = varStatements;
      }
      accept(visitor) {
          visitor.visitVarsStatements(this);
      }
  }
  class ClassStatement {
      constructor(name, properties, methods, superclass) {
          this.name = name;
          this.supercase = this.supercase;
          this.properties = properties;
          this.methods = methods;
      }
      accept(visitor) {
          visitor.visitClassStatement(this);
      }
  }
  class FunctionStatement {
      constructor(name, parameters, body) {
          this.name = name;
          this.parameters = parameters;
          this.body = body;
      }
      accept(visitor) {
          visitor.visitFunctionStatement(this);
      }
  }
  class ReturnStatement {
      constructor(value) {
          this.value = value;
      }
      accept(visitor) {
          visitor.visitReturnStatement(this);
      }
  }
  class EmptyStatement {
      accept(visitor) { }
  }
  class ExpressionStatement {
      constructor(expression) {
          this.expression = expression;
      }
      accept(visitor) {
          visitor.visitExpressionStatement(this);
      }
  }

  class AssignmentExpression {
      constructor(object, expression) {
          this.object = object;
      }
      accept(visitor) {
          return visitor.visitAssignmentExpression(this);
      }
  }
  class LogicalExpression {
      constructor(left, operator, right) {
          this.left = left;
          this.operator = operator;
          this.right = right;
      }
      accept(visitor) {
          return visitor.visitLogicalExpression(this);
      }
  }
  class BinaryExpression {
      constructor(left, operator, right) {
          this.left = left;
          this.operator = operator;
          this.right = right;
      }
      accept(visitor) {
          return visitor.visitBinaryExpression(this);
      }
  }
  class UnaryExpression {
      constructor(operator, right) {
          this.operator = operator;
          this.right = right;
      }
      accept(visitor) {
          return visitor.visitUnaryExpression(this);
      }
  }
  class CallExpression {
      constructor(callee, args) {
          this.callee = callee;
          this.args = args;
      }
      accept(visitor) {
          return visitor.visitCallExpression(this);
      }
  }
  class SetExpression {
      constructor(object, name, value) {
          this.object = object;
          this.name = name;
          this.value = value;
      }
      accept(visitor) {
          visitor.visitSetExpression(this);
      }
  }
  class GetExpression {
      constructor(object, name) {
          this.object = object;
          this.name = name;
      }
      accept(visitor) {
          return visitor.visitGetExpression(this);
      }
  }
  class LiteralExpression {
      constructor(name) {
          this.name = name;
      }
      accept(visitor) {
          return visitor.visitLiteralExpression(this);
      }
  }
  class GroupExpression {
      constructor(expression) {
          this.expression = expression;
      }
      accept(visitor) {
          return visitor.visitGroupExpression(this);
      }
  }
  class LambdaExpression {
      constructor(parameters, body) {
          this.parameters = parameters;
          this.body = body;
      }
      accept(visitor) {
          return visitor.visitLambdaExpression(this);
      }
  }
  class NewExpression {
      constructor(name, args) {
          this.name = name;
          this.args = args;
      }
      accept(visitor) {
          return visitor.visitNewExpression(this);
      }
  }
  class ArrayExpression {
      constructor(elements) {
          this.elements = elements;
      }
      accept(visitor) {
          return visitor.visitArrayExpression(this);
      }
  }
  class ThisExpression {
      constructor(keyword) {
          this.keyword = keyword;
      }
      accept(visitor) {
          return visitor.visitThisExpression(this);
      }
  }
  class SuperExpression {
      constructor(keyword) {
          this.keyword = keyword;
      }
      accept(visitor) {
          return visitor.visitSuperExpression(this);
      }
  }
  class VarExpression {
      constructor(name) {
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
              return new VarsStatement(this.varStatement());
          }
          else if (this.match(exports.TokenType.CLASS)) {
              return this.classStatement();
          }
          else if (this.match(exports.TokenType.RETURN)) {
              return this.returnStatement();
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
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after if");
          const condition = this.expression();
          this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after if condition");
          const thenStatement = this.statement();
          let elseStatement;
          if (this.match(exports.TokenType.ELSE)) {
              elseStatement = this.statement();
          }
          return new IfStatement(condition, thenStatement, elseStatement);
      }
      blockStatement() {
          const statements = [];
          while (!this.match(exports.TokenType.RIGHT_BRACE)) {
              statements.push(this.statement());
          }
          return new BlockStatement(statements);
      }
      breakStatement() {
          this.consume(exports.TokenType.SEMICOLON, "Expect ; after break");
          return new BreakStatement();
      }
      continueStatement() {
          this.consume(exports.TokenType.SEMICOLON, "Expect ; after continue");
          return new ContinueStatement();
      }
      forStatement() {
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after for");
          let initializer = [];
          if (!this.match(exports.TokenType.SEMICOLON)) {
              if (this.match(exports.TokenType.VAR)) {
                  initializer = this.varStatement();
              }
              else if (this.check(exports.TokenType.IDENTIFIER)) {
                  initializer = [this.expression()];
                  while (!this.match(exports.TokenType.COMMA)) {
                      initializer.push(this.expression());
                  }
                  this.consume(exports.TokenType.SEMICOLON, "Expect ; after while intializer");
              }
              else {
                  this.error(this.peek(), "Expect declaration or assignment");
              }
          }
          const condition = this.expression();
          this.consume(exports.TokenType.SEMICOLON, "Expect ; after condition");
          let increment;
          if (!this.match(exports.TokenType.RIGHT_PAREN)) {
              increment = new ExpressionStatement(this.expression());
          }
          return new ForStatement(this.statement(), condition, initializer, increment);
      }
      whileStatement() {
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after while");
          const condition = this.expression();
          this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after condition");
          return new ForStatement(this.statement(), condition);
      }
      varStatement() {
          const statements = [];
          while (this.notAtEnd()) {
              const name = this.consume(exports.TokenType.IDENTIFIER, "Expect identifier after var");
              let intializer;
              if (this.match(exports.TokenType.EQUAL)) {
                  intializer = this.expression();
              }
              statements.push(new VarStatement(name, intializer));
              if (this.match(exports.TokenType.COMMA)) {
                  continue;
              }
              else if (this.match(exports.TokenType.SEMICOLON)) {
                  break;
              }
              else {
                  this.error(this.previous(), "Expect ; after declaration");
              }
          }
          return statements;
      }
      classStatement() {
          const name = this.consume(exports.TokenType.IDENTIFIER, "Expect class name");
          let supercase;
          if (this.match(exports.TokenType.EXTENDS)) {
              supercase = this.consume(exports.TokenType.IDENTIFIER, "Expect super class name");
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
                  this.error(this.peek(), "Expect class properties or methods");
              }
          }
          return new ClassStatement(name, varStatements, methodStatements, supercase);
      }
      functionStatement() {
          const name = this.consume(exports.TokenType.IDENTIFIER, "Expect name");
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after name");
          const parameters = this.paramenters();
          this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after parameters");
          const body = this.statement();
          return new FunctionStatement(name, parameters, body);
      }
      returnStatement() {
          if (this.match(exports.TokenType.SEMICOLON)) {
              return new ReturnStatement();
          }
          return new ReturnStatement(this.expression());
      }
      emptyStatement() {
          return new EmptyStatement();
      }
      expressionStatement() {
          const expression = this.expression();
          this.consume(exports.TokenType.SEMICOLON, "Expect ;");
          return new ExpressionStatement(expression);
      }
      expression() {
          return this.assignmentExpression();
      }
      assignmentExpression() {
          let logical = this.logicalExpression();
          while (this.match(exports.TokenType.EQUAL)) {
              if (logical instanceof GetExpression) {
                  logical = new SetExpression(logical.object, logical.name, this.logicalExpression());
              }
              else if (logical instanceof VarExpression) {
                  logical = new AssignmentExpression(logical, this.logicalExpression());
              }
          }
          return logical;
      }
      logicalExpression() {
          let equality = this.equalityExpression();
          while (this.match(exports.TokenType.AND, exports.TokenType.OR)) {
              equality = new LogicalExpression(equality, this.previous(), this.equalityExpression());
          }
          return equality;
      }
      equalityExpression() {
          let compare = this.compareExpression();
          while (this.match(exports.TokenType.EQUAL_EQUAL, exports.TokenType.BANG_EQUAL)) {
              compare = new LogicalExpression(compare, this.previous(), this.compareExpression());
          }
          return compare;
      }
      compareExpression() {
          let addition = this.additionExpression();
          while (this.match(exports.TokenType.GREAT, exports.TokenType.GREAT_THAN, exports.TokenType.LESS, exports.TokenType.LESS_THAN)) {
              addition = new LogicalExpression(addition, this.previous(), this.additionExpression());
          }
          return addition;
      }
      additionExpression() {
          let multiplication = this.multiplicationExpression();
          while (this.match(exports.TokenType.PLUS, exports.TokenType.MINUS)) {
              multiplication = new BinaryExpression(multiplication, this.previous(), this.multiplicationExpression());
          }
          return multiplication;
      }
      multiplicationExpression() {
          let exponentiation = this.exponentiationExpression();
          while (this.match(exports.TokenType.STAR, exports.TokenType.SLASH)) {
              exponentiation = new BinaryExpression(exponentiation, this.previous(), this.exponentiationExpression());
          }
          return exponentiation;
      }
      exponentiationExpression() {
          let unary = this.unaryExpression();
          while (this.match(exports.TokenType.STAR_STAR)) {
              unary = new BinaryExpression(unary, this.previous(), this.unaryExpression());
          }
          return unary;
      }
      unaryExpression() {
          if (this.match(exports.TokenType.PLUS, exports.TokenType.MINUS, exports.TokenType.PLUS_PLUS, exports.TokenType.MINUS_MINUS, exports.TokenType.BANG)) {
              new UnaryExpression(this.previous(), this.callExpression());
          }
          return this.callExpression();
      }
      callExpression() {
          let member = this.memberAccessExpression();
          while (this.match(exports.TokenType.LEFT_PAREN)) {
              const args = [];
              args.push(this.expression());
              while (this.match(exports.TokenType.COMMA)) {
                  args.push(this.expression());
              }
              this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after arguments");
              member = new CallExpression(member, args);
          }
          return member;
      }
      memberAccessExpression() {
          let primary = this.primaryExpression();
          while (this.match(exports.TokenType.DOT)) {
              const name = this.consume(exports.TokenType.IDENTIFIER, "Expect identifer after .");
              primary = new GetExpression(primary, name);
          }
          return primary;
      }
      primaryExpression() {
          debugger;
          if (this.match(exports.TokenType.BOOLEAN, exports.TokenType.NUMBER, exports.TokenType.STRING, exports.TokenType.NULL)) {
              return new LiteralExpression(this.previous());
          }
          else if (this.match(exports.TokenType.THIS)) {
              return new ThisExpression(this.previous());
          }
          else if (this.match(exports.TokenType.SUPER)) {
              return new SuperExpression(this.previous());
          }
          else if (this.match(exports.TokenType.LEFT_PAREN)) {
              const expression = new GroupExpression(this.expression());
              this.consume(exports.TokenType.RIGHT_PAREN, "Expect ) after group");
              return expression;
          }
          else if (this.match(exports.TokenType.IDENTIFIER)) {
              return new VarExpression(this.previous());
          }
          else if (this.match(exports.TokenType.NEW)) {
              const name = this.consume(exports.TokenType.IDENTIFIER, "Expect class name after new");
              return new NewExpression(name, this.arguments());
          }
          else if (this.match(exports.TokenType.LEFT_BRACKET)) {
              const elements = [];
              while (!this.match(exports.TokenType.RIGHT_BRACKET)) {
                  elements.push(this.expression());
                  while (this.match(exports.TokenType.COMMA)) {
                      elements.push(this.expression());
                  }
              }
              return new ArrayExpression(elements);
          }
          else if (this.match(exports.TokenType.DEF)) {
              const paramenters = this.paramenters();
              this.consume(exports.TokenType.ARROW, "Expect => after lambda");
              return new LambdaExpression(paramenters, this.statement());
          }
          return this.error(this.previous(), "Expect expression");
      }
      arguments() {
          const args = [];
          this.consume(exports.TokenType.LEFT_PAREN, "Expect ( after identifier");
          while (!this.match(exports.TokenType.RIGHT_PAREN)) {
              args.push(this.expression());
              while (this.match(exports.TokenType.COMMA)) {
                  args.push(this.expression());
              }
          }
          return args;
      }
      paramenters() {
          const paramenters = [];
          this.consume(exports.TokenType.LEFT_PAREN, "Expect (");
          while (!this.match(exports.TokenType.RIGHT_PAREN)) {
              paramenters.push(this.consume(exports.TokenType.IDENTIFIER, "Expect identifier after ("));
              while (this.match(exports.TokenType.COMMA)) {
                  paramenters.push(this.consume(exports.TokenType.IDENTIFIER, "Expect identifer after ,"));
              }
          }
          return paramenters;
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
          return this.error(this.peek(), errorMessage);
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
      error(token, errorMessage) {
          throw new Error(`${token.toString()} ${errorMessage}`);
      }
  }

  exports.Token = Token;
  exports.Lexer = Lexer;
  exports.Parser = Parser;

  Object.defineProperty(exports, '__esModule', { value: true });

})));
