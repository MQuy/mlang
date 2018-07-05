import { Token, TokenType } from "./token";
import {
  ClassStatement,
  FunctionStatement,
  VarExpression,
  VarStatement,
  WhileStatement,
  ExpressionStatement,
  IfStatement,
  ReturnStatement,
  BlockStatement,
  AssignExpression,
  GetExpression,
  SetExpression,
  LogicalExpression,
  BinaryExpression,
  UnaryExpression,
  CallExpression,
  LiteralExpression,
  SuperExpression,
  ThisExpression,
  GroupingExpression,
  PrintStatement
} from "./ast";
import { Statement } from "typescript";
import { Expression } from "./ast/base";

// program = (declaration)*
// declaration = classDeclaration | function | varDeclaration | statement
// classDeclaration = IDENTIFIER-name (LESS IDENTIFIER-super)? LEFT_BRACE (function-method)* RIGHT_BRACE
// statement = forStatement | ifStatement | printStatement | returnStatement | whileStatement | block | expressionStatement
// forStatement = LEFT_PAREN (varDeclaration | expressionStatement | SEMICOLON) expression? SEMICOLON expression? RIGHT_PAREN statement
// ifStatement = LEFT_PAREN expression RIGHT_PAREN statement (ELSE statement)?
// expression = assignment
// printStatement = expression SEMICOLON
// returnStatement = expression? SEMICOLON
// varDeclaration = IDENTIFIER (EQUAL expression)? SEMICOLON
// whileDeclaration = LEFT_PAREN expression RIGHT_PAREN statement
// expressionStatement = expression SEMICOLON
// function = IDENTIFIER-name LEFT_PAREN (IDENTIFIER-param COMMA)* RIGHT_PAREN block
// block = (declaration)* RIGHT_BRACE
// assignment = or (EQUAL or)?
// or = and (OR and)*
// and = equality (AND equality)*
// equality = comparison ((BANG_EQUAL | EQUAL_EQUAL) comparison)*
// comparison = addition ((GREATER | GREATER_EQUAL | LESS | LESS_EQUAL) addition)*
// addition = multiplication ((MINUS | PLUS) multiplication)*
// multiplication = unary ((SLASH | STAR) unary)*
// unary = ((BANG | MINUS) unary) | call
// call = primary ((LEFT_PAREN finish_call) | (DOT IDENTIFIER))*
// finish_call = (expression COMMA)* RIGHT_PAREN
// primary = TRUE | FALSE | NIL | NUMBER | STRING | SUPER | THIS | IDENTIFIER | LEFT_PAREN expression RIGHT_PAREN

export class Parser {
  tokens: Token[];
  current: number;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  parse() {
    this.current = 0;
    return this.program();
  }

  program() {
    const statements: Statement[] = [];

    while (this.isNotEnd()) {
      statements.push(this.declaration());
    }

    return statements;
  }

  declaration() {
    if (this.match(TokenType.CLASS)) {
      return this.classDeclaration();
    } else if (this.match(TokenType.FUN)) {
      return this.function("function");
    } else if (this.match(TokenType.VAR)) {
      return this.varDeclaration();
    } else {
      return this.statement();
    }
  }

  classDeclaration() {
    const name = this.consume(TokenType.IDENTIFIER);
    let superclass: VarExpression | undefined;
    let body: FunctionStatement[] = [];

    if (this.match(TokenType.LESS)) {
      superclass = new VarExpression(this.consume(TokenType.IDENTIFIER));
    }
    this.consume(TokenType.LEFT_BRACE);
    while (!this.match(TokenType.RIGHT_BRACE) && this.isNotEnd()) {
      body.push(this.function("method"));
    }
    return new ClassStatement(name, body, superclass);
  }

  function(_kind: string) {
    const name = this.consume(TokenType.IDENTIFIER);
    const parameters: Token[] = [];
    this.consume(TokenType.LEFT_PAREN);

    while (!this.match(TokenType.RIGHT_PAREN) && this.isNotEnd()) {
      parameters.push(this.consume(TokenType.IDENTIFIER));
    }

    this.consume(TokenType.LEFT_BRACE);
    const block = this.block();
    return new FunctionStatement(name, parameters, block);
  }

  block() {
    const statements: Statement[] = [];

    while (!this.match(TokenType.RIGHT_BRACE) && this.isNotEnd()) {
      statements.push(this.declaration());
    }
    return statements;
  }

  varDeclaration() {
    const name = this.consume(TokenType.IDENTIFIER);
    let initializer: Expression | undefined;

    if (this.match(TokenType.EQUAL)) {
      initializer = this.expression();
    }
    this.consume(TokenType.SEMICOLON);
    return new VarStatement(name, initializer);
  }

  expression() {
    return this.assignment();
  }

  statement() {
    if (this.match(TokenType.FOR)) {
      return this.forStatement();
    } else if (this.match(TokenType.IF)) {
      return this.ifStatement();
    } else if (this.match(TokenType.RETURN)) {
      return this.returnStatement();
    } else if (this.match(TokenType.WHILE)) {
      return this.whileStatement();
    } else if (this.match(TokenType.LEFT_BRACE)) {
      return new BlockStatement(this.block());
    } else if (this.match(TokenType.PRINT)) {
      return this.printStatement();
    } else {
      return this.expressionStatement();
    }
  }

  forStatement() {
    this.consume(TokenType.LEFT_PAREN);
    let initializer;
    let condition;
    let increment;
    if (this.match(TokenType.VAR)) {
      initializer = this.varDeclaration();
    } else if (this.match(TokenType.SEMICOLON)) {
      initializer = undefined;
    } else {
      initializer = this.expressionStatement();
    }
    if (!this.match(TokenType.SEMICOLON)) {
      condition = this.expression();
      this.consume(TokenType.SEMICOLON);
    }
    if (!this.match(TokenType.SEMICOLON)) {
      increment = this.expression();
      this.consume(TokenType.SEMICOLON);
    }
    this.consume(TokenType.RIGHT_PAREN);
    let block = this.statement();
    if (initializer) {
      block.shift(initializer);
    }
    if (increment) {
      block.push(increment);
    }
    return new WhileStatement(block, condition);
  }

  ifStatement() {
    this.consume(TokenType.LEFT_PAREN);
    const condition = this.expression();
    this.consume(TokenType.RIGHT_PAREN);

    const thenBranch = this.statement();
    let elseBranch: Statement | undefined;

    if (this.match(TokenType.ELSE)) {
      elseBranch = this.statement();
    }

    new IfStatement(condition, thenBranch, elseBranch);
  }

  whileStatement() {
    this.consume(TokenType.LEFT_PAREN);
    const condition = this.expression();
    this.consume(TokenType.RIGHT_PAREN);
    const statement = this.statement();

    return new WhileStatement(statement, condition);
  }

  returnStatement() {
    let expression: Expression | undefined;

    if (!this.match(TokenType.SEMICOLON)) {
      expression = this.expression();
    }

    this.consume(TokenType.SEMICOLON);
    return new ReturnStatement(expression);
  }

  expressionStatement() {
    const expression = this.expression();
    this.consume(TokenType.SEMICOLON);
    return new ExpressionStatement(expression);
  }

  printStatement() {
    const expression = this.expression();
    this.consume(TokenType.SEMICOLON);
    return new PrintStatement(expression);
  }

  assignment() {
    const or = this.or();

    if (this.match(TokenType.EQUAL)) {
      const assignment = this.assignment();

      if (or instanceof VarExpression) {
        return new AssignExpression(or.name, assignment);
      } else if (or instanceof GetExpression) {
        return new SetExpression(or.object, or.name, assignment);
      }
    }

    return or;
  }

  or() {
    let and = this.and();

    while (this.match(TokenType.OR)) {
      and = new LogicalExpression(and, this.previous(), this.or());
    }

    return and;
  }

  and() {
    let equality = this.equality();

    while (this.match(TokenType.AND)) {
      equality = new LogicalExpression(equality, this.previous(), this.and());
    }

    return equality;
  }

  equality() {
    let comparison = this.comparison();

    while (this.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
      comparison = new LogicalExpression(
        comparison,
        this.previous(),
        this.comparison()
      );
    }
    return comparison;
  }

  comparison() {
    let addition = this.addition();

    while (
      this.match(
        TokenType.GREATER,
        TokenType.GREATER_EQUAL,
        TokenType.LESS,
        TokenType.LESS_EQUAL
      )
    ) {
      addition = new LogicalExpression(
        addition,
        this.previous(),
        this.addition()
      );
    }

    return addition;
  }

  addition() {
    let multiplication = this.multiplication();

    while (this.match(TokenType.MINUS, TokenType.PLUS)) {
      multiplication = new BinaryExpression(
        multiplication,
        this.previous(),
        this.multiplication()
      );
    }

    return multiplication;
  }

  multiplication() {
    let unary = this.unary();

    while (this.match(TokenType.SLASH, TokenType.STAR)) {
      unary = new BinaryExpression(unary, this.previous(), this.unary());
    }

    return unary;
  }

  unary() {
    if (this.match(TokenType.BANG, TokenType.MINUS)) {
      return new UnaryExpression(this.previous(), this.unary());
    }

    return this.call();
  }

  call() {
    let primary = this.primary();

    while (true) {
      if (this.match(TokenType.LEFT_PAREN)) {
        primary = this.finishCall(primary);
      } else if (this.match(TokenType.DOT)) {
        const name = this.consume(TokenType.IDENTIFIER);
        primary = new GetExpression(primary, name);
      } else {
        break;
      }
    }

    return primary;
  }

  finishCall(token: Expression) {
    const args: Expression[] = [];

    while (!this.match(TokenType.RIGHT_PAREN)) {
      args.push(this.expression());
      this.consume(TokenType.COMMA);
    }
    return new CallExpression(token, args);
  }

  primary() {
    if (this.match(TokenType.FALSE)) return new LiteralExpression(false);
    if (this.match(TokenType.TRUE)) return new LiteralExpression(true);
    if (this.match(TokenType.NIL)) return new LiteralExpression(undefined);

    if (this.match(TokenType.NUMBER, TokenType.STRING)) {
      return new LiteralExpression(this.previous().liternal);
    }

    if (this.match(TokenType.SUPER)) {
      let keyword = this.previous();
      this.consume(TokenType.DOT);
      let method = this.consume(TokenType.IDENTIFIER);
      return new SuperExpression(keyword, method);
    }

    if (this.match(TokenType.THIS)) return new ThisExpression(this.previous());

    if (this.match(TokenType.IDENTIFIER)) {
      return new VarExpression(this.previous());
    }

    if (this.match(TokenType.LEFT_PAREN)) {
      let expr = this.expression();
      this.consume(TokenType.RIGHT_PAREN);
      return new GroupingExpression(expr);
    }

    throw new Error("Syntax error: " + this.peek());
  }

  match(...types: TokenType[]) {
    return types.some(type => {
      if (this.check(type)) {
        this.advance();
        return true;
      }
      return false;
    });
  }

  consume(type: TokenType) {
    if (this.check(type)) {
      return this.advance();
    }

    throw new Error("Wrong syntax");
  }

  check(type: TokenType) {
    if (this.isAtEnd()) return false;

    return this.peek().type === type;
  }

  peek() {
    return this.tokens[this.current];
  }

  advance() {
    if (!this.isAtEnd()) this.current++;

    return this.previous();
  }

  previous() {
    return this.tokens[this.current - 1];
  }

  isAtEnd() {
    return !this.isNotEnd();
  }

  isNotEnd() {
    return (
      this.current < this.tokens.length && this.peek().type !== TokenType.EOF
    );
  }
}
