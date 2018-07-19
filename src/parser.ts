import {
  Statement,
  IfStatement,
  BlockStatement,
  BreakStatement,
  ContinueStatement,
  VarStatement,
  ForStatement,
  ExpressionStatement,
  ClassStatement,
  FunctionStatement,
  ReturnStatement,
  EmptyStatement,
  VarsStatement,
} from "./ast/statement";
import {
  Expression,
  AssignmentExpression,
  LogicalExpression,
  BinaryExpression,
  UnaryExpression,
  CallExpression,
  GetExpression,
  LiternalExpression,
  ThisExpression,
  SuperExpression,
  GroupExpression,
  NewExpression,
  ArrayExpression,
  LambdaExpression,
  VarExpression,
  SetExpression,
} from "./ast/expression";
import { Token, TokenType } from "./token";
import { Program } from "./ast/program";

export class Parser {
  tokens: Token[];
  current: number;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  parse() {
    const statements: Statement[] = [];

    this.tokens = [];
    this.current = 0;

    while (this.notAtEnd()) {
      statements.push(this.statement());
    }

    return new Program(statements);
  }

  statement(): Statement {
    const token = this.peek();

    switch (token.type) {
      case TokenType.IF:
        return this.ifStatement();
      case TokenType.LEFT_BRACE:
        return this.blockStatement();
      case TokenType.BREAK:
        return this.breakStatement();
      case TokenType.CONTINUE:
        return this.continueStatement();
      case TokenType.FOR:
        return this.forStatement();
      case TokenType.WHILE:
        return this.whileStatement();
      case TokenType.VAR:
        return new VarsStatement(this.varStatement());
      case TokenType.CLASS:
        return this.classStatement();
      case TokenType.RETURN:
        return this.returnStatement();
      case TokenType.SEMICOLON:
        return this.emptyStatement();
      default:
        if (
          this.check(TokenType.DEF) &&
          this.next().type === TokenType.IDENTIFIER
        ) {
          this.consume(TokenType.DEF, "Expect def");
          return this.functionStatement();
        }
        return this.expressionStatement();
    }
  }

  ifStatement() {
    this.consume(TokenType.LEFT_PAREN, "Expect ( after if");
    const condition = this.expression();
    this.consume(TokenType.RIGHT_PAREN, "Expect ) after if condition");

    const thenStatement = this.statement();
    let elseStatement: Statement | undefined;

    if (this.match(TokenType.ELSE)) {
      elseStatement = this.statement();
    }
    return new IfStatement(condition, thenStatement, elseStatement);
  }

  blockStatement() {
    const statements: Statement[] = [];

    while (!this.match(TokenType.RIGHT_BRACE)) {
      statements.push(this.statement());
    }
    return new BlockStatement(statements);
  }

  breakStatement() {
    this.consume(TokenType.SEMICOLON, "Expect ; after break");
    return new BreakStatement();
  }

  continueStatement() {
    this.consume(TokenType.SEMICOLON, "Expect ; after continue");
    return new ContinueStatement();
  }

  forStatement() {
    this.consume(TokenType.LEFT_PAREN, "Expect ( after for");

    let initializer: VarStatement[] | Expression[] = [];

    if (!this.match(TokenType.SEMICOLON)) {
      if (this.match(TokenType.VAR)) {
        initializer = this.varStatement();
      } else if (this.check(TokenType.IDENTIFIER)) {
        initializer = [this.expression()];
        while (!this.match(TokenType.COMMA)) {
          initializer.push(this.expression());
        }
        this.consume(TokenType.SEMICOLON, "Expect ; after while intializer");
      } else {
        this.error(this.peek(), "Expect declaration or assignment");
      }
    }
    const condition = this.expression();
    this.consume(TokenType.SEMICOLON, "Expect ; after condition");
    let increment: ExpressionStatement | undefined;
    if (!this.match(TokenType.RIGHT_PAREN)) {
      increment = new ExpressionStatement(this.expression());
    }
    return new ForStatement(
      this.statement(),
      condition,
      initializer,
      increment,
    );
  }

  whileStatement() {
    this.consume(TokenType.LEFT_PAREN, "Expect ( after while");
    const condition = this.expression();
    this.consume(TokenType.RIGHT_PAREN, "Expect ) after condition");
    return new ForStatement(this.statement(), condition);
  }

  varStatement() {
    const statements: VarStatement[] = [];

    while (true) {
      const name = this.consume(TokenType.IDENTIFIER, "Expect identifier");
      this.consume(TokenType.EQUAL, "Expect =");
      const intializer = this.expression();
      statements.push(new VarStatement(name, intializer));

      if (this.match(TokenType.COMMA)) {
        continue;
      } else if (this.match(TokenType.SEMICOLON)) {
        break;
      } else {
        this.error(this.peek(), "Unexpected token");
      }
    }
    return statements;
  }

  classStatement() {
    const name = this.consume(TokenType.IDENTIFIER, "Expect class name");
    let supercase: Token | undefined;

    if (this.match(TokenType.EXTENDS)) {
      supercase = this.consume(TokenType.IDENTIFIER, "Expect super class name");
    }
    this.consume(TokenType.LEFT_BRACE, "Expect {");

    let varStatements: VarStatement[] = [];
    let methodStatements: FunctionStatement[] = [];
    while (!this.match(TokenType.RIGHT_BRACE)) {
      if (this.match(TokenType.VAR)) {
        varStatements = [...varStatements, ...this.varStatement()];
      } else if (this.match(TokenType.DEF)) {
        methodStatements = [...methodStatements, this.functionStatement()];
      } else {
        this.error(this.peek(), "Expect class properties or methods");
      }
    }
    return new ClassStatement(name, varStatements, methodStatements, supercase);
  }

  functionStatement() {
    const name = this.consume(TokenType.IDENTIFIER, "Expect name");
    this.consume(TokenType.LEFT_PAREN, "Expect ( after name");
    const parameters = this.paramenters();
    this.consume(TokenType.RIGHT_PAREN, "Expect ) after parameters");
    const body = this.statement();
    return new FunctionStatement(name, parameters, body);
  }

  returnStatement() {
    if (this.match(TokenType.SEMICOLON)) {
      return new ReturnStatement();
    }
    return new ReturnStatement(this.expression());
  }

  emptyStatement() {
    return new EmptyStatement();
  }

  expressionStatement() {
    const expression = this.expression();
    this.consume(TokenType.SEMICOLON, "Expect ;");
    return new ExpressionStatement(expression);
  }

  expression() {
    return this.assignmentExpression();
  }

  assignmentExpression() {
    let logical = this.logicalExpression();

    while (this.match(TokenType.EQUAL)) {
      if (logical instanceof GetExpression) {
        logical = new SetExpression(
          logical.object,
          logical.name,
          this.logicalExpression(),
        );
      } else if (logical instanceof VarExpression) {
        logical = new AssignmentExpression(logical, this.logicalExpression());
      }
    }
    return logical;
  }

  logicalExpression() {
    let equality = this.equalityExpression();

    while (this.match(TokenType.AND, TokenType.OR)) {
      equality = new LogicalExpression(
        equality,
        this.previous(),
        this.equalityExpression(),
      );
    }
    return equality;
  }

  equalityExpression() {
    let compare = this.compareExpression();

    while (this.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)) {
      compare = new LogicalExpression(
        compare,
        this.previous(),
        this.compareExpression(),
      );
    }
    return compare;
  }

  compareExpression() {
    let addition = this.additionExpression();

    while (
      this.match(
        TokenType.GREAT,
        TokenType.GREAT_THAN,
        TokenType.LESS,
        TokenType.LESS_THAN,
      )
    ) {
      addition = new LogicalExpression(
        addition,
        this.previous(),
        this.additionExpression(),
      );
    }
    return addition;
  }

  additionExpression() {
    let multiplication = this.multiplicationExpression();

    while (this.match(TokenType.PLUS, TokenType.MINUS)) {
      multiplication = new BinaryExpression(
        multiplication,
        this.previous(),
        this.multiplicationExpression(),
      );
    }
    return multiplication;
  }

  multiplicationExpression() {
    let exponentiation = this.exponentiationExpression();

    while (this.match(TokenType.STAR, TokenType.SLASH)) {
      exponentiation = new BinaryExpression(
        exponentiation,
        this.previous(),
        this.exponentiationExpression(),
      );
    }
    return exponentiation;
  }

  exponentiationExpression() {
    let unary = this.unaryExpression();

    while (this.match(TokenType.STAR_STAR)) {
      unary = new BinaryExpression(
        unary,
        this.previous(),
        this.unaryExpression(),
      );
    }
    return unary;
  }

  unaryExpression() {
    if (
      this.match(
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.PLUS_PLUS,
        TokenType.MINUS_MINUS,
        TokenType.BANG,
      )
    ) {
      new UnaryExpression(this.previous(), this.callExpression());
    }
    return this.callExpression();
  }

  callExpression() {
    let member = this.memberAccessExpression();

    while (this.match(TokenType.LEFT_PAREN)) {
      const args: Expression[] = [];

      args.push(this.expression());
      while (this.match(TokenType.COMMA)) {
        args.push(this.expression());
      }
      this.consume(TokenType.RIGHT_PAREN, "Expect ) after arguments");
      member = new CallExpression(member, args);
    }
    return member;
  }

  memberAccessExpression() {
    let primary = this.primaryExpression();

    while (this.match(TokenType.DOT)) {
      const name = this.consume(
        TokenType.IDENTIFIER,
        "Expect identifer after .",
      );
      primary = new GetExpression(primary, name);
    }
    return primary;
  }

  primaryExpression(): Expression {
    if (
      this.match(
        TokenType.BOOLEAN,
        TokenType.NUMBER,
        TokenType.STRING,
        TokenType.NULL,
      )
    ) {
      return new LiternalExpression(this.previous());
    } else if (this.match(TokenType.THIS)) {
      return new ThisExpression(this.previous());
    } else if (this.match(TokenType.SUPER)) {
      return new SuperExpression(this.previous());
    } else if (this.match(TokenType.LEFT_PAREN)) {
      const expression = new GroupExpression(this.expression());

      this.consume(TokenType.RIGHT_PAREN, "Expect ) after group");
      return expression;
    } else if (this.match(TokenType.IDENTIFIER)) {
      return new VarExpression(this.previous());
    } else if (this.match(TokenType.NEW)) {
      const name = this.consume(
        TokenType.IDENTIFIER,
        "Expect class name after new",
      );

      return new NewExpression(name, this.arguments());
    } else if (this.match(TokenType.LEFT_BRACKET)) {
      const elements: Expression[] = [];

      while (!this.match(TokenType.RIGHT_BRACKET)) {
        elements.push(this.expression());
        while (this.match(TokenType.COMMA)) {
          elements.push(this.expression());
        }
      }
      return new ArrayExpression(elements);
    } else if (this.match(TokenType.DEF)) {
      const paramenters = this.paramenters();

      this.consume(TokenType.ARROW, "Expect => after lambda");
      return new LambdaExpression(paramenters, this.statement());
    }

    return this.error(this.peek(), "Expect expression");
  }

  arguments() {
    const args: Expression[] = [];

    this.consume(TokenType.LEFT_PAREN, "Expect ( after identifier");
    while (!this.match(TokenType.RIGHT_PAREN)) {
      args.push(this.expression());
      while (this.match(TokenType.COMMA)) {
        args.push(this.expression());
      }
    }
    return args;
  }

  paramenters() {
    const paramenters: Token[] = [];

    this.consume(TokenType.LEFT_PAREN, "Expect (");
    while (!this.match(TokenType.RIGHT_PAREN)) {
      paramenters.push(
        this.consume(TokenType.IDENTIFIER, "Expect identifier after ("),
      );

      while (this.match(TokenType.COMMA)) {
        paramenters.push(
          this.consume(TokenType.IDENTIFIER, "Expect identifer after ,"),
        );
      }
    }
    return paramenters;
  }

  notAtEnd() {
    return (
      this.current < this.tokens.length && this.peek().type !== TokenType.EOF
    );
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

  consume(type: TokenType, errorMessage: string): Token | never {
    if (this.check(type)) {
      return this.advance();
    }

    return this.error(this.peek(), errorMessage);
  }

  check(type: TokenType) {
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

  error(token: Token, errorMessage: string): never {
    throw new Error(`${token.toString()} ${errorMessage}`);
  }
}
