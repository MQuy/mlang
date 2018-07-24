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
  ParameterDeclaration,
} from "./ast/statement";
import {
  Expression,
  AssignmentExpression,
  LogicalExpression,
  BinaryExpression,
  UnaryExpression,
  CallExpression,
  GetExpression,
  LiteralExpression,
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

    this.current = 0;

    while (this.notAtEnd()) {
      statements.push(this.statement());
    }

    return new Program(statements);
  }

  statement(): Statement {
    if (this.match(TokenType.IF)) {
      return this.ifStatement();
    } else if (this.match(TokenType.LEFT_BRACE)) {
      return this.blockStatement();
    } else if (this.match(TokenType.BREAK)) {
      return this.breakStatement();
    } else if (this.match(TokenType.CONTINUE)) {
      return this.continueStatement();
    } else if (this.match(TokenType.FOR)) {
      return this.forStatement();
    } else if (this.match(TokenType.WHILE)) {
      return this.whileStatement();
    } else if (this.match(TokenType.VAR)) {
      return new VarsStatement(this.varStatement());
    } else if (this.match(TokenType.CLASS)) {
      return this.classStatement();
    } else if (this.match(TokenType.RETURN)) {
      return this.returnStatement();
    } else if (this.match(TokenType.SEMICOLON)) {
      return this.emptyStatement();
    } else if (
      this.check(TokenType.DEF) &&
      this.next().type === TokenType.IDENTIFIER
    ) {
      this.consume(TokenType.DEF, "Expect def");
      return this.functionStatement();
    } else {
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
        while (this.match(TokenType.COMMA)) {
          initializer.push(this.expression());
        }
        this.consume(TokenType.SEMICOLON, "Expect ; after while intializer");
      } else {
        this.error(this.peek(), "Expect declaration or assignment");
      }
    }

    let condition: Expression | undefined;
    if (!this.match(TokenType.SEMICOLON)) {
      condition = this.expression();
      this.consume(TokenType.SEMICOLON, "Expect ; after condition");
    }
    let increment: ExpressionStatement | undefined;
    if (!this.match(TokenType.RIGHT_PAREN)) {
      increment = new ExpressionStatement(this.expression());
      this.consume(TokenType.RIGHT_PAREN, "Expect )");
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

    while (this.notAtEnd()) {
      const name = this.consume(
        TokenType.IDENTIFIER,
        "Expect identifier after var",
      );
      let intializer: Expression | undefined;
      let type: string | undefined;

      if (this.match(TokenType.COLON)) {
        const kind = this.consume(
          TokenType.IDENTIFIER,
          "Expect type after var",
        );
        type = kind.lexeme;
      }

      if (this.match(TokenType.EQUAL)) {
        intializer = this.expression();
      }

      statements.push(new VarStatement(name, intializer, type));

      if (this.match(TokenType.COMMA)) {
        continue;
      } else if (this.match(TokenType.SEMICOLON)) {
        break;
      } else {
        this.error(this.previous(), "Expect ; after declaration");
      }
    }
    return statements;
  }

  classStatement() {
    const name = this.consume(TokenType.IDENTIFIER, "Expect class name");
    let superclass: Token | undefined;

    if (this.match(TokenType.EXTENDS)) {
      superclass = this.consume(
        TokenType.IDENTIFIER,
        "Expect super class name",
      );
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
    return new ClassStatement(
      name,
      varStatements,
      methodStatements,
      superclass,
    );
  }

  functionStatement() {
    const name = this.consume(TokenType.IDENTIFIER, "Expect name");
    this.consume(TokenType.LEFT_PAREN, "Expect ( after name");

    const parameters = this.parameters();

    this.consume(TokenType.COLON, "Expect return type");
    const kind = this.consume(
      TokenType.IDENTIFIER,
      "Expect type after function",
    );

    return new FunctionStatement(
      name,
      parameters,
      this.statement(),
      kind.lexeme,
    );
  }

  returnStatement() {
    let expression: Expression | undefined;

    if (!this.match(TokenType.SEMICOLON)) {
      expression = this.expression();
      this.consume(TokenType.SEMICOLON, "Expect ; after return expression");
    }
    return new ReturnStatement(expression);
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
      return new UnaryExpression(this.previous(), this.callExpression());
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
      const type = this.previous().type.toLowerCase();
      return new LiteralExpression(
        this.previous(),
        type.slice(0, 1).toUpperCase() + type.slice(1),
      );
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

      this.consume(TokenType.LEFT_PAREN, "Expect ( after class name");
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
      this.consume(TokenType.LEFT_PAREN, "Expect ( lambda");
      const paramenters = this.parameters();

      this.consume(TokenType.COLON, "Expect : after lambda");
      const returnKind = this.consume(
        TokenType.IDENTIFIER,
        "Expect lambda return type",
      );

      this.consume(TokenType.ARROW, "Expect => after lambda");
      return new LambdaExpression(
        paramenters,
        this.statement(),
        returnKind.lexeme,
      );
    }

    return this.error(this.previous(), "Expect expression");
  }

  arguments() {
    const args: Expression[] = [];

    while (!this.match(TokenType.RIGHT_PAREN)) {
      args.push(this.expression());
      while (this.match(TokenType.COMMA)) {
        args.push(this.expression());
      }
    }
    return args;
  }

  parameters() {
    const paramenters: ParameterDeclaration[] = [];

    while (!this.match(TokenType.RIGHT_PAREN)) {
      paramenters.push(this.parameter());

      while (this.match(TokenType.COMMA)) {
        paramenters.push(this.parameter());
      }
    }
    return paramenters;
  }

  parameter() {
    const name = this.consume(TokenType.IDENTIFIER, "Expect parameter name");

    debugger;
    this.consume(TokenType.COLON, "Expect : after parameter name");
    const kind = this.consume(
      TokenType.IDENTIFIER,
      "Expect type after parameter",
    );

    return new ParameterDeclaration(name, kind.lexeme);
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
