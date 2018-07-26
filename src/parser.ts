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
import { IRPosition } from "./ast/types";
import { error } from "./utils/print";

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
      return this.varsStatement();
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
    const ifToken = this.previous();
    this.consume(TokenType.LEFT_PAREN, "Expect ( after if");
    const condition = this.expression();
    this.consume(TokenType.RIGHT_PAREN, "Expect ) after if condition");

    const thenStatement = this.statement();
    let elseStatement: Statement | undefined;

    if (this.match(TokenType.ELSE)) {
      elseStatement = this.statement();
    }
    return this.generateStatement(
      new IfStatement(condition, thenStatement, elseStatement),
      ifToken,
    );
  }

  blockStatement() {
    const blockToken = this.previous();
    const statements: Statement[] = [];

    while (!this.match(TokenType.RIGHT_BRACE)) {
      statements.push(this.statement());
    }
    return this.generateStatement(new BlockStatement(statements), blockToken);
  }

  breakStatement() {
    const breakToken = this.previous();
    this.consume(TokenType.SEMICOLON, "Expect ; after break");
    return this.generateStatement(new BreakStatement(), breakToken);
  }

  continueStatement() {
    const continueToken = this.previous();
    this.consume(TokenType.SEMICOLON, "Expect ; after continue");
    return this.generateStatement(new ContinueStatement(), continueToken);
  }

  forStatement() {
    const forToken = this.previous();
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
        error(this.peek(), "Expect declaration or assignment");
      }
    }

    let condition: Expression | undefined;
    if (!this.match(TokenType.SEMICOLON)) {
      condition = this.expression();
      this.consume(TokenType.SEMICOLON, "Expect ; after condition");
    }
    let increment: ExpressionStatement | undefined;
    if (!this.match(TokenType.RIGHT_PAREN)) {
      const incrementExpression = this.expression();
      increment = this.generateStatement(
        new ExpressionStatement(incrementExpression),
        incrementExpression.pStart,
      );
      this.consume(TokenType.RIGHT_PAREN, "Expect )");
    }
    return this.generateStatement(
      new ForStatement(this.statement(), condition, initializer, increment),
      forToken,
    );
  }

  whileStatement() {
    const whileToken = this.previous();
    this.consume(TokenType.LEFT_PAREN, "Expect ( after while");
    const condition = this.expression();
    this.consume(TokenType.RIGHT_PAREN, "Expect ) after condition");
    return this.generateStatement(
      new ForStatement(this.statement(), condition),
      whileToken,
    );
  }

  varsStatement() {
    const varToken = this.previous();
    return this.generateStatement(
      new VarsStatement(this.varStatement()),
      varToken,
    );
  }

  varStatement() {
    const statements: VarStatement[] = [];
    while (this.notAtEnd()) {
      const varToken =
        this.previous().type === TokenType.VAR ? this.previous() : this.peek();
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

      statements.push(
        this.generateStatement(
          new VarStatement(name, intializer, type),
          varToken,
          this.peek().type === TokenType.SEMICOLON
            ? this.peek()
            : this.previous(),
        ),
      );

      if (this.match(TokenType.COMMA)) {
        continue;
      } else if (this.match(TokenType.SEMICOLON)) {
        break;
      } else {
        error(this.peek(), "Expect ; after declaration");
      }
    }
    return statements;
  }

  classStatement() {
    const classToken = this.previous();
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
        error(this.peek(), "Expect class properties or methods");
      }
    }
    return this.generateStatement(
      new ClassStatement(name, varStatements, methodStatements, superclass),
      classToken,
    );
  }

  functionStatement() {
    const functionToken = this.previous();
    const name = this.consume(TokenType.IDENTIFIER, "Expect name");
    this.consume(TokenType.LEFT_PAREN, "Expect ( after name");

    const parameters = this.parameters();

    this.consume(TokenType.COLON, "Expect return type");
    const kind = this.consume(
      TokenType.IDENTIFIER,
      "Expect type after function",
    );

    return this.generateStatement(
      new FunctionStatement(name, parameters, this.statement(), kind.lexeme),
      functionToken,
    );
  }

  returnStatement() {
    const returnToken = this.previous();
    let expression: Expression | undefined;

    if (!this.match(TokenType.SEMICOLON)) {
      expression = this.expression();
      this.consume(TokenType.SEMICOLON, "Expect ; after return expression");
    }
    return this.generateStatement(new ReturnStatement(expression), returnToken);
  }

  emptyStatement() {
    return this.generateStatement(new EmptyStatement(), this.previous());
  }

  expressionStatement() {
    const expression = this.expression();
    this.consume(TokenType.SEMICOLON, "Expect ;");
    return this.generateStatement(
      new ExpressionStatement(expression),
      expression.pStart,
    );
  }

  expression() {
    return this.assignmentExpression();
  }

  assignmentExpression() {
    let logical = this.logicalExpression();

    while (this.match(TokenType.EQUAL)) {
      if (logical instanceof GetExpression) {
        logical = this.generateExpression(
          new SetExpression(
            logical.object,
            logical.name,
            this.logicalExpression(),
          ),
          logical.pStart,
        );
      } else if (logical instanceof VarExpression) {
        logical = this.generateExpression(
          new AssignmentExpression(logical, this.logicalExpression()),
          logical.pStart,
        );
      }
    }
    return logical;
  }

  logicalExpression() {
    let equality = this.equalityExpression();

    while (this.match(TokenType.AND, TokenType.OR)) {
      equality = this.generateExpression(
        new LogicalExpression(
          equality,
          this.previous(),
          this.equalityExpression(),
        ),
        equality.pStart,
      );
    }
    return equality;
  }

  equalityExpression() {
    let compare = this.compareExpression();

    while (this.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL)) {
      compare = this.generateExpression(
        new LogicalExpression(
          compare,
          this.previous(),
          this.compareExpression(),
        ),
        compare.pStart,
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
      addition = this.generateExpression(
        new LogicalExpression(
          addition,
          this.previous(),
          this.additionExpression(),
        ),
        addition.pStart,
      );
    }
    return addition;
  }

  additionExpression() {
    let multiplication = this.multiplicationExpression();

    while (this.match(TokenType.PLUS, TokenType.MINUS)) {
      multiplication = this.generateExpression(
        new BinaryExpression(
          multiplication,
          this.previous(),
          this.multiplicationExpression(),
        ),
        multiplication.pStart,
      );
    }
    return multiplication;
  }

  multiplicationExpression() {
    let exponentiation = this.exponentiationExpression();

    while (this.match(TokenType.STAR, TokenType.SLASH)) {
      exponentiation = this.generateExpression(
        new BinaryExpression(
          exponentiation,
          this.previous(),
          this.exponentiationExpression(),
        ),
        exponentiation.pStart,
      );
    }
    return exponentiation;
  }

  exponentiationExpression() {
    let unary = this.unaryExpression();

    while (this.match(TokenType.STAR_STAR)) {
      unary = this.generateExpression(
        new BinaryExpression(unary, this.previous(), this.unaryExpression()),
        unary.pStart,
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
      const unaryToken = this.previous();
      return this.generateExpression(
        new UnaryExpression(this.previous(), this.callExpression()),
        unaryToken,
      );
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
      member = this.generateExpression(
        new CallExpression(member, args),
        member.pStart,
      );
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
      primary = this.generateExpression(
        new GetExpression(primary, name),
        primary.pStart,
      );
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
      const literalToken = this.previous();
      return this.generateExpression(
        new LiteralExpression(literalToken),
        literalToken,
      );
    } else if (this.match(TokenType.THIS)) {
      return this.generateExpression(
        new ThisExpression(this.previous()),
        this.previous(),
      );
    } else if (this.match(TokenType.SUPER)) {
      return this.generateExpression(
        new SuperExpression(this.previous()),
        this.previous(),
      );
    } else if (this.match(TokenType.LEFT_PAREN)) {
      const parenToken = this.previous();
      const expression = this.expression();
      const groupExpression = new GroupExpression(expression);

      this.consume(TokenType.RIGHT_PAREN, "Expect ) after group");
      return this.generateExpression(groupExpression, parenToken);
    } else if (this.match(TokenType.IDENTIFIER)) {
      return this.generateExpression(
        new VarExpression(this.previous()),
        this.previous(),
      );
    } else if (this.match(TokenType.NEW)) {
      const newToken = this.previous();
      const name = this.consume(
        TokenType.IDENTIFIER,
        "Expect class name after new",
      );

      this.consume(TokenType.LEFT_PAREN, "Expect ( after class name");
      return this.generateExpression(
        new NewExpression(name, this.arguments()),
        newToken,
      );
    } else if (this.match(TokenType.LEFT_BRACKET)) {
      const arrayToken = this.previous();
      const elements: Expression[] = [];

      while (!this.match(TokenType.RIGHT_BRACKET)) {
        elements.push(this.expression());
        while (this.match(TokenType.COMMA)) {
          elements.push(this.expression());
        }
      }
      return this.generateExpression(new ArrayExpression(elements), arrayToken);
    } else if (this.match(TokenType.DEF)) {
      const defToken = this.previous();
      this.consume(TokenType.LEFT_PAREN, "Expect ( lambda");
      const paramenters = this.parameters();

      this.consume(TokenType.COLON, "Expect : after lambda");
      const returnKind = this.consume(
        TokenType.IDENTIFIER,
        "Expect lambda return type",
      );

      this.consume(TokenType.ARROW, "Expect => after lambda");
      return this.generateExpression(
        new LambdaExpression(paramenters, this.statement(), returnKind.lexeme),
        defToken,
      );
    }

    return error(this.peek(), "Expect expression");
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

    this.consume(TokenType.COLON, "Expect : after parameter name");
    const kind = this.consume(
      TokenType.IDENTIFIER,
      "Expect type after parameter",
    );

    return new ParameterDeclaration(name, kind.lexeme);
  }

  generateStatement<T extends Statement>(
    statement: T,
    start: Token | IRPosition,
    end = this.previous(),
  ): T {
    statement.pStart = { line: start.line, column: start.column };
    statement.pEnd = { line: end.line, column: end.column + end.lexeme.length };
    return statement;
  }

  generateExpression(
    expression: Expression,
    start: Token | IRPosition,
    end: Token = this.previous(),
  ) {
    expression.pStart = { line: start.line, column: start.column };
    expression.pEnd = {
      line: end.line,
      column: end.column + end.lexeme.length,
    };
    return expression;
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

    return error(this.peek(), errorMessage);
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
}
