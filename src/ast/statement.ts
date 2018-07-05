import { Token } from "../token";

export class AstNode {}

export class Statement extends AstNode {}

export class Expression extends Statement {}

export class BlockStatement extends Statement {
  statements: Statement[];

  constructor(statements: Statement[]) {
    super();

    this.statements = statements;
  }
}

export class FunctionStatement extends Statement {
  name: Token;
  parameters: Token[];
  methods: Statement[];

  constructor(name: Token, parameters: Token[], methods: Statement[]) {
    super();

    this.name = name;
    this.parameters = parameters;
    this.methods = methods;
  }
}

export class ClassStatement extends Statement {
  name: Token;
  methods: FunctionStatement[];

  constructor(name: Token, methods: FunctionStatement[]) {
    super();

    this.name = name;
    this.methods = methods;
  }
}

export class ExpressionStatement extends Statement {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }
}

export class IfStatement extends Statement {
  condition: Expression;
  thenBranch: Statement[];
  elseBranch: Statement[];

  constructor(
    condition: Expression,
    thenBranch: Statement[],
    elseBranch: Statement[]
  ) {
    super();

    this.condition = condition;
    this.thenBranch = thenBranch;
    this.elseBranch = elseBranch;
  }
}

export class PrintStatement extends Statement {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }
}

export class ReturnStatement extends Statement {
  value: Expression;

  constructor(value: Expression) {
    super();

    this.value = value;
  }
}

export class VarStatement extends Statement {
  name: Token;
  initializer: Expression;

  constructor(name: Token, initializer: Expression) {
    super();

    this.name = name;
    this.initializer = initializer;
  }
}

export class WhileStatement extends Statement {
  condition: Expression;
  body: Statement[];

  constructor(condition: Expression, body: Statement[]) {
    super();

    this.condition = condition;
    this.body = body;
  }
}
