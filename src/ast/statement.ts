import { Token } from "../token";
import { Statement, Expression } from "./base";
import { VarExpression } from "./expression";

export class BlockStatement extends Statement {
  statements: Statement[];

  constructor(statements: Statement[]) {
    super();

    this.statements = statements;
  }

  accept(vistor: StatementVistor) {
    vistor.visitBlockStatement(this);
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

  accept(vistor: StatementVistor) {
    vistor.visitFunctionStatement(this);
  }
}

export class ClassStatement extends Statement {
  name: Token;
  superclass?: VarExpression;
  methods: FunctionStatement[];

  constructor(
    name: Token,
    methods: FunctionStatement[],
    superclass?: VarExpression
  ) {
    super();

    this.name = name;
    this.methods = methods;
    this.superclass = superclass;
  }

  accept(vistor: StatementVistor) {
    vistor.visitClassStatement(this);
  }
}

export class ExpressionStatement extends Statement {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(vistor: StatementVistor) {
    vistor.visitExpressionStatement(this);
  }
}

export class IfStatement extends Statement {
  condition: Expression;
  thenBranch: Statement;
  elseBranch?: Statement;

  constructor(
    condition: Expression,
    thenBranch: Statement,
    elseBranch?: Statement
  ) {
    super();

    this.condition = condition;
    this.thenBranch = thenBranch;
    this.elseBranch = elseBranch;
  }

  accept(vistor: StatementVistor) {
    vistor.visitIfStatement(this);
  }
}

export class PrintStatement extends Statement {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(vistor: StatementVistor) {
    vistor.visitPrintStatement(this);
  }
}

export class ReturnStatement extends Statement {
  value?: Expression;

  constructor(value?: Expression) {
    super();

    this.value = value;
  }

  accept(vistor: StatementVistor) {
    vistor.visitReturnStatement(this);
  }
}

export class VarStatement extends Statement {
  name: Token;
  initializer?: Expression;

  constructor(name: Token, initializer?: Expression) {
    super();

    this.name = name;
    this.initializer = initializer;
  }

  accept(vistor: StatementVistor) {
    vistor.visitVarStatement(this);
  }
}

export class WhileStatement extends Statement {
  condition?: Expression;
  body: Statement;

  constructor(body: Statement, condition?: Expression) {
    super();

    this.body = body;
    this.condition = condition;
  }

  accept(vistor: StatementVistor) {
    vistor.visitWhileStatement(this);
  }
}

export abstract class StatementVistor {
  abstract visitBlockStatement(stms: BlockStatement);
  abstract visitFunctionStatement(stms: FunctionStatement);
  abstract visitClassStatement(stms: ClassStatement);
  abstract visitExpressionStatement(stms: ExpressionStatement);
  abstract visitIfStatement(stms: IfStatement);
  abstract visitPrintStatement(stms: PrintStatement);
  abstract visitReturnStatement(stms: ReturnStatement);
  abstract visitVarStatement(smts: VarStatement);
  abstract visitWhileStatement(stms: WhileStatement);
}
