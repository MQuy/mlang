import { Token } from "../token";
import { AstNode } from "./base";
import { VarExpression, Expression } from "./expression";

let incrementValue = 0;

export class Statement extends AstNode {
  hash: number;

  constructor() {
    super();

    this.hash = incrementValue++;
  }

  accept(vistor: StatementVistor) {}
}

interface Vistor {
  accept(vistor: StatementVistor);
}

export class BlockStatement extends Statement implements Vistor {
  statements: Statement[];

  constructor(statements: Statement[]) {
    super();

    this.statements = statements;
  }

  accept(vistor: StatementVistor) {
    return vistor.visitBlockStatement(this);
  }
}

export class FunctionStatement extends Statement implements Vistor {
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
    return vistor.visitFunctionStatement(this);
  }
}

export class ClassStatement extends Statement implements Vistor {
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
    return vistor.visitClassStatement(this);
  }
}

export class ExpressionStatement extends Statement implements Vistor {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(vistor: StatementVistor) {
    return vistor.visitExpressionStatement(this);
  }
}

export class IfStatement extends Statement implements Vistor {
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
    return vistor.visitIfStatement(this);
  }
}

export class PrintStatement extends Statement implements Vistor {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(vistor: StatementVistor) {
    return vistor.visitPrintStatement(this);
  }
}

export class ReturnStatement extends Statement implements Vistor {
  value?: Expression;

  constructor(value?: Expression) {
    super();

    this.value = value;
  }

  accept(vistor: StatementVistor) {
    return vistor.visitReturnStatement(this);
  }
}

export class VarStatement extends Statement implements Vistor {
  name: Token;
  initializer?: Expression;

  constructor(name: Token, initializer?: Expression) {
    super();

    this.name = name;
    this.initializer = initializer;
  }

  accept(vistor: StatementVistor) {
    return vistor.visitVarStatement(this);
  }
}

export class WhileStatement extends Statement implements Vistor {
  condition?: Expression;
  body: Statement;

  constructor(body: Statement, condition?: Expression) {
    super();

    this.body = body;
    this.condition = condition;
  }

  accept(vistor: StatementVistor) {
    return vistor.visitWhileStatement(this);
  }
}

export interface StatementVistor {
  visitBlockStatement(stms: BlockStatement);
  visitFunctionStatement(stms: FunctionStatement);
  visitClassStatement(stms: ClassStatement);
  visitExpressionStatement(stms: ExpressionStatement);
  visitIfStatement(stms: IfStatement);
  visitPrintStatement(stms: PrintStatement);
  visitReturnStatement(stms: ReturnStatement);
  visitVarStatement(smts: VarStatement);
  visitWhileStatement(stms: WhileStatement);
}
