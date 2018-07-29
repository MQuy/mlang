import { Expression } from "./expression";
import { Token } from "../token";
import { IRNode, IRPosition } from "./types";

export interface Statement {
  pStart: IRPosition;
  pEnd: IRPosition;
  accept(visitor: StatementVisitor);
}

export class IfStatement extends IRNode implements Statement {
  condition: Expression;
  thenStatement: Statement;
  elseStatement?: Statement;

  constructor(
    condition: Expression,
    thenStatement: Statement,
    elseStatement?: Statement,
  ) {
    super();

    this.condition = condition;
    this.thenStatement = thenStatement;
    this.elseStatement = elseStatement;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitIfStatement(this);
  }
}

export class BlockStatement extends IRNode implements Statement {
  statements: Statement[];

  constructor(statements: Statement[]) {
    super();

    this.statements = statements;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitBlockStatement(this);
  }
}

export class BreakStatement extends IRNode implements Statement {
  accept(visitor: StatementVisitor) {
    return visitor.visitBreakStatement(this);
  }
}

export class ContinueStatement extends IRNode implements Statement {
  accept(visitor: StatementVisitor) {
    return visitor.visitContinueStatement(this);
  }
}

export class ForStatement extends IRNode implements Statement {
  initializer?: VarStatement[] | Expression[];
  condition?: Expression;
  increment?: ExpressionStatement;
  body: Statement;

  constructor(
    body: Statement,
    condition?: Expression,
    intializer?: VarStatement[] | Expression[],
    increment?: ExpressionStatement,
  ) {
    super();

    this.body = body;
    this.condition = condition;
    this.initializer = intializer;
    this.increment = increment;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitForStatement(this);
  }
}

export class VarStatement extends IRNode implements Statement {
  type?: string;
  name: Token;
  initializer?: Expression;

  constructor(name: Token, initializer?: Expression, type?: string) {
    super();

    this.name = name;
    this.initializer = initializer;
    this.type = type;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitVarStatement(this);
  }
}

export class VarsStatement extends IRNode implements Statement {
  varStatements: VarStatement[];

  constructor(varStatements: VarStatement[]) {
    super();

    this.varStatements = varStatements;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitVarsStatements(this);
  }
}

export class ClassStatement extends IRNode implements Statement {
  name: Token;
  superclass?: Token;
  properties?: VarStatement[];
  methods?: FunctionStatement[];

  constructor(
    name: Token,
    properties: VarStatement[],
    methods: FunctionStatement[],
    superclass?: Token,
  ) {
    super();

    this.name = name;
    this.superclass = superclass;
    this.properties = properties;
    this.methods = methods;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitClassStatement(this);
  }
}

export class FunctionStatement extends IRNode implements Statement {
  returnType?: string;
  name: Token;
  parameters: ParameterDeclaration[];
  body: Statement;

  constructor(
    name: Token,
    parameters: ParameterDeclaration[],
    body: Statement,
    returnType?: string,
  ) {
    super();

    this.name = name;
    this.parameters = parameters;
    this.body = body;
    this.returnType = returnType;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitFunctionStatement(this);
  }
}

export class ParameterDeclaration {
  name: Token;
  type: string;

  constructor(name: Token, type: string) {
    this.name = name;
    this.type = type;
  }
}

export class ReturnStatement extends IRNode implements Statement {
  value?: Expression;

  constructor(value?: Expression) {
    super();

    this.value = value;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitReturnStatement(this);
  }
}

export class EmptyStatement extends IRNode implements Statement {
  accept(visitor: StatementVisitor) {
    return undefined;
  }
}

export class ExpressionStatement extends IRNode implements Statement {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(visitor: StatementVisitor) {
    return visitor.visitExpressionStatement(this);
  }
}

export interface StatementVisitor {
  visitIfStatement(statement: IfStatement);
  visitBlockStatement(statement: BlockStatement);
  visitBreakStatement(statement: BreakStatement);
  visitContinueStatement(statement: ContinueStatement);
  visitForStatement(statement: ForStatement);
  visitVarStatement(statement: VarStatement);
  visitVarsStatements(statement: VarsStatement);
  visitClassStatement(statement: ClassStatement);
  visitFunctionStatement(statement: FunctionStatement);
  visitReturnStatement(statement: ReturnStatement);
  visitExpressionStatement(statement: ExpressionStatement);
}
