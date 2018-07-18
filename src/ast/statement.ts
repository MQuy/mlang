import { Expression, AssignmentExpression } from "./expression";
import { Token } from "../token";
import { TreeNode } from "./type";

export interface Statement extends TreeNode {
  accept(visitor: StatementVisitor): void;
}

export class IfStatement implements Statement {
  condition: Expression;
  thenStatement: Statement;
  elseStatement?: Statement;

  constructor(
    condition: Expression,
    thenStatement: Statement,
    elseStatement?: Statement,
  ) {
    this.condition = condition;
    this.thenStatement = thenStatement;
    this.elseStatement = elseStatement;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitIfStatement(this);
  }
}

export class BlockStatement implements Statement {
  statements: Statement[];

  constructor(statements: Statement[]) {
    this.statements = statements;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitBlockStatement(this);
  }
}

export class BreakStatement implements Statement {
  accept(visitor: StatementVisitor) {
    visitor.visitBreakStatement(this);
  }
}

export class ContinueStatement implements Statement {
  accept(visitor: StatementVisitor) {
    visitor.visitContinueStatement(this);
  }
}

export class ForStatement implements Statement {
  initializer?: VarStatement[] | AssignmentExpression[];
  condition?: Expression;
  body: Statement;

  constructor(
    body: Statement,
    condition?: Expression,
    intializer?: VarStatement[] | AssignmentExpression[],
  ) {
    this.body = body;
    this.condition = condition;
    this.initializer = intializer;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitForStatement(this);
  }
}

export class VarStatement implements Statement {
  name: Token;
  initializer?: Expression;

  constructor(name: Token, initializer: Expression) {
    this.name = name;
    this.initializer = initializer;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitVarStatement(this);
  }
}

export class ClassStatement implements Statement {
  name: Token;
  supercase?: Token;
  properties?: VarStatement[];
  methods?: FunctionStatement[];

  constructor(
    name: Token,
    properties: VarStatement[],
    methods: FunctionStatement[],
    superclass?: Token,
  ) {
    this.name = name;
    this.supercase = this.supercase;
    this.properties = properties;
    this.methods = methods;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitClassStatement(this);
  }
}

export class FunctionStatement implements Statement {
  name: Token;
  parameters: Token[];
  body: Statement;

  constructor(name: Token, parameters: Token[], body: Statement) {
    this.name = name;
    this.parameters = parameters;
    this.body = body;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitFunctionStatement(this);
  }
}

export class ReturnStatement implements Statement {
  value?: Expression;

  constructor(value?: Expression) {
    this.value = value;
  }

  accept(visitor: StatementVisitor) {
    visitor.visitReturnStatement(this);
  }
}

export class EmptyStatement implements Statement {
  accept(visitor: StatementVisitor) {}
}

export interface StatementVisitor {
  visitIfStatement(statement: IfStatement): void;
  visitBlockStatement(statement: BlockStatement): void;
  visitBreakStatement(statement: BreakStatement): void;
  visitContinueStatement(statement: ContinueStatement): void;
  visitForStatement(statement: ForStatement): void;
  visitVarStatement(statement: VarStatement): void;
  visitClassStatement(statement: ClassStatement): void;
  visitFunctionStatement(statement: FunctionStatement): void;
  visitReturnStatement(statement: ReturnStatement): void;
}
