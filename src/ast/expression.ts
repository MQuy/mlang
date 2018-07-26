import { Token } from "../token";
import { Statement, ParameterDeclaration } from "./statement";
import { BuiltinTypes, Types } from "../semantic/types";
import { IRNode, IRPosition } from "./types";

export interface Expression {
  type?: Types;
  pStart: IRPosition;
  pEnd: IRPosition;
  accept(visitor: ExpressionVisitor);
}

export class AssignmentExpression extends IRNode implements Expression {
  type?: Types;
  object: VarExpression;
  expression: Expression;

  constructor(object: VarExpression, expression: Expression) {
    super();

    this.object = object;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitAssignmentExpression(this);
  }
}

export class LogicalExpression extends IRNode implements Expression {
  type?: Types;
  left: Expression;
  operator: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    super();

    this.left = left;
    this.operator = operator;
    this.right = right;
    this.type = BuiltinTypes.Boolean;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLogicalExpression(this);
  }
}

export class BinaryExpression extends IRNode implements Expression {
  type?: Types;
  left: Expression;
  operator: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    super();

    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitBinaryExpression(this);
  }
}

export class UnaryExpression extends IRNode implements Expression {
  type?: Types;
  operator: Token;
  right: Expression;

  constructor(operator: Token, right: Expression) {
    super();

    this.operator = operator;
    this.right = right;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitUnaryExpression(this);
  }
}

export class CallExpression extends IRNode implements Expression {
  type?: Types;
  callee: Expression;
  args: Expression[];

  constructor(callee: Expression, args: Expression[]) {
    super();

    this.callee = callee;
    this.args = args;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitCallExpression(this);
  }
}

export class SetExpression extends IRNode implements Expression {
  type?: Types;
  object: Expression;
  name: Token;
  value: Expression;

  constructor(object: Expression, name: Token, value: Expression) {
    super();

    this.object = object;
    this.name = name;
    this.value = value;
  }

  accept(visitor: ExpressionVisitor) {
    visitor.visitSetExpression(this);
  }
}

export class GetExpression extends IRNode implements Expression {
  type?: Types;
  object: Expression;
  name: Token;

  constructor(object: Expression, name: Token) {
    super();

    this.object = object;
    this.name = name;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitGetExpression(this);
  }
}

export class LiteralExpression extends IRNode implements Expression {
  type?: Types;
  name: Token;

  constructor(name: Token) {
    super();

    this.name = name;

    const type = name.type.toLowerCase();
    this.type = (type.slice(0, 1).toUpperCase() + type.slice(1)) as Types;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLiteralExpression(this);
  }
}

export class GroupExpression extends IRNode implements Expression {
  type?: Types;
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitGroupExpression(this);
  }
}

export class LambdaExpression extends IRNode implements Expression {
  type?: Types;
  returnType: string;
  parameters: ParameterDeclaration[];
  body: Statement;

  constructor(
    parameters: ParameterDeclaration[],
    body: Statement,
    returnType: string,
  ) {
    super();

    this.parameters = parameters;
    this.body = body;
    this.returnType = returnType;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLambdaExpression(this);
  }
}

export class TupleExpression extends IRNode implements Expression {
  type?: Types;
  values: Expression[];

  constructor(values: Expression[]) {
    super();

    this.values = values;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitTupleExpression(this);
  }
}

export class NewExpression extends IRNode implements Expression {
  name: Token;
  args: Expression[];

  constructor(name: Token, args: Expression[]) {
    super();

    this.name = name;
    this.args = args;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitNewExpression(this);
  }
}

export class ArrayExpression extends IRNode implements Expression {
  type?: Types;
  elements: Expression[];

  constructor(elements: Expression[]) {
    super();

    this.elements = elements;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitArrayExpression(this);
  }
}

export class ThisExpression extends IRNode implements Expression {
  type?: Types;
  keyword: Token;

  constructor(keyword: Token) {
    super();

    this.keyword = keyword;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitThisExpression(this);
  }
}

export class SuperExpression extends IRNode implements Expression {
  type?: Types;
  keyword: Token;

  constructor(keyword: Token) {
    super();

    this.keyword = keyword;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitSuperExpression(this);
  }
}

export class VarExpression extends IRNode implements Expression {
  type?: Types;
  name: Token;

  constructor(name: Token) {
    super();

    this.name = name;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitVarExpression(this);
  }
}

export interface ExpressionVisitor {
  visitAssignmentExpression(expression: AssignmentExpression);
  visitLogicalExpression(expression: LogicalExpression);
  visitBinaryExpression(expression: BinaryExpression);
  visitUnaryExpression(expression: UnaryExpression);
  visitCallExpression(expression: CallExpression);
  visitGetExpression(expression: GetExpression);
  visitSetExpression(expression: SetExpression);
  visitLiteralExpression(expression: LiteralExpression);
  visitGroupExpression(expression: GroupExpression);
  visitLambdaExpression(expression: LambdaExpression);
  visitTupleExpression(expression: TupleExpression);
  visitNewExpression(expression: NewExpression);
  visitArrayExpression(expression: ArrayExpression);
  visitThisExpression(expression: ThisExpression);
  visitSuperExpression(expression: SuperExpression);
  visitVarExpression(expression: VarExpression);
}
