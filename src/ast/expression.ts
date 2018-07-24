import { Token } from "../token";
import { Statement, ParameterDeclaration } from "./statement";
import { BuiltinTypes } from "../semantic/types";

export interface Expression {
  type?: string;
  accept(visitor: ExpressionVisitor);
}

export class AssignmentExpression implements Expression {
  type?: string;
  object: VarExpression;
  expression: Expression;

  constructor(object: VarExpression, expression: Expression) {
    this.object = object;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitAssignmentExpression(this);
  }
}

export class LogicalExpression implements Expression {
  type?: string;
  left: Expression;
  operator: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    this.left = left;
    this.operator = operator;
    this.right = right;
    this.type = BuiltinTypes.Boolean;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLogicalExpression(this);
  }
}

export class BinaryExpression implements Expression {
  type?: string;
  left: Expression;
  operator: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitBinaryExpression(this);
  }
}

export class UnaryExpression implements Expression {
  type?: string;
  operator: Token;
  right: Expression;

  constructor(operator: Token, right: Expression) {
    this.operator = operator;
    this.right = right;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitUnaryExpression(this);
  }
}

export class CallExpression implements Expression {
  type?: string;
  callee: Expression;
  args: Expression[];

  constructor(callee: Expression, args: Expression[]) {
    this.callee = callee;
    this.args = args;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitCallExpression(this);
  }
}

export class SetExpression implements Expression {
  type?: string;
  object: Expression;
  name: Token;
  value: Expression;

  constructor(object: Expression, name: Token, value: Expression) {
    this.object = object;
    this.name = name;
    this.value = value;
  }

  accept(visitor: ExpressionVisitor) {
    visitor.visitSetExpression(this);
  }
}

export class GetExpression implements Expression {
  type?: string;
  object: Expression;
  name: Token;

  constructor(object: Expression, name: Token) {
    this.object = object;
    this.name = name;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitGetExpression(this);
  }
}

export class LiteralExpression implements Expression {
  type?: string;
  name: Token;

  constructor(name: Token, type?: string) {
    this.name = name;
    this.type = type;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLiteralExpression(this);
  }
}

export class GroupExpression implements Expression {
  type?: string;
  expression: Expression;

  constructor(expression: Expression) {
    this.expression = expression;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitGroupExpression(this);
  }
}

export class LambdaExpression implements Expression {
  type?: string;
  returnType?: string;
  parameters: ParameterDeclaration[];
  body: Statement;

  constructor(
    parameters: ParameterDeclaration[],
    body: Statement,
    returnType?: string,
  ) {
    this.parameters = parameters;
    this.body = body;
    this.returnType = returnType;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLambdaExpression(this);
  }
}

export class TupleExpression implements Expression {
  type?: string;
  values: Expression[];

  constructor(values: Expression[]) {
    this.values = values;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitTupleExpression(this);
  }
}

export class NewExpression implements Expression {
  name: Token;
  args: Expression[];

  constructor(name: Token, args: Expression[]) {
    this.name = name;
    this.args = args;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitNewExpression(this);
  }
}

export class ArrayExpression implements Expression {
  type?: string;
  elements: Expression[];

  constructor(elements: Expression[]) {
    this.elements = elements;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitArrayExpression(this);
  }
}

export class ThisExpression implements Expression {
  type?: string;
  keyword: Token;

  constructor(keyword: Token) {
    this.keyword = keyword;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitThisExpression(this);
  }
}

export class SuperExpression implements Expression {
  type?: string;
  keyword: Token;

  constructor(keyword: Token) {
    this.keyword = keyword;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitSuperExpression(this);
  }
}

export class VarExpression implements Expression {
  type?: string;
  name: Token;

  constructor(name: Token) {
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
