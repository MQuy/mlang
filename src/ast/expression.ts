import { Token } from "../token";
import { Statement } from "./statement";

export interface Expression {
  accept(visitor: ExpressionVisitor);
}

export class AssignmentExpression implements Expression {
  name: Token;
  expression: Expression;

  constructor(name: Token, expression: Expression) {
    this.name = name;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitAssignmentExpression(this);
  }
}

export class LogicalExpression implements Expression {
  left: Expression;
  operator: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLogicalExpression(this);
  }
}

export class BinaryExpression implements Expression {
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
  name: Token;
  args: Expression[];

  constructor(name: Token, args: Expression[]) {
    this.name = name;
    this.args = args;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitCallExpression(this);
  }
}

export class GetExpression implements Expression {
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

export class LiternalExpression implements Expression {
  name: Token;

  constructor(name: Token) {
    this.name = name;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLiteralExpression(this);
  }
}

export class GroupExpression implements Expression {
  expression: Expression;

  constructor(expression: Expression) {
    this.expression = expression;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitGroupExpression(this);
  }
}

export class LambdaExpression implements Expression {
  parameters: Token[];
  body: Statement;

  constructor(parameters: Token[], body: Statement) {
    this.parameters = parameters;
    this.body = body;
  }

  accept(visitor: ExpressionVisitor) {
    return visitor.visitLambdaExpression(this);
  }
}

export class TupleExpression implements Expression {
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
  elements: Expression[];

  constructor(elements: Expression[]) {
    this.elements = elements;
  }

  accept(visitor: ExpressionVisitor) {
    visitor.visitArrayExpression(this);
  }
}

export interface ExpressionVisitor {
  visitAssignmentExpression(expression: AssignmentExpression);
  visitLogicalExpression(expression: LogicalExpression): boolean;
  visitBinaryExpression(expression: BinaryExpression);
  visitUnaryExpression(expression: UnaryExpression);
  visitCallExpression(expression: CallExpression);
  visitGetExpression(expression: GetExpression);
  visitLiteralExpression(expression: LiternalExpression);
  visitGroupExpression(expression: GroupExpression);
  visitLambdaExpression(expression: LambdaExpression);
  visitTupleExpression(expression: TupleExpression);
  visitNewExpression(expression: NewExpression);
  visitArrayExpression(expression: ArrayExpression);
}
