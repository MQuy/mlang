import { Token } from "../token";
import { AstNode } from "./base";

export abstract class Expression extends AstNode {
  abstract accept(vistor: ExpressionVistor);
}

export class AssignExpression extends Expression {
  name: Token;
  expression: Expression;

  constructor(name: Token, expression: Expression) {
    super();

    this.name = name;
    this.expression = expression;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitAssignExpression(this);
  }
}

export class BinaryExpression extends Expression {
  left: Expression;
  operator: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    super();

    this.left = left;
    this.operator = operator;
    this.right = right;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitBinaryExpression(this);
  }
}

export class CallExpression extends Expression {
  callee: Expression;
  arguments: Expression[];

  constructor(callee: Expression, args: Expression[]) {
    super();

    this.callee = callee;
    this.arguments = args;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitCallExpression(this);
  }
}

export class GetExpression extends Expression {
  object: Expression;
  name: Token;

  constructor(object: Expression, name: Token) {
    super();

    this.object = object;
    this.name = name;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitGetExpression(this);
  }
}

export class GroupingExpression extends Expression {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitGroupingExpression(this);
  }
}

export class LiteralExpression extends Expression {
  value: string | number | boolean | undefined;

  constructor(value: string | number | boolean | undefined) {
    super();

    this.value = value;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitLiternalExpression(this);
  }
}

export class LogicalExpression extends Expression {
  left: Expression;
  opeartor: Token;
  right: Expression;

  constructor(left: Expression, operator: Token, right: Expression) {
    super();

    this.left = left;
    this.opeartor = operator;
    this.right = right;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitLogicalExpression(this);
  }
}

export class SetExpression extends Expression {
  object: Expression;
  name: Token;
  expression: Expression;

  constructor(object: Expression, name: Token, expression: Expression) {
    super();

    this.object = object;
    this.name = name;
    this.expression = expression;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitSetExpression(this);
  }
}

export class SuperExpression extends Expression {
  name: Token;
  method: Token;

  constructor(name: Token, method: Token) {
    super();

    this.name = name;
    this.method = method;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitSuperExpression(this);
  }
}

export class ThisExpression extends Expression {
  keyword: Token;

  constructor(keyword: Token) {
    super();

    this.keyword = keyword;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitThisExpression(this);
  }
}

export class UnaryExpression extends Expression {
  opeartor: Token;
  right: Expression;

  constructor(operator: Token, right: Expression) {
    super();

    this.opeartor = operator;
    this.right = right;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitUnaryExpression(this);
  }
}

export class VarExpression extends Expression {
  name: Token;

  constructor(name: Token) {
    super();

    this.name = name;
  }

  accept(vistor: ExpressionVistor) {
    return vistor.visitVarExpression(this);
  }
}

export interface ExpressionVistor {
  visitAssignExpression(expr: AssignExpression);
  visitBinaryExpression(expr: BinaryExpression);
  visitCallExpression(expr: CallExpression);
  visitGetExpression(expr: GetExpression);
  visitGroupingExpression(expr: GroupingExpression);
  visitLiternalExpression(expr: LiteralExpression);
  visitLogicalExpression(expr: LogicalExpression);
  visitSetExpression(expr: SetExpression);
  visitSuperExpression(expr: SuperExpression);
  visitThisExpression(expr: ThisExpression);
  visitUnaryExpression(expr: UnaryExpression);
  visitVarExpression(expr: VarExpression);
}
