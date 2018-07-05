import { Token } from "../token";
import { Expression } from "./base";

export class AssignExpression extends Expression {
  name: Token;
  expression: Expression;

  constructor(name: Token, expression: Expression) {
    super();

    this.name = name;
    this.expression = expression;
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
}

export class CallExpression extends Expression {
  callee: Expression;
  arguments: Expression[];

  constructor(callee: Expression, args: Expression[]) {
    super();

    this.callee = callee;
    this.arguments = args;
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
}

export class GroupingExpression extends Expression {
  expression: Expression;

  constructor(expression: Expression) {
    super();

    this.expression = expression;
  }
}

export class LiteralExpression extends Expression {
  value: string | number | boolean | undefined;

  constructor(value: string | number | boolean | undefined) {
    super();

    this.value = value;
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
}

export class SuperExpression extends Expression {
  name: Token;
  method: Token;

  constructor(name: Token, method: Token) {
    super();

    this.name = name;
    this.method = method;
  }
}

export class ThisExpression extends Expression {
  keyword: Token;

  constructor(keyword: Token) {
    super();

    this.keyword = keyword;
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
}

export class VarExpression extends Expression {
  name: Token;

  constructor(name: Token) {
    super();

    this.name = name;
  }
}
