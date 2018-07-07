import {
  StatementVistor,
  ExpressionVistor,
  LiteralExpression,
  GroupingExpression,
  Expression,
  UnaryExpression,
  BinaryExpression,
  LogicalExpression,
  AssignExpression,
  BlockStatement,
  CallExpression,
  ClassStatement,
  ExpressionStatement,
  FunctionStatement,
  GetExpression,
  IfStatement,
  PrintStatement,
  ReturnStatement,
  SetExpression,
  SuperExpression,
  ThisExpression,
  VarExpression,
  VarStatement,
  WhileStatement,
  Statement
} from "./ast";
import { TokenType } from "./token";

export class Interpreter implements StatementVistor, ExpressionVistor {
  statements: Statement[];

  constructor(statements: Statement[]) {
    this.statements = statements;
  }

  interpret() {
    this.statements.forEach(statement => this.execute(statement));
  }

  visitLiternalExpression(liternal: LiteralExpression) {
    return liternal.value;
  }

  visitGroupingExpression(group: GroupingExpression) {
    return this.evaluate(group.expression);
  }

  visitUnaryExpression(unary: UnaryExpression) {
    const right = this.evaluate(unary.right);

    switch (unary.opeartor.type) {
      case TokenType.MINUS:
        return -right;
      case TokenType.BANG:
        return !right;
      default:
        return right;
    }
  }

  visitBinaryExpression(binary: BinaryExpression) {
    const left = this.evaluate(binary.left);
    const right = this.evaluate(binary.right);

    switch (binary.operator.type) {
      case TokenType.MINUS:
        return left + right;
      case TokenType.PLUS:
        return left + right;
      case TokenType.STAR:
        return left * right;
      case TokenType.SLASH:
        return left / right;
      case TokenType.EQUAL_EQUAL:
        return left === right;
      case TokenType.BANG_EQUAL:
        return left !== right;
      case TokenType.GREATER:
        return left > right;
      case TokenType.GREATER_EQUAL:
        return left >= right;
      case TokenType.LESS:
        return left < right;
      case TokenType.LESS_EQUAL:
        return left <= right;
    }
  }

  visitLogicalExpression(logical: LogicalExpression) {
    const left = this.evaluate(logical.left);
    const right = this.evaluate(logical.right);

    switch (logical.opeartor.type) {
      case TokenType.AND:
        return left && right;
      case TokenType.OR:
        return left || right;
    }
  }

  visitExpressionStatement(expressionStatement: ExpressionStatement) {
    return this.evaluate(expressionStatement.expression);
  }

  evaluate(expression: Expression) {
    return expression.accept(this);
  }

  execute(statement: Statement) {
    statement.accept(this);
  }

  visitAssignExpression(assign: AssignExpression) {}
  visitCallExpression(call: CallExpression) {}
  visitGetExpression(getExpression: GetExpression) {}
  visitSetExpression(setExpression: SetExpression) {}
  visitSuperExpression(superExpression: SuperExpression) {}
  visitThisExpression(thisExpression: ThisExpression) {}
  visitVarExpression(varExpression: VarExpression) {}
  visitVarStatement(varStatement: VarStatement) {}
  visitWhileStatement(whileStatement: WhileStatement) {}
  visitBlockStatement(block: BlockStatement) {}
  visitClassStatement(classStatement: ClassStatement) {}
  visitIfStatement(ifStatement: IfStatement) {}
  visitFunctionStatement(functionStatement: FunctionStatement) {}
  visitPrintStatement(printStatement: PrintStatement) {}
  visitReturnStatement(returnStatement: ReturnStatement) {}
}
