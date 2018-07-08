import {
  StatementVistor,
  ExpressionVistor,
  LiteralExpression,
  GroupingExpression,
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
  Statement,
  Expression
} from "./ast";
import { Interpreter } from "./interpreter";
import { Token } from "./token";

export class Resolver implements StatementVistor, ExpressionVistor {
  interpreter: Interpreter;
  scopes: { [key: string]: boolean }[];

  constructor(interpreter: Interpreter) {
    this.interpreter = interpreter;
  }

  resolve() {
    this.scopes = [];
    this.beginScope();
    this.resolveStatements(this.interpreter.statements);
    this.endScope();
  }

  resolveStatements(statements: Statement[]) {
    statements.forEach(statement => this.resolveStatement(statement));
  }

  visitBlockStatement(stms: BlockStatement) {
    this.beginScope();
    this.resolveStatements(stms.statements);
    this.endScope();
  }

  visitExpressionStatement(stms: ExpressionStatement) {
    this.resolveExpression(stms.expression);
  }

  visitFunctionStatement(stms: FunctionStatement) {
    this.declare(stms.name);
    this.define(stms.name);
    this.resolveFunction(stms);
  }

  visitIfStatement(stms: IfStatement) {
    this.resolveExpression(stms.condition);
    this.resolveStatement(stms.thenBranch);
    if (stms.elseBranch) this.resolveStatement(stms.elseBranch);
  }

  visitPrintStatement(stms: PrintStatement) {
    this.resolveExpression(stms.expression);
  }

  visitReturnStatement(stms: ReturnStatement) {
    if (stms.value) this.resolveExpression(stms.value);
  }

  visitVarStatement(smts: VarStatement) {
    this.declare(smts.name);
    if (smts.initializer) {
      this.resolveExpression(smts.initializer);
    }
    this.define(smts.name);
  }

  visitWhileStatement(stms: WhileStatement) {
    if (stms.condition) this.resolveExpression(stms.condition);
    this.resolveStatement(stms.body);
  }

  visitAssignExpression(expr: AssignExpression) {
    this.resolveExpression(expr.expression);
    this.resolveLocal(expr, expr.name);
  }

  visitBinaryExpression(expr: BinaryExpression) {
    this.resolveExpression(expr.left);
    this.resolveExpression(expr.right);
  }

  visitCallExpression(expr: CallExpression) {
    this.resolveExpression(expr.callee);
    expr.arguments.forEach(arg => this.resolveExpression(arg));
  }

  visitGetExpression(expr: GetExpression) {
    this.resolveExpression(expr.object);
  }

  visitGroupingExpression(expr: GroupingExpression) {
    this.resolveExpression(expr.expression);
  }

  visitLogicalExpression(expr: LogicalExpression) {
    this.resolveExpression(expr.left);
    this.resolveExpression(expr.right);
  }

  visitSetExpression(expr: SetExpression) {
    this.resolveExpression(expr.expression);
    this.resolveExpression(expr.object);
  }

  visitUnaryExpression(expr: UnaryExpression) {
    this.resolveExpression(expr.right);
  }

  visitVarExpression(expr: VarExpression) {
    this.resolveLocal(expr, expr.name);
  }

  resolveFunction(stms: FunctionStatement) {
    this.beginScope();
    stms.parameters.forEach(param => {
      this.declare(param);
      this.define(param);
    });
    this.resolveStatements(stms.methods);
    this.endScope();
  }

  declare(token: Token) {
    const scope = this.scopes[this.scopes.length - 1];

    if (scope) {
      if (scope[token.lexeme]) {
        throw new Error(
          `Variable ${token.lexeme} is already declared in this scope.`
        );
      } else {
        scope[token.lexeme] = false;
      }
    }
  }

  define(token: Token) {
    const scope = this.scopes[this.scopes.length - 1];

    if (scope) {
      scope[token.lexeme] = true;
    }
  }

  beginScope() {
    this.scopes.push({});
  }

  endScope() {
    this.scopes.pop();
  }

  resolveStatement(statement: Statement) {
    statement.accept(this);
  }

  resolveExpression(expression: Expression) {
    expression.accept(this);
  }

  resolveLocal(expression: Expression, token: Token) {
    debugger;
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      if (this.scopes[i][token.lexeme]) {
        this.interpreter.resolve(expression, this.scopes.length - 1 - i);
        return;
      }
    }
  }

  visitLiternalExpression(expr: LiteralExpression) {}
  visitSuperExpression(expr: SuperExpression) {}
  visitThisExpression(expr: ThisExpression) {}
  visitClassStatement(stms: ClassStatement) {}
}
