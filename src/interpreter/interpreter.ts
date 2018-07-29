import {
  StatementVisitor,
  IfStatement,
  BlockStatement,
  BreakStatement,
  ContinueStatement,
  ForStatement,
  VarStatement,
  VarsStatement,
  ClassStatement,
  FunctionStatement,
  ReturnStatement,
  ExpressionStatement,
  Statement,
} from "../ast/statement";
import {
  ExpressionVisitor,
  AssignmentExpression,
  LogicalExpression,
  BinaryExpression,
  UnaryExpression,
  CallExpression,
  GetExpression,
  SetExpression,
  LiteralExpression,
  GroupExpression,
  LambdaExpression,
  TupleExpression,
  NewExpression,
  ArrayExpression,
  ThisExpression,
  SuperExpression,
  VarExpression,
  Expression,
} from "../ast/expression";
import { Program } from "../ast/program";
import { SymbolTable } from "./symbolTable";
import { Classable } from "./classable";
import { Functionable } from "./functionable";
import { ReturnCall, BreakCall, ContinueCall } from "../utils/error";
import { TokenType, Token } from "../token";

export class Interpreter implements StatementVisitor, ExpressionVisitor {
  program: Program;
  scope: SymbolTable;

  constructor(program: Program) {
    this.program = program;
  }

  interpret() {
    this.scope = new SymbolTable();
    this.program.statements.forEach(this.execute);
  }

  visitIfStatement(statement: IfStatement) {
    if (this.evaluate(statement.condition)) {
      this.execute(statement.thenStatement);
    } else {
      this.execute(statement.elseStatement);
    }
  }

  visitBlockStatement(statement: BlockStatement) {
    this.beginScope();
    statement.statements.forEach(this.execute);
    this.endScope();
  }

  visitForStatement(statement: ForStatement) {
    this.beginScope();
    if (statement.initializer) {
      const initializers: any = statement.initializer || [];
      initializers.forEach(
        initializer =>
          initializer instanceof VarStatement
            ? this.execute(initializer)
            : this.evaluate(initializer),
      );
    }
    while (true) {
      if (!this.evaluate(statement.condition)) {
        break;
      }
      try {
        this.execute(statement.body);
      } catch (e) {
        if (e instanceof BreakCall) {
          break;
        } else if (e instanceof ContinueCall) {
          continue;
        } else if (e instanceof ReturnCall) {
          throw e;
        }
      }
      this.execute(statement.increment);
    }
    this.endScope();
  }

  visitVarStatement(statement: VarStatement) {
    const value = this.evaluate(statement.initializer);
    this.scope.define(statement.name.lexeme, value);
  }

  visitVarsStatements(statement: VarsStatement) {
    statement.varStatements.forEach(this.execute);
  }

  visitClassStatement(statement: ClassStatement) {
    const klass = new Classable(statement.name.lexeme);
    this.scope.define(statement.name.lexeme, klass);

    this.beginScope();
    if (statement.superclass) {
      klass.superclass = this.scope.lookup(statement.superclass.lexeme);
    }
    if (statement.properties) {
      statement.properties.forEach(property => {
        klass.properties[property.name.lexeme] = this.evaluate(
          property.initializer,
        );
      });
    }
    if (statement.methods) {
      statement.methods.forEach(method => {
        klass.methods[method.name.lexeme] = new Functionable(
          method.name.lexeme,
          method,
          this.scope,
        );
      });
    }
    this.endScope();
  }

  visitFunctionStatement(statement: FunctionStatement) {
    const kunction = new Functionable(
      statement.name.lexeme,
      statement,
      this.scope,
    );
    this.scope.define(statement.name.lexeme, kunction);
  }

  visitReturnStatement(statement: ReturnStatement) {
    const value = this.evaluate(statement.value);
    throw new ReturnCall(value);
  }

  visitBreakStatement(statement: BreakStatement) {
    throw new BreakCall();
  }

  visitContinueStatement(statement: ContinueStatement) {
    throw new ContinueCall();
  }

  visitExpressionStatement(statement: ExpressionStatement) {
    this.evaluate(statement.expression);
  }

  visitAssignmentExpression(expression: AssignmentExpression) {
    const object: VarExpression = this.evaluate(expression.object);
    const value = this.evaluate(expression.expression);
    this.scope.define(object.name.lexeme, value);
    return value;
  }

  visitLogicalExpression(expression: LogicalExpression) {
    const left = this.evaluate(expression.left);
    const right = this.evaluate(expression.right);

    switch (expression.operator.type) {
      case TokenType.AND:
        return left && right;
      case TokenType.OR:
        return left || right;
      case TokenType.EQUAL_EQUAL:
        return left === right;
      case TokenType.BANG_EQUAL:
        return left !== right;
      case TokenType.LESS:
        return left < right;
      case TokenType.LESS_THAN:
        return left <= right;
      case TokenType.GREAT:
        return left > right;
      case TokenType.GREAT_THAN:
        return left >= right;
    }
  }

  visitBinaryExpression(expression: BinaryExpression) {
    const left = this.evaluate(expression.left);
    const right = this.evaluate(expression.right);

    switch (expression.operator.type) {
      case TokenType.PLUS:
        return left + right;
      case TokenType.MINUS:
        return left - right;
      case TokenType.STAR:
        return left * right;
      case TokenType.SLASH:
        return left / right;
      case TokenType.STAR_STAR:
        return left ** right;
    }
  }

  visitUnaryExpression(expression: UnaryExpression) {
    const value = this.evaluate(expression.right);

    switch (expression.operator.type) {
      case TokenType.BANG:
        return !value;
      case TokenType.PLUS:
        return +value;
      case TokenType.PLUS_PLUS:
        return value + 1;
      case TokenType.MINUS:
        return -value;
      case TokenType.MINUS_MINUS:
        return value - 1;
    }
  }

  visitCallExpression(expression: CallExpression) {}
  visitGetExpression(expression: GetExpression) {}
  visitSetExpression(expression: SetExpression) {}

  visitLiteralExpression(expression: LiteralExpression) {
    return expression.name.literal;
  }

  visitGroupExpression(expression: GroupExpression) {
    return this.evaluate(expression.expression);
  }

  visitLambdaExpression(expression: LambdaExpression) {}
  visitTupleExpression(expression: TupleExpression) {}
  visitNewExpression(expression: NewExpression) {}
  visitArrayExpression(expression: ArrayExpression) {}

  visitThisExpression(expression: ThisExpression) {
    return this.scope.lookup("this");
  }

  visitSuperExpression(expression: SuperExpression) {
    return this.scope.lookup("super");
  }

  visitVarExpression(expression: VarExpression) {
    this.scope.lookup(expression.name.lexeme);
  }

  evaluate = (expression?: Expression) => {
    return expression ? expression.accept(this) : undefined;
  };

  execute = (statement?: Statement) => {
    statement && statement.accept(this);
  };

  beginScope() {
    const currentScope = new SymbolTable(this.scope);
    const enclosing = this.scope;

    this.scope = currentScope;
    return enclosing;
  }

  endScope() {
    if (this.scope.enclosing) {
      this.scope = this.scope.enclosing;
    } else {
      throw new Error("There is something wrong with scope");
    }
  }
}
