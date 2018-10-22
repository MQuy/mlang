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
  PrintStatement,
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
import { TokenType } from "../token";
import { Instance } from "./instance";
import { Lambda } from "./lambda";
import { error } from "../utils/print";

export class Interpreter implements StatementVisitor, ExpressionVisitor {
  program: Program;
  scope: SymbolTable;

  constructor(program: Program, scope?: SymbolTable) {
    this.program = program;
    this.scope = scope || new SymbolTable();
  }

  interpret() {
    this.program.statements.forEach(this.execute);
  }

  visitIfStatement(statement: IfStatement) {
    if (this.evaluate(statement.condition)) {
      return this.execute(statement.thenStatement);
    } else {
      return this.execute(statement.elseStatement);
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
          method.parameters,
          method.body,
          this.scope,
          method.name.lexeme,
        );
      });
    }
    this.endScope();
    return klass;
  }

  visitFunctionStatement(statement: FunctionStatement) {
    const kunction = new Functionable(
      statement.parameters,
      statement.body,
      this.scope,
      statement.name.lexeme,
    );
    this.scope.define(statement.name.lexeme, kunction);
    return kunction;
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

  visitPrintStatement(statement: PrintStatement) {
    const value = this.evaluate(statement.expression);
    console.log(value);
  }

  visitExpressionStatement(statement: ExpressionStatement) {
    return this.evaluate(statement.expression);
  }

  visitAssignmentExpression(expression: AssignmentExpression) {
    const value = this.evaluate(expression.expression);
    this.scope.assign(expression.name.lexeme, value);
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
      case TokenType.MINUS:
        return -value;
      case TokenType.PLUS_PLUS:
      case TokenType.MINUS_MINUS:
        const newValue =
          expression.operator.type === TokenType.PLUS_PLUS
            ? value + 1
            : value - 1;
        if (expression.right instanceof VarExpression) {
          this.scope.define(expression.right.name.lexeme, newValue);
        } else {
          error(expression.right.pStart, "has to be variable name");
        }
        return newValue;
    }
  }

  visitCallExpression(expression: CallExpression) {
    const callee = this.evaluate(expression.callee);

    if (callee instanceof Functionable || callee instanceof Lambda) {
      return callee.invoke(this, expression.args.map(this.evaluate));
    }
  }

  visitGetExpression(expression: GetExpression) {
    const instance = this.evaluate(expression.object);

    if (instance instanceof Instance) {
      return instance.get(expression.name.lexeme);
    } else {
      error(expression.pStart, "has to be instance");
    }
  }

  visitSetExpression(expression: SetExpression) {
    const instance = this.evaluate(expression.object);
    const value = this.evaluate(expression.value);

    if (instance instanceof Instance) {
      instance.set(expression.name.lexeme, value);
      return value;
    } else {
      error(expression.pStart, "has to be instance");
    }
  }

  visitLiteralExpression(expression: LiteralExpression) {
    return expression.name.literal;
  }

  visitGroupExpression(expression: GroupExpression) {
    return this.evaluate(expression.expression);
  }

  visitNewExpression(expression: NewExpression) {
    const klass = this.scope.lookup(expression.name.lexeme);

    if (klass instanceof Classable) {
      return new Instance(klass);
    } else {
      error(expression.name, "has to be class name");
    }
  }

  visitLambdaExpression(expression: LambdaExpression) {
    return new Lambda(expression.parameters, expression.body, this.scope);
  }

  visitThisExpression(expression: ThisExpression) {
    return this.scope.lookup("this");
  }

  visitSuperExpression(expression: SuperExpression) {
    return this.scope.lookup("super");
  }

  visitVarExpression(expression: VarExpression) {
    return this.scope.lookup(expression.name.lexeme);
  }

  visitTupleExpression(expression: TupleExpression) {}
  visitArrayExpression(expression: ArrayExpression) {}

  evaluate = (expression?: Expression) => {
    return expression ? expression.accept(this) : undefined;
  };

  execute = (statement?: Statement) => {
    return statement ? statement.accept(this) : undefined;
  };

  executeFunctionBody(statement: Statement, scope: SymbolTable) {
    const currentScope = this.scope;

    try {
      this.scope = scope;
      statement.accept(this);
    } finally {
      this.scope = currentScope;
    }
  }

  beginScope() {
    const newScope = new SymbolTable(this.scope);
    const currentScope = this.scope;

    this.scope = newScope;
    return currentScope;
  }

  endScope() {
    if (this.scope.enclosing) {
      this.scope = this.scope.enclosing;
    } else {
      throw new Error("There is something wrong with scope");
    }
  }
}
