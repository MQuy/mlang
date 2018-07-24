import { Program } from "../ast/program";
import {
  StatementVisitor,
  Statement,
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
import { SymbolTable } from "./symbolTable";
import { checkInheritance, BuiltinTypes } from "./types";
import { TokenType } from "../token";
import { Classable } from "./classable";
import { Functionable } from "./functionable";

export class TypeChecking implements StatementVisitor, ExpressionVisitor {
  program: Program;
  scope: SymbolTable;

  constructor(program: Program) {
    this.program = program;
  }

  run() {
    this.scope = new SymbolTable(undefined, BuiltinTypes);
    this.program.statements.forEach(statement =>
      this.evaluateStatement(statement),
    );
  }

  visitIfStatement(statement: IfStatement) {
    this.evaluateExpression(statement.condition);
    this.evaluateStatement(statement.thenStatement);
    this.evaluateStatement(statement.elseStatement);
  }

  visitBlockStatement(statement: BlockStatement) {
    this.beginScope();
    statement.statements.forEach(statement =>
      this.evaluateStatement(statement),
    );
    this.endScope();
  }

  visitForStatement(statement: ForStatement) {
    this.beginScope();

    const initializers: any = statement.initializer || [];
    initializers.forEach(
      declaration =>
        declaration instanceof VarStatement
          ? this.evaluateStatement(declaration)
          : this.evaluateExpression(declaration),
    );

    this.evaluateExpression(statement.condition);

    this.beginScope();
    this.evaluateStatement(statement.body);
    this.endScope();

    this.evaluateStatement(statement.increment);
    this.endScope();
  }

  visitVarStatement(statement: VarStatement) {
    const initializerType = this.evaluateExpression(statement.initializer);

    if (statement.type) {
      const variableType = this.scope.lookup(statement.type);
      checkInheritance(initializerType, variableType);
      this.scope.define(statement.name.lexeme, variableType);
    } else {
      this.scope.define(statement.name.lexeme, initializerType);
    }
  }

  visitVarsStatements(statement: VarsStatement) {
    statement.varStatements.forEach(varStatement =>
      this.visitVarStatement(varStatement),
    );
  }

  visitClassStatement(statement: ClassStatement) {
    const klass = new Classable(statement.name.lexeme);
    this.scope.define(statement.name.lexeme, klass);

    this.beginScope();

    this.scope.define("this", klass);
    if (statement.superclass) {
      klass.superclass = this.scope.lookup(statement.superclass.lexeme);
      this.scope.define("super", klass.superclass);
    }

    if (statement.properties) {
      statement.properties.forEach(property => {
        const initializerType = this.evaluateExpression(property.initializer);
        const variableType = this.scope.lookup(property.type);
        checkInheritance(initializerType, variableType);
        klass.properties[property.name.lexeme] = variableType;
      });
    }

    if (statement.methods) {
      statement.methods.forEach(method => {
        const kethod = new Functionable(
          method.name.lexeme,
          this.scope.lookup(method.returnType),
        );

        method.parameters.forEach(parameter => {
          kethod.parameters.push(this.scope.lookup(parameter.type));
        });

        klass.methods[method.name.lexeme] = kethod;

        this.evaluateStatement(method.body);
      });
    }

    this.endScope();
  }

  visitFunctionStatement(statement: FunctionStatement) {
    const kunction = new Functionable(
      statement.name.lexeme,
      this.scope.lookup(statement.returnType),
    );

    this.scope.define(statement.name.lexeme, kunction);

    statement.parameters.forEach(parameter => {
      kunction.parameters[parameter.type] = this.scope.lookup(parameter.type);
    });

    this.beginScope();
    this.evaluateStatement(statement.body);
    this.endScope();
  }

  visitExpressionStatement(statement: ExpressionStatement) {
    this.evaluateExpression(statement.expression);
  }

  // TODO:
  visitReturnStatement(statement: ReturnStatement) {
    // const type = this.evaluateExpression(statement.value);
  }

  visitAssignmentExpression(expression: AssignmentExpression) {
    const expressionType = this.evaluateExpression(expression.expression);
    const variableType = this.evaluateExpression(expression.object);

    checkInheritance(expressionType, variableType);
    return variableType;
  }

  visitLogicalExpression(expression: LogicalExpression) {
    return expression.type;
  }

  visitBinaryExpression(expression: BinaryExpression) {
    const leftType = this.evaluateExpression(expression.left);
    const rightType = this.evaluateExpression(expression.right);

    if (
      leftType === BuiltinTypes.String &&
      rightType === BuiltinTypes.String &&
      expression.operator.type === TokenType.PLUS
    ) {
      return (expression.type = BuiltinTypes.String);
    } else if (
      leftType === BuiltinTypes.Number &&
      rightType === BuiltinTypes.Number
    ) {
      return (expression.type = BuiltinTypes.Number);
    } else {
      throw new Error("Only do binary expression in number or string type");
    }
  }

  visitUnaryExpression(expression: UnaryExpression) {
    this.evaluateExpression(expression.right);

    switch (expression.operator.type) {
      case TokenType.BANG:
        return (expression.type = BuiltinTypes.Boolean);
      case TokenType.PLUS:
      case TokenType.PLUS_PLUS:
      case TokenType.MINUS:
      case TokenType.MINUS_MINUS:
        return (expression.type = BuiltinTypes.Number);
      default:
        throw new Error(`Unknow operator ${expression.operator.type}`);
    }
  }

  visitCallExpression(expression: CallExpression) {
    const type = this.evaluateExpression(expression.callee);

    if (type instanceof Functionable) {
      type.parameters.forEach((parameterType, index) => {
        const argType = this.evaluateExpression(expression.args[index]);

        checkInheritance(argType, parameterType);
      });
      return type.returnType;
    } else {
      throw new Error("is not callable");
    }
  }

  visitGetExpression(expression: GetExpression) {
    const type: Classable = this.evaluateExpression(expression.object);
    return type.get(expression.name.lexeme);
  }

  visitSetExpression(expression: SetExpression) {
    const object: Classable = this.evaluateExpression(expression.object);

    if (object instanceof Classable) {
      const type = object.get(expression.name.lexeme);
      const setType = this.evaluateExpression(expression.value);

      checkInheritance(setType, type);
      return type;
    } else {
      throw new Error("cannot set");
    }
  }

  visitLiteralExpression(expression: LiteralExpression) {
    return expression.type;
  }

  visitGroupExpression(expression: GroupExpression) {
    this.evaluateExpression(expression.expression);
    return (expression.type = expression.expression.type);
  }

  visitLambdaExpression(expression: LambdaExpression) {
    const kunction = new Functionable(
      undefined,
      this.scope.lookup(expression.returnType),
    );
    expression.parameters.forEach(parameter =>
      kunction.parameters.push(this.scope.lookup(parameter.type)),
    );
    return kunction;
  }

  visitNewExpression(expression: NewExpression) {
    return this.scope.lookup(expression.name.lexeme);
  }

  visitThisExpression(expression: ThisExpression) {
    return this.scope.lookup(expression.keyword.lexeme);
  }

  visitSuperExpression(expression: SuperExpression) {
    return this.scope.lookup(expression.keyword.lexeme);
  }

  visitVarExpression(expression: VarExpression) {
    return this.scope.lookup(expression.name.lexeme);
  }

  visitArrayExpression(expression: ArrayExpression) {}
  visitTupleExpression(expression: TupleExpression) {}

  visitBreakStatement(statement: BreakStatement) {}
  visitContinueStatement(statement: ContinueStatement) {}

  evaluateStatement(statement?: Statement) {
    statement && statement.accept(this);
  }

  evaluateExpression(expression?: Expression) {
    return expression && expression.accept(this);
  }

  beginScope() {
    const enclosing = this.scope;

    this.scope = new SymbolTable(enclosing);
    return enclosing;
  }

  endScope() {
    if (this.scope.enclosing) {
      this.scope = this.scope.enclosing;
    }
  }
}
