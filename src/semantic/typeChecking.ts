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
import {
  checkInheritance,
  BuiltinTypes,
  Classable,
  Functionable,
} from "./types";
import { TokenType, Token } from "../token";

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
    this.checkVarStatement(statement, (name: string, type: any) =>
      this.scope.define(name, type),
    );
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
        this.checkVarStatement(
          property,
          (name: string, type: any) => (klass.properties[name] = type),
        );
      });
    }

    if (statement.methods) {
      statement.methods.forEach(method => {
        this.checkFunctionStatement(
          method,
          (name: string, type: Functionable) => (klass.methods[name] = type),
        );
      });
    }

    this.endScope();
  }

  visitFunctionStatement(statement: FunctionStatement) {
    this.checkFunctionStatement(statement, (name: string, type: Functionable) =>
      this.scope.define(name, type),
    );
  }

  visitExpressionStatement(statement: ExpressionStatement) {
    this.evaluateExpression(statement.expression);
  }

  visitReturnStatement(statement: ReturnStatement) {
    const kunction = this.scope.lookup("this");

    if (kunction instanceof Functionable) {
      const type = this.evaluateExpression(statement.value);
      checkInheritance(type, kunction.returnType);
    } else {
      throw new Error("return is not correct");
    }
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
      this.error(
        expression.operator,
        "Only can do binary operator for number and string type",
      );
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
        this.error(expression.operator, "Unknow operator");
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
      this.error(expression.name, "cannot set");
    }
  }

  visitLiteralExpression(expression: LiteralExpression) {
    return expression.type as BuiltinTypes;
  }

  visitGroupExpression(expression: GroupExpression) {
    this.evaluateExpression(expression.expression);
    return (expression.type = expression.expression.type);
  }

  visitLambdaExpression(expression: LambdaExpression) {
    const kunction = new Functionable(this.scope.lookup(expression.returnType));
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

  error(token: Token, errorMessage: string): never {
    throw new Error(`${token.toString()} ${errorMessage}`);
  }

  checkVarStatement(
    statement: VarStatement,
    define: (name: string, type: any) => void,
  ) {
    const initializerType = this.evaluateExpression(statement.initializer);

    if (statement.type) {
      const variableType = this.scope.lookup(statement.type);
      checkInheritance(initializerType, variableType);
      define(statement.name.lexeme, variableType);
    } else if (initializerType) {
      define(statement.name.lexeme, initializerType);
    } else {
      this.error(statement.name, "cannot declare without type");
    }
  }

  checkFunctionStatement(
    statement: FunctionStatement,
    define: (name: string, type: Functionable) => void,
  ) {
    const kunction = new Functionable(
      this.scope.lookup(statement.returnType),
      statement.name.lexeme,
    );

    statement.parameters.forEach(parameter => {
      kunction.parameters[parameter.type] = this.scope.lookup(parameter.type);
    });

    define(statement.name.lexeme, kunction);

    this.beginScope();
    this.scope.define("this", kunction);
    this.evaluateStatement(statement.body);
    this.endScope();
  }
}
