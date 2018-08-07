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
import { SymbolTable } from "./symbolTable";
import {
  checkTypeAssignable,
  BuiltinTypes,
  Classable,
  Functionable,
  leastUpperBound,
} from "./types";
import { TokenType } from "../token";
import { error } from "../utils/print";

export class TypeChecking implements StatementVisitor, ExpressionVisitor {
  program: Program;
  scope: SymbolTable;

  constructor(program: Program, scope?: SymbolTable) {
    this.program = program;
    this.scope = scope || new SymbolTable(undefined, BuiltinTypes);
  }

  run() {
    this.program.statements.forEach(statement => this.execute(statement));
  }

  visitIfStatement(statement: IfStatement) {
    this.evaluate(statement.condition);
    this.execute(statement.thenStatement);
    this.execute(statement.elseStatement);
  }

  visitBlockStatement(statement: BlockStatement) {
    this.beginScope();
    statement.statements.forEach(statement => this.execute(statement));
    this.endScope();
  }

  visitForStatement(statement: ForStatement) {
    this.beginScope();

    const initializers: any = statement.initializer || [];
    initializers.forEach(
      declaration =>
        declaration instanceof VarStatement
          ? this.execute(declaration)
          : this.evaluate(declaration),
    );

    this.evaluate(statement.condition);

    this.beginScope();
    this.execute(statement.body);
    this.endScope();

    this.execute(statement.increment);
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
      const superType = this.scope.lookup(statement.superclass.lexeme);
      if (superType instanceof Classable) {
        klass.superclass = superType;
        this.scope.define("super", klass.superclass);
      } else {
        error(statement.superclass, "super has to be classname");
      }
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
    this.evaluate(statement.expression);
  }

  visitReturnStatement(statement: ReturnStatement) {
    const kunction = this.scope.lookup("this");

    if (kunction instanceof Functionable) {
      const type = this.evaluate(statement.value);
      checkTypeAssignable(type, kunction.returnType, statement.pStart);
    } else {
      error(statement.pStart, "cannot return without function's scope");
    }
  }

  visitAssignmentExpression(expression: AssignmentExpression) {
    const expressionType = this.evaluate(expression.expression);
    const variableType = this.scope.lookup(expression.name.lexeme);

    checkTypeAssignable(expressionType, variableType, expression.pStart);
    return variableType;
  }

  visitLogicalExpression(expression: LogicalExpression) {
    return expression.type;
  }

  visitBinaryExpression(expression: BinaryExpression) {
    const leftType = this.evaluate(expression.left);
    const rightType = this.evaluate(expression.right);

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
      error(
        expression.operator,
        "Only can do binary operator for number and string type",
      );
    }
  }

  visitUnaryExpression(expression: UnaryExpression) {
    this.evaluate(expression.right);

    switch (expression.operator.type) {
      case TokenType.BANG:
        return (expression.type = BuiltinTypes.Boolean);
      case TokenType.PLUS:
      case TokenType.PLUS_PLUS:
      case TokenType.MINUS:
      case TokenType.MINUS_MINUS:
        return (expression.type = BuiltinTypes.Number);
      default:
        error(expression.operator, "Unknow operator");
    }
  }

  visitCallExpression(expression: CallExpression) {
    const type = this.evaluate(expression.callee);

    if (type instanceof Functionable) {
      type.parameters.forEach((parameterType, index) => {
        const argType = this.evaluate(expression.args[index]);

        checkTypeAssignable(argType, parameterType, expression.pStart);
      });
      return type.returnType;
    } else {
      error(expression.callee.pStart, "is not callable");
    }
  }

  visitGetExpression(expression: GetExpression) {
    const type: Classable = this.evaluate(expression.object);
    return type.get(expression.name.lexeme);
  }

  visitSetExpression(expression: SetExpression) {
    const object: Classable = this.evaluate(expression.object);

    if (object instanceof Classable) {
      const type = object.get(expression.name.lexeme);
      const setType = this.evaluate(expression.value);

      checkTypeAssignable(setType, type, expression.name);
      return type;
    } else {
      error(expression.name, "cannot set");
    }
  }

  visitLiteralExpression(expression: LiteralExpression) {
    return expression.type as BuiltinTypes;
  }

  visitGroupExpression(expression: GroupExpression) {
    this.evaluate(expression.expression);
    return (expression.type = expression.expression.type);
  }

  visitLambdaExpression(expression: LambdaExpression) {
    const kunction = new Functionable(this.scope.lookup(expression.returnType));

    expression.parameters.forEach(parameter => {
      const parameterType = this.scope.lookup(parameter.type);

      kunction.parameters.push(parameterType);
      this.scope.define(parameter.name.lexeme, parameterType);
    });

    this.beginScope();
    this.scope.define("this", kunction);
    this.execute(expression.body);
    this.endScope();

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

  visitArrayExpression(expression: ArrayExpression) {
    if (expression.elements.length > 0) {
      let baseType = this.evaluate(expression.elements[0]);
      for (let i = 1; i < expression.elements.length; ++i) {
        const otherType = this.evaluate(expression.elements[i]);
        const tempType = leastUpperBound(otherType, baseType);
        if (tempType) {
          baseType = tempType;
        } else {
          error(expression.elements[i].pStart, "Not same type");
        }
      }
      return new Array(expression.elements.length).fill(baseType);
    }
    return [];
  }

  visitTupleExpression(expression: TupleExpression) {}

  // NOTE: Don't need to check type for these statements
  visitPrintStatement(statement: PrintStatement) {}
  visitBreakStatement(statement: BreakStatement) {}
  visitContinueStatement(statement: ContinueStatement) {}

  execute(statement?: Statement) {
    statement && statement.accept(this);
  }

  evaluate(expression?: Expression) {
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

  checkVarStatement(
    statement: VarStatement,
    define: (name: string, type: any) => void,
  ) {
    const initializerType = this.evaluate(statement.initializer);

    if (statement.type) {
      const variableType = this.scope.lookup(statement.type);
      checkTypeAssignable(initializerType, variableType, statement.name);
      define(statement.name.lexeme, variableType);
    } else if (initializerType) {
      define(statement.name.lexeme, initializerType);
    } else {
      error(statement.name, "cannot declare without type");
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
      const parameterType = this.scope.lookup(parameter.type);

      kunction.parameters.push(parameterType);
      this.scope.define(parameter.name.lexeme, parameterType);
    });

    define(statement.name.lexeme, kunction);

    this.beginScope();
    this.scope.define("this", kunction);
    this.execute(statement.body);
    this.endScope();
  }
}
