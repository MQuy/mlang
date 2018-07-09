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
import { TokenType, Token } from "./token";
import { SymbolTable } from "./symbolTable";
import { Functionable } from "./ast/function";
import { ReturnError } from "./ast/base";
import { Classable } from "./ast/class";
import { Instance } from "./ast/instance";

export class Interpreter implements StatementVistor, ExpressionVistor {
  statements: Statement[];
  symbolTable: SymbolTable;
  locals: { [key: number]: number };

  constructor(statements: Statement[]) {
    this.statements = statements;
    this.symbolTable = new SymbolTable();
    this.locals = {};
  }

  interpret() {
    this.statements.forEach(statement => this.execute(statement));
  }

  resolve(expression: Expression, depth: number) {
    this.locals[expression.hash] = depth;
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

  visitVarExpression(varExpression: VarExpression) {
    return this.lookupVariable(varExpression.name, varExpression);
  }

  visitAssignExpression(assign: AssignExpression) {
    const value = this.evaluate(assign.expression);

    this.symbolTable.assignAt(assign.hash, assign.name, value);
    return value;
  }

  visitExpressionStatement(expressionStatement: ExpressionStatement) {
    return this.evaluate(expressionStatement.expression);
  }

  visitVarStatement(varStatement: VarStatement) {
    let value;

    if (varStatement.initializer) {
      value = this.evaluate(varStatement.initializer);
    }
    this.symbolTable.define(varStatement.name, value);
  }

  visitBlockStatement(blockStatement: BlockStatement) {
    this.executeBlock(
      blockStatement.statements,
      new SymbolTable(this.symbolTable)
    );
  }

  visitPrintStatement(printStatement: PrintStatement) {
    const value = this.evaluate(printStatement.expression);

    console.log(value);
  }

  visitIfStatement(ifStatement: IfStatement) {
    if (this.evaluate(ifStatement.condition)) {
      this.execute(ifStatement.thenBranch);
    } else if (ifStatement.elseBranch) {
      this.execute(ifStatement.elseBranch);
    }
  }

  visitWhileStatement(whileStatement: WhileStatement) {
    while (
      whileStatement.condition &&
      this.evaluate(whileStatement.condition)
    ) {
      this.execute(whileStatement.body);
    }
  }

  visitCallExpression(callExpression: CallExpression) {
    const callee = this.evaluate(callExpression.callee);
    const args = callExpression.arguments.map(argument =>
      this.evaluate(argument)
    );

    return callee.invoke(this, args);
  }

  visitFunctionStatement(functionStatement: FunctionStatement) {
    const func = new Functionable(
      functionStatement.name.lexeme,
      functionStatement,
      this.symbolTable
    );

    this.symbolTable.define(functionStatement.name, func);
  }

  visitReturnStatement(returnStatement: ReturnStatement) {
    let value;

    if (returnStatement.value) {
      value = this.evaluate(returnStatement.value);
    }
    throw new ReturnError(value);
  }

  visitClassStatement(classStatement: ClassStatement) {
    this.symbolTable.define(classStatement.name, undefined);
    const methods: { [key: string]: Functionable } = {};

    classStatement.methods.forEach(method => {
      methods[method.name.lexeme] = new Functionable(
        method.name.lexeme,
        method,
        this.symbolTable
      );
    });
    const klass = new Classable(classStatement.name.lexeme, methods);
    this.symbolTable.assign(classStatement.name, klass);
  }

  visitGetExpression(getExpression: GetExpression) {
    const object = this.evaluate(getExpression.object);

    if (object instanceof Instance) {
      return object.get(getExpression.name);
    }

    throw new Error(
      `${getExpression.name.lexeme} only instances have properties.`
    );
  }

  visitSetExpression(setExpression: SetExpression) {
    const object = this.evaluate(setExpression.object);

    if (object instanceof Instance) {
      const value = this.evaluate(setExpression.expression);
      return object.set(setExpression.name, value);
    }

    throw new Error(`Only instances have fields.`);
  }

  evaluate(expression: Expression) {
    return expression.accept(this);
  }

  execute(statement: Statement) {
    statement.accept(this);
  }

  executeBlock(statements: Statement[], symbolTable: SymbolTable) {
    const currentScope = this.symbolTable;

    try {
      this.symbolTable = symbolTable;
      statements.forEach(statement => this.execute(statement));
    } finally {
      this.symbolTable = currentScope;
    }
  }

  lookupVariable(token: Token, expression: Expression) {
    const distance = this.locals[expression.hash];

    if (distance != null) {
      return this.symbolTable.getAt(distance, token);
    } else {
      throw new Error(`Cannot find ${token.toString()}`);
    }
  }

  visitThisExpression(thisExpression: ThisExpression) {
    return this.lookupVariable(thisExpression.keyword, thisExpression);
  }

  visitSuperExpression(superExpression: SuperExpression) {}
}

/*
tokens = new Lexer(`
class Cake {
  taste() {
    var adjective = "delicious";
    print "The " + this.flavor + " cake is " + adjective + "!";
  }
}

var cake = Cake();
cake.flavor = "German chocolate";
cake.taste(); // Prints "The German chocolate cake is delicious!".
`).scan();
ast = new Parser(tokens).parse();
interpreter = new Interpreter(ast);
resolver = new Resolver(interpreter);
resolver.resolve();
interpreter.interpret();
*/
