import * as AST from "./ast";
import { Visitor } from "./visitor";

export class VarSymbol {
  name: string;
  type: BuiltinSymbol;

  constructor(name: string, type: BuiltinSymbol) {
    this.name = name;
    this.type = type;
  }
}

export class BuiltinSymbol {
  name: string;

  constructor(name: string) {
    this.name = name;
  }

  static getBuiltinSymbols() {
    return {
      INTEGER: new BuiltinSymbol("INTEGER"),
      REAL: new BuiltinSymbol("REAL")
    };
  }
}

export class ProcedureSymbol {
  name: string;
  parameters: VarSymbol[];

  constructor(name: string, parameters: VarSymbol[]) {
    this.name = name;
    this.parameters = parameters;
  }
}

export class SymbolTable {
  symbols: { [key: string]: VarSymbol | ProcedureSymbol | BuiltinSymbol };
  scopeName: string;
  scopeLevel: number;
  parentScope?: SymbolTable;

  constructor(
    scopeName: string,
    scopeLevel: number,
    parentScope?: SymbolTable
  ) {
    this.scopeName = scopeName;
    this.scopeLevel = scopeLevel;
    this.parentScope = parentScope;
    this.symbols = {};
  }

  define(name: string, type: BuiltinSymbol | ProcedureSymbol) {
    this.symbols[name] = new VarSymbol(name, type);
  }

  lookup(name: string, level = Number.MAX_SAFE_INTEGER) {
    let scope: SymbolTable | undefined = this;

    while (scope && !scope.symbols[name] && level > 0) {
      scope = scope.parentScope;
      level -= 1;
    }

    return scope ? scope.symbols[name] : undefined;
  }
}

export class SemanticAnalyzer extends Visitor {
  ast: AST.ProgramNode;
  rootSymbolTable: SymbolTable;
  currentSymbolTable: SymbolTable;

  constructor(ast: AST.ProgramNode) {
    super();

    this.ast = ast;
    this.rootSymbolTable = new SymbolTable(ast.name, 1);
    this.rootSymbolTable.symbols = BuiltinSymbol.getBuiltinSymbols();
    this.currentSymbolTable = this.rootSymbolTable;
  }

  execute() {
    this.visit(this.ast);
  }

  visitProcedure({ name, paramters, block }: AST.ProcedureNode) {
    let procedureSymbol = new ProcedureSymbol(
      name,
      paramters.map(
        parameter =>
          new VarSymbol(
            parameter.name.token.value,
            this.currentSymbolTable.lookup(
              parameter.type.token.value
            ) as BuiltinSymbol
          )
      )
    );
    this.currentSymbolTable.define(name, procedureSymbol);
    let procedureScope = new SymbolTable(
      name,
      this.currentSymbolTable.scopeLevel + 1,
      this.currentSymbolTable
    );

    this.currentSymbolTable = procedureScope;
    paramters.forEach(parameter => {
      this.currentSymbolTable.define(
        parameter.name.token.value,
        this.currentSymbolTable.lookup(
          parameter.type.token.value
        ) as BuiltinSymbol
      );
    });

    this.visit(block);
    this.currentSymbolTable = procedureScope.parentScope!;
  }

  visitVariableDeclaration({ name, type }: AST.VariableDeclarationNode) {
    if (this.currentSymbolTable.lookup(name.token.value, 0)) {
      throw new Error(`${name.token.value} is already declared`);
    }
    let typeSymbol = this.currentSymbolTable.lookup(
      type.token.value
    ) as BuiltinSymbol;
    this.currentSymbolTable.define(name.token.value, typeSymbol);
  }

  visitAssignment({ variable, expression }: AST.AssignmentNode) {
    if (!this.currentSymbolTable.lookup(variable.token.value)) {
      throw new Error(`${variable.token.value} is not defined`);
    }
    this.visit(expression);
  }

  visitVariable(node: AST.TokenNode) {
    if (!this.currentSymbolTable.lookup(node.token.value)) {
      throw new Error(`${node.token.value} is not defined`);
    }
  }
}
