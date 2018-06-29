import * as AST from "./ast";

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

export class SymbolTable {
  builtins: { [key: string]: BuiltinSymbol };
  symbols: { [key: string]: VarSymbol };

  constructor() {
    this.builtins = BuiltinSymbol.getBuiltinSymbols();
    this.symbols = {};
  }

  define(name: string, type: BuiltinSymbol) {
    this.symbols[name] = new VarSymbol(name, type);
  }

  lookup(name: string) {
    return this.symbols[name];
  }
}

export class SymbolTableBuilder {
  ast: AST.ProgramNode;
  symbolTable: SymbolTable;

  constructor(ast: AST.ProgramNode) {
    this.ast = ast;
    this.symbolTable = new SymbolTable();
  }

  execute() {
    this.visit(this.ast);
  }

  visitProgram(node: AST.ProgramNode) {
    this.visit(node.block);
  }

  visitBlock(node: AST.BlockNode) {
    node.declaration.children.forEach(child =>
      this.visitVariableDeclaration(child)
    );
    this.visitCompound(node.compound);
  }

  visitVariableDeclaration({ name, type }: AST.VariableDeclarationNode) {
    let typeSymbol = this.symbolTable.builtins[type.token.value];
    this.symbolTable.define(name.token.value, typeSymbol);
  }

  visitCompound(node: AST.CompoundNode) {
    node.children.forEach(child => this.visit(child));
  }

  visitAssignment(node: AST.AssignmentNode) {}

  visit(node: AST.Node) {
    if (node instanceof AST.ProgramNode) {
      this.visitProgram(node);
    } else if (node instanceof AST.BlockNode) {
      this.visitBlock(node);
    } else if (node instanceof AST.VariableDeclarationNode) {
      this.visitVariableDeclaration(node);
    } else if (node instanceof AST.CompoundNode) {
      this.visitCompound(node);
    } else {
      throw new Error("Cannot find suitable visit");
    }
  }
}
