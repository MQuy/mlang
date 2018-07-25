import { Types } from "./types";

export class SymbolTable {
  enclosing?: SymbolTable;
  symbols: { [name: string]: Types };

  constructor(enclosing?: SymbolTable, symbols = {}) {
    this.enclosing = enclosing;
    this.symbols = symbols;
  }

  define(name: string, value: Types) {
    this.symbols = { ...this.symbols, [name]: value };
  }

  lookup(name?: string, depth = Number.MAX_SAFE_INTEGER) {
    let scope: SymbolTable | undefined = this;

    if (name) {
      for (let i = 0; i <= depth && scope; ++i) {
        if (Object.keys(scope.symbols).includes(name)) {
          return scope.symbols[name];
        }
        scope = scope.enclosing;
      }
    }
    throw new Error(`Cannot find ${name}`);
  }
}
