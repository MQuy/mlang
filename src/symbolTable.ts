import { Token } from "./token";

export class SymbolTable {
  enclosing?: SymbolTable;
  symbols: { [key: string]: any };

  constructor(enclosing?: SymbolTable) {
    this.enclosing = enclosing;
    this.symbols = {};
  }

  define(token: Token, value: any) {
    this.symbols[token.lexeme] = value;
  }

  assign(token: Token, value: any, onlyCurrentScope: boolean = false) {
    const key = token.lexeme;

    if (Object.keys(this.symbols).includes(key)) {
      this.symbols[key] = value;
    } else if (this.enclosing && !onlyCurrentScope) {
      this.enclosing.assign(token, value);
    } else {
      throw new Error(`Undefined variable ${key}.`);
    }
  }

  ancestor(distance) {
    let scope: SymbolTable = this;

    for (let i = 0; i < distance; ++i) {
      if (scope.enclosing) {
        scope = scope.enclosing;
      }
    }
    return scope;
  }

  getAt(distance: number, token: Token) {
    return this.ancestor(distance).lookup(token, true);
  }

  assignAt(distance: number, token: Token, value: any) {
    this.ancestor(distance).assign(token, value, true);
  }

  lookup(token: Token, onlyCurrentScope: boolean = false) {
    const key = token.lexeme;

    if (Object.keys(this.symbols).includes(key)) {
      return this.symbols[key];
    } else if (this.enclosing && !onlyCurrentScope) {
      return this.enclosing.lookup(token);
    } else {
      throw new Error(`Undefined variable ${key}.`);
    }
  }
}
