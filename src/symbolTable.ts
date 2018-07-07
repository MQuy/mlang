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

  assign(token: Token, value: any) {
    const key = token.lexeme;

    if (Object.keys(this.symbols).includes(key)) {
      this.symbols[key] = value;
    } else if (this.enclosing) {
      this.enclosing.assign(token, value);
    } else {
      throw new Error(`Undefined variable ${key}.`);
    }
  }

  lookup(token: Token) {
    const key = token.lexeme;

    if (Object.keys(this.symbols).includes(key)) {
      return this.symbols[key];
    } else if (this.enclosing) {
      return this.enclosing.lookup(token);
    } else {
      throw new Error(`Undefined variable ${key}.`);
    }
  }
}
