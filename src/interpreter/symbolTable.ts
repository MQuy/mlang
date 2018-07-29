const MAX_STACK = 500;

export class SymbolTable {
  enclosing?: SymbolTable;
  symbols: { [name: string]: any };

  constructor(enclosing?: SymbolTable) {
    this.enclosing = enclosing;
    this.symbols = {};
  }

  define(name: string, value: any) {
    this.symbols[name] = value;
  }

  lookup(name?: string, depth = MAX_STACK) {
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

  assign(name: string, value: any) {
    let scope: SymbolTable | undefined = this;

    for (let i = 0; i <= MAX_STACK && scope; ++i) {
      if (Object.keys(scope.symbols).includes(name)) {
        break;
      } else {
        scope = scope.enclosing;
      }
    }
    if (scope) {
      scope.symbols[name] = value;
    } else {
      throw new Error(`Cannot find ${name}`);
    }
  }
}
