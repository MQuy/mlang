import { Classable } from "./class";
import { Token } from "../token";

export class Instance {
  klass: Classable;
  fields: { [key: string]: any };

  constructor(klass: Classable) {
    this.klass = klass;
    this.fields = {};
  }

  get(token: Token) {
    if (Object.keys(this.fields).includes(token.lexeme)) {
      return this.fields[token.lexeme];
    }

    const method = this.klass.lookupMethod(this, token);

    if (method) return method;

    throw new Error(`Undefined property ${token.lexeme} in ${this.klass.name}`);
  }

  set(token: Token, value: any) {
    this.fields[token.lexeme] = value;
  }
}
