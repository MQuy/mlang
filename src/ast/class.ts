import { Interpreter } from "../interpreter";
import { Instance } from "./instance";
import { Functionable } from "./function";
import { Token } from "../token";

export class Classable {
  name: string;
  methods: { [key: string]: Functionable };

  constructor(name: string, methods: { [key: string]: Functionable }) {
    this.name = name;
    this.methods = methods;
  }

  invoke(interpreter: Interpreter, args: any[]) {
    return new Instance(this);
  }

  lookupMethod(instance: Instance, token: Token) {
    if (this.methods[token.lexeme]) {
      return this.methods[token.lexeme].bind(instance);
    }
  }
}
