import { FunctionStatement } from "./statement";
import { Interpreter } from "../interpreter";
import { SymbolTable } from "../symbolTable";
import { Instance } from "./instance";
import { Token, TokenType } from "../token";

export class Functionable {
  name: string;
  declaration: FunctionStatement;
  closure: SymbolTable;

  constructor(
    name: string,
    declaration: FunctionStatement,
    closure: SymbolTable
  ) {
    this.name = name;
    this.declaration = declaration;
    this.closure = closure;
  }

  invoke(interpreter: Interpreter, args: any[]) {
    const closure = new SymbolTable(this.closure);

    this.declaration.parameters.forEach((parameter, index) =>
      closure.define(parameter, args[index])
    );

    try {
      interpreter.executeBlock(this.declaration.methods, closure);
    } catch (e) {
      return e.value;
    }
  }

  bind(instance: Instance) {
    const closure = new SymbolTable(this.closure);
    closure.define(new Token(TokenType.THIS, "this", undefined, 0), instance);
    return new Functionable(this.name, this.declaration, closure);
  }
}
