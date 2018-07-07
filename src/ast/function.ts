import { FunctionStatement } from "./statement";
import { Interpreter } from "../interpreter";
import { SymbolTable } from "../symbolTable";

export class Functionable {
  declaration: FunctionStatement;
  closure: SymbolTable;

  constructor(declaration: FunctionStatement, closure: SymbolTable) {
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
}
