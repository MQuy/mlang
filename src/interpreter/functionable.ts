import { FunctionStatement } from "../ast/statement";
import { SymbolTable } from "./symbolTable";
import { Interpreter } from "./interpreter";
import { ReturnCall } from "../utils/error";

export class Functionable {
  name: string;
  declaration: FunctionStatement;
  closure: SymbolTable;

  constructor(
    name: string,
    declaration: FunctionStatement,
    closure: SymbolTable,
  ) {
    this.name = name;
    this.declaration = declaration;
    this.closure = closure;
  }

  invoke(interpreter: Interpreter, args: any[]) {
    const scope = new SymbolTable(this.closure);

    this.declaration.parameters.forEach((parameterName, index) => {
      scope.define(parameterName.name.lexeme, args[index]);
    });

    try {
      interpreter.executeInScope(this.declaration.body, scope);
    } catch (e) {
      if (e instanceof ReturnCall) {
        return e.value;
      } else {
        throw e;
      }
    }
  }
}
