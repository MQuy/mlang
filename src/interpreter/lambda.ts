import { ParameterDeclaration, Statement } from "../ast/statement";
import { SymbolTable } from "./symbolTable";
import { Interpreter } from "./interpreter";
import { ReturnCall } from "../utils/error";

export class Lambda {
  parameters: ParameterDeclaration[];
  body: Statement;
  closure: SymbolTable;

  constructor(
    parameters: ParameterDeclaration[],
    body: Statement,
    closure: SymbolTable,
  ) {
    this.parameters = parameters;
    this.body = body;
    this.closure = closure;
  }

  invoke(interpreter: Interpreter, args: any[]) {
    const scope = new SymbolTable(this.closure);

    this.parameters.forEach((parameterName, index) => {
      scope.define(parameterName.name.lexeme, args[index]);
    });

    try {
      interpreter.executeFunctionBody(this.body, scope);
    } catch (e) {
      if (e instanceof ReturnCall) {
        return e.value;
      } else {
        throw e;
      }
    }
  }
}
