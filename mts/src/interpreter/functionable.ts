import { ParameterDeclaration, Statement } from "../ast/statement";
import { SymbolTable } from "./symbolTable";
import { Lambda } from "./lambda";

export class Functionable extends Lambda {
  name: string;

  constructor(
    parameters: ParameterDeclaration[],
    body: Statement,
    closure: SymbolTable,
    name: string,
  ) {
    super(parameters, body, closure);

    this.name = name;
  }
}
