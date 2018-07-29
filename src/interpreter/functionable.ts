import { FunctionStatement } from "../ast/statement";
import { SymbolTable } from "./symbolTable";

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
}
