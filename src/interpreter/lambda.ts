import { SymbolTable } from "./symbolTable";
import { LambdaExpression } from "../ast/expression";

export class Lambda {
  declaration: LambdaExpression;
  closure: SymbolTable;

  constructor(declaration: LambdaExpression, closure: SymbolTable) {
    this.declaration = declaration;
    this.closure = closure;
  }
}
