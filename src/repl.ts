import * as readlineSync from "readline-sync";
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { Interpreter } from "./interpreter/interpreter";
import { SymbolTable } from "./interpreter/symbolTable";
import { TypeChecking } from "./semantic/typeChecking";
import { SymbolTable as SymbolTableType } from "./semantic/symbolTable";
import { BuiltinTypes } from "./semantic/types";

export class Repl {
  scope: SymbolTable;
  scopeTypes: SymbolTableType;

  constructor() {
    this.scope = new SymbolTable();
    this.scopeTypes = new SymbolTableType(undefined, BuiltinTypes);
  }

  execute() {
    this.welcome();

    while (true) {
      const tokens = new Lexer(this.getInput()).scan();
      const ast = new Parser(tokens).parse();

      new TypeChecking(ast, this.scopeTypes).run();

      const interpreter = new Interpreter(ast, this.scope);
      const result = ast.statements.reduce(
        (_, statement) => interpreter.execute(statement),
        undefined,
      );
      console.log(result);
    }
  }

  getInput() {
    return readlineSync.question("> ");
  }

  welcome() {
    console.log("mqlang, version 0.1.0");
  }
}

new Repl().execute();
