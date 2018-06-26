import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { Interpreter } from "./interpreter";
import * as readlineSync from "readline-sync";

function main() {
  while (true) {
    const source = readlineSync.question("> ");
    const tokens = new Lexer(source).execute();
    const ast = new Parser(tokens).expression();
    const result = new Interpreter(ast).execute();

    console.log(`${result}`);
  }
}

main();
