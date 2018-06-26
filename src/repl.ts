import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { Interpreter } from "./interpreter";
import * as readline from "readline";

function main() {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  while (true) {
    rl.question("> ", source => {
      const tokens = new Lexer(source).execute();
      const ast = new Parser(tokens).expression();
      const result = new Interpreter(ast).execute();

      console.log(`${result}\n`);
    });
  }
}

main();
