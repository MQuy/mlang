import { Lexer, Parser } from "../../src";
import { Program } from "../../src/ast/program";
import { ContinueStatement } from "../../src/ast/statement";
import { generateStatement } from "../helpers";

it("continue", () => {
  const source = `continue;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ContinueStatement(),
        { line: 1, column: 1 },
        { line: 1, column: 10 },
      ),
    ]),
  );
});

it("missing ;", () => {
  const source = `continue`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:9 Expect ; after continue"),
  );
});
