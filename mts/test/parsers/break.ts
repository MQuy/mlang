import { Lexer, Parser } from "../../src";
import { Program } from "../../src/ast/program";
import { BreakStatement } from "../../src/ast/statement";
import { generateStatement } from "../helpers";

it("break", () => {
  const source = `break;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new BreakStatement(),
        { line: 1, column: 1 },
        { line: 1, column: 7 },
      ),
    ]),
  );
});

it("missing ;", () => {
  const source = `break`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:6 Expect ; after break"),
  );
});
