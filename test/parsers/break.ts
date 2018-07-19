import { Lexer, Parser } from "../../src";
import { Program } from "../../src/ast/program";
import { BreakStatement } from "../../src/ast/statement";

it("break", () => {
  const source = `break;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(new Program([new BreakStatement()]));
});

it("missing ;", () => {
  const source = `break`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect ; after break"),
  );
});
