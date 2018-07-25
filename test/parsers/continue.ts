import { Lexer, Parser } from "../../src";
import { Program } from "../../src/ast/program";
import { ContinueStatement } from "../../src/ast/statement";

it("continue", () => {
  const source = `continue;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(new Program([new ContinueStatement()]));
});

it("missing ;", () => {
  const source = `continue`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:9 Expect ; after continue"),
  );
});
