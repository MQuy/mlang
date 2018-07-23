import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { ReturnStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";

it("return expression", () => {
  const source = `return 1;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new ReturnStatement(
        new LiteralExpression(new Token(TokenType.NUMBER, "1", 1, 1), "Number"),
      ),
    ]),
  );
});

it("return", () => {
  const source = `return;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(new Program([new ReturnStatement()]));
});

it("missing ;", () => {
  const source = `return`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect expression"),
  );
});
