import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { ForStatement, EmptyStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";

it("while", () => {
  const source = `while(true);`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new ForStatement(
        new EmptyStatement(),
        new LiteralExpression(
          new Token(TokenType.BOOLEAN, "true", true, 1),
          "Boolean",
        ),
      ),
    ]),
  );
});

it("missing condition", () => {
  const source = `while();`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect expression"),
  );
});
