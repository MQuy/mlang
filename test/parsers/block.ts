import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { BlockStatement, ExpressionStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";

it("block", () => {
  const source = `{ true; }`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new BlockStatement([
        new ExpressionStatement(
          new LiteralExpression(
            new Token(TokenType.BOOLEAN, "true", true, 1, 3),
            "Boolean",
          ),
        ),
      ]),
    ]),
  );
});

it("missing {", () => {
  const source = ` true; }`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:8 Expect expression"),
  );
});

it("missing }", () => {
  const source = `{ true; `;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:9 Expect expression"),
  );
});
