import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { BlockStatement, ExpressionStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("block", () => {
  const source = `{ true; }`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new BlockStatement([
          generateStatement(
            new ExpressionStatement(
              generateExpression(
                new LiteralExpression(
                  new Token(TokenType.BOOLEAN, "true", true, 1, 3),
                  "Boolean",
                ),
                { line: 1, column: 3 },
                { line: 1, column: 7 },
              ),
            ),
            { line: 1, column: 3 },
            { line: 1, column: 8 },
          ),
        ]),
        { line: 1, column: 1 },
        { line: 1, column: 10 },
      ),
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
