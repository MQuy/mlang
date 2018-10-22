import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { ForStatement, EmptyStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("while", () => {
  const source = `while(true);`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ForStatement(
          generateStatement(
            new EmptyStatement(),
            {
              line: 1,
              column: 12,
            },
            {
              line: 1,
              column: 13,
            },
          ),
          generateExpression(
            new LiteralExpression(
              new Token(TokenType.BOOLEAN, "true", true, 1, 7),
              "Boolean",
            ),
            { line: 1, column: 7 },
            { line: 1, column: 11 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 13 },
      ),
    ]),
  );
});

it("missing condition", () => {
  const source = `while();`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:7 Expect expression"),
  );
});
