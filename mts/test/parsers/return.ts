import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { ReturnStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("return expression", () => {
  const source = `return 1;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ReturnStatement(
          generateExpression(
            new LiteralExpression(
              new Token(TokenType.NUMBER, "1", 1, 1, 8),
              "Number",
            ),
            { line: 1, column: 8 },
            { line: 1, column: 9 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 10 },
      ),
    ]),
  );
});

it("return", () => {
  const source = `return;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ReturnStatement(),
        { line: 1, column: 1 },
        { line: 1, column: 8 },
      ),
    ]),
  );
});

it("missing ;", () => {
  const source = `return`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:7 Expect expression"),
  );
});
