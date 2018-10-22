import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { ExpressionStatement } from "../../src/ast/statement";
import {
  AssignmentExpression,
  VarExpression,
  LiteralExpression,
} from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("assignment", () => {
  const source = `x = null;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ExpressionStatement(
          generateExpression(
            new AssignmentExpression(
              new Token(TokenType.IDENTIFIER, "x", undefined, 1, 1),
              generateExpression(
                new LiteralExpression(
                  new Token(TokenType.NULL, "null", null, 1, 5),
                ),
                { line: 1, column: 5 },
                { line: 1, column: 9 },
              ),
            ),
            { line: 1, column: 1 },
            { line: 1, column: 9 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 10 },
      ),
    ]),
  );
});

it("missing initializer", () => {
  const source = `x = ;`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:5 Expect expression"),
  );
});
