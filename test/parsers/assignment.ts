import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { ExpressionStatement } from "../../src/ast/statement";
import {
  AssignmentExpression,
  VarExpression,
  LiteralExpression,
} from "../../src/ast/expression";

it("assignment", () => {
  const source = `x = null;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new ExpressionStatement(
        new AssignmentExpression(
          new VarExpression(new Token(TokenType.IDENTIFIER, "x", undefined, 1)),
          new LiteralExpression(new Token(TokenType.NULL, "null", null, 1)),
        ),
      ),
    ]),
  );
});

it("missing initializer", () => {
  const source = `x = ;`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect expression"),
  );
});
