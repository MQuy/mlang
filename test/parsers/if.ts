import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  IfStatement,
  ExpressionStatement,
  BlockStatement,
} from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";

it("if without else", () => {
  const source = `if (true) false;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new IfStatement(
        new LiteralExpression(
          new Token(TokenType.BOOLEAN, "true", true, 1),
          "Boolean",
        ),
        new ExpressionStatement(
          new LiteralExpression(
            new Token(TokenType.BOOLEAN, "false", false, 1),
            "Boolean",
          ),
        ),
      ),
    ]),
  );
});

it("if", () => {
  const source = `
  if(true) {

  } else {

  }
  `;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new IfStatement(
        new LiteralExpression(
          new Token(TokenType.BOOLEAN, "true", true, 2),
          "Boolean",
        ),
        new BlockStatement([]),
        new BlockStatement([]),
      ),
    ]),
  );
});

it("missing (", () => {
  const source = `if true;`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect ( after if"),
  );
});

it("missing then", () => {
  const source = `if (true)`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect expression"),
  );
});
