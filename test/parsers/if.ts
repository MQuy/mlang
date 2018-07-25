import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  IfStatement,
  ExpressionStatement,
  BlockStatement,
} from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("if without else", () => {
  const source = `if (true) false;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new IfStatement(
          generateExpression(
            new LiteralExpression(
              new Token(TokenType.BOOLEAN, "true", true, 1, 5),
              "Boolean",
            ),
            { line: 1, column: 5 },
            { line: 1, column: 9 },
          ),
          generateStatement(
            new ExpressionStatement(
              generateExpression(
                new LiteralExpression(
                  new Token(TokenType.BOOLEAN, "false", false, 1, 11),
                  "Boolean",
                ),
                { line: 1, column: 11 },
                { line: 1, column: 16 },
              ),
            ),
            { line: 1, column: 11 },
            { line: 1, column: 17 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 17 },
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
      generateStatement(
        new IfStatement(
          generateExpression(
            new LiteralExpression(
              new Token(TokenType.BOOLEAN, "true", true, 2, 6),
              "Boolean",
            ),
            { line: 2, column: 6 },
            { line: 2, column: 10 },
          ),
          generateStatement(
            new BlockStatement([]),
            { line: 2, column: 12 },
            { line: 4, column: 4 },
          ),
          generateStatement(
            new BlockStatement([]),
            { line: 4, column: 10 },
            { line: 6, column: 4 },
          ),
        ),
        { line: 2, column: 3 },
        { line: 6, column: 4 },
      ),
    ]),
  );
});

it("missing (", () => {
  const source = `if true;`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:4 Expect ( after if"),
  );
});

it("missing then", () => {
  const source = `if (true)`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:10 Expect expression"),
  );
});
