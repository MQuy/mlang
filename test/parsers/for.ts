import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  ForStatement,
  EmptyStatement,
  VarStatement,
  ExpressionStatement,
} from "../../src/ast/statement";
import {
  LiteralExpression,
  UnaryExpression,
  VarExpression,
} from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("for", () => {
  const source = `for(var x; true ; ++x);`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ForStatement(
          generateStatement(
            new EmptyStatement(),
            { line: 1, column: 23 },
            { line: 1, column: 24 },
          ),
          generateExpression(
            new LiteralExpression(
              new Token(TokenType.BOOLEAN, "true", true, 1, 12),
              "Boolean",
            ),
            { line: 1, column: 12 },
            { line: 1, column: 16 },
          ),
          [
            generateStatement(
              new VarStatement(
                new Token(TokenType.IDENTIFIER, "x", undefined, 1, 9),
              ),
              { line: 1, column: 5 },
              { line: 1, column: 11 },
            ),
          ],
          generateStatement(
            new ExpressionStatement(
              generateExpression(
                new UnaryExpression(
                  new Token(TokenType.PLUS_PLUS, "++", undefined, 1, 19),
                  generateExpression(
                    new VarExpression(
                      new Token(TokenType.IDENTIFIER, "x", undefined, 1, 21),
                    ),
                    { line: 1, column: 21 },
                    { line: 1, column: 22 },
                  ),
                ),
                { line: 1, column: 19 },
                { line: 1, column: 22 },
              ),
            ),
            { line: 1, column: 19 },
            { line: 1, column: 22 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 24 },
      ),
    ]),
  );
});

it("empty", () => {
  const source = `for(; ; );`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ForStatement(
          generateStatement(
            new EmptyStatement(),
            { line: 1, column: 10 },
            { line: 1, column: 11 },
          ),
          undefined,
          [],
          undefined,
        ),
        { line: 1, column: 1 },
        { line: 1, column: 11 },
      ),
    ]),
  );
});

it("missing ;", () => {
  const source = `for(x = 1 true ; ++x);`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:11 Expect ; after while intializer"),
  );
});
