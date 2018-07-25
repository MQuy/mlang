import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { VarsStatement, VarStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("one var declaration", () => {
  const source = `var x = 1;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new VarsStatement([
          generateStatement(
            new VarStatement(
              new Token(TokenType.IDENTIFIER, "x", undefined, 1, 5),
              generateExpression(
                new LiteralExpression(
                  new Token(TokenType.NUMBER, "1", 1, 1, 9),
                  "Number",
                ),
                { line: 1, column: 9 },
                { line: 1, column: 10 },
              ),
            ),
            { line: 1, column: 1 },
            { line: 1, column: 11 },
          ),
        ]),
        { line: 1, column: 1 },
        { line: 1, column: 11 },
      ),
    ]),
  );
});

it("three var declarations", () => {
  const source = `
    var x, y = "minh quy";
    var z = true;
  `;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new VarsStatement([
          generateStatement(
            new VarStatement(
              new Token(TokenType.IDENTIFIER, "x", undefined, 2, 9),
            ),
            { line: 2, column: 5 },
            { line: 2, column: 10 },
          ),
          generateStatement(
            new VarStatement(
              new Token(TokenType.IDENTIFIER, "y", undefined, 2, 12),
              generateExpression(
                new LiteralExpression(
                  new Token(TokenType.STRING, '"minh quy"', "minh quy", 2, 16),
                  "String",
                ),
                { line: 2, column: 16 },
                { line: 2, column: 26 },
              ),
            ),
            { line: 2, column: 12 },
            { line: 2, column: 27 },
          ),
        ]),
        { line: 2, column: 5 },
        { line: 2, column: 27 },
      ),
      generateStatement(
        new VarsStatement([
          generateStatement(
            new VarStatement(
              new Token(TokenType.IDENTIFIER, "z", undefined, 3, 9),
              generateExpression(
                new LiteralExpression(
                  new Token(TokenType.BOOLEAN, "true", true, 3, 13),
                  "Boolean",
                ),
                { line: 3, column: 13 },
                { line: 3, column: 17 },
              ),
            ),
            { line: 3, column: 5 },
            { line: 3, column: 18 },
          ),
        ]),
        { line: 3, column: 5 },
        { line: 3, column: 18 },
      ),
    ]),
  );
});

it("missing ;", () => {
  const source = "var x";
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:6 Expect ; after declaration"),
  );
});

it("missing identifier", () => {
  const source = "var ;";
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:5 Expect identifier after var"),
  );
});
