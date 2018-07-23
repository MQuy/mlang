import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import { VarsStatement, VarStatement } from "../../src/ast/statement";
import { LiteralExpression } from "../../src/ast/expression";

it("one var declaration", () => {
  const source = `var x = 1;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new VarsStatement([
        new VarStatement(
          new Token(TokenType.IDENTIFIER, "x", undefined, 1),
          new LiteralExpression(
            new Token(TokenType.NUMBER, "1", 1, 1),
            "Number",
          ),
        ),
      ]),
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
      new VarsStatement([
        new VarStatement(new Token(TokenType.IDENTIFIER, "x", undefined, 2)),
        new VarStatement(
          new Token(TokenType.IDENTIFIER, "y", undefined, 2),
          new LiteralExpression(
            new Token(TokenType.STRING, '"minh quy"', "minh quy", 2),
            "String",
          ),
        ),
      ]),
      new VarsStatement([
        new VarStatement(
          new Token(TokenType.IDENTIFIER, "z", undefined, 3),
          new LiteralExpression(
            new Token(TokenType.BOOLEAN, "true", true, 3),
            "Boolean",
          ),
        ),
      ]),
    ]),
  );
});

it("missing ;", () => {
  const source = "var x";
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect ; after declaration"),
  );
});

it("missing identifier", () => {
  const source = "var ;";
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect identifier after var"),
  );
});
