import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  ForStatement,
  EmptyStatement,
  VarStatement,
  ExpressionStatement,
} from "../../src/ast/statement";
import { LiteralExpression, UnaryExpression } from "../../src/ast/expression";

it("for", () => {
  const source = `for(var x; true ; ++x);`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new ForStatement(
        new EmptyStatement(),
        new LiteralExpression(
          new Token(TokenType.BOOLEAN, "true", true, 1, 12),
          "Boolean",
        ),
        [new VarStatement(new Token(TokenType.IDENTIFIER, "x", undefined, 1, 9))],
        new ExpressionStatement(
          new UnaryExpression(
            new Token(TokenType.PLUS_PLUS, "++", undefined, 1, 19),
            new LiteralExpression(
              new Token(TokenType.IDENTIFIER, "x", undefined, 1, 21),
            ),
          ),
        ),
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
      new ForStatement(new EmptyStatement(), undefined, [], undefined),
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
