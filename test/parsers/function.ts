import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  FunctionStatement,
  BlockStatement,
  ParameterDeclaration,
} from "../../src/ast/statement";

it("function", () => {
  const source = `def hello(x: number, y: haha): huhu {}`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new FunctionStatement(
        new Token(TokenType.IDENTIFIER, "hello", undefined, 1),
        [
          new ParameterDeclaration(
            new Token(TokenType.IDENTIFIER, "x", undefined, 1),
            "number",
          ),
          new ParameterDeclaration(
            new Token(TokenType.IDENTIFIER, "y", undefined, 1),
            "haha",
          ),
        ],
        new BlockStatement([]),
        "huhu",
      ),
    ]),
  );
});

it("missing parameter type", () => {
  const source = `def hello(x, y {}`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect : after parameter name"),
  );
});
