import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  FunctionStatement,
  BlockStatement,
  ParameterDeclaration,
} from "../../src/ast/statement";
import { generateStatement } from "../helpers";

it("function", () => {
  const source = `def hello(x: number, y: haha): huhu {}`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new FunctionStatement(
          new Token(TokenType.IDENTIFIER, "hello", undefined, 1, 5),
          [
            new ParameterDeclaration(
              new Token(TokenType.IDENTIFIER, "x", undefined, 1, 11),
              "number",
            ),
            new ParameterDeclaration(
              new Token(TokenType.IDENTIFIER, "y", undefined, 1, 22),
              "haha",
            ),
          ],
          generateStatement(
            new BlockStatement([]),
            { line: 1, column: 37 },
            { line: 1, column: 39 },
          ),
          "huhu",
        ),
        { line: 1, column: 1 },
        { line: 1, column: 39 },
      ),
    ]),
  );
});

it("missing parameter type", () => {
  const source = `def hello(x, y {}`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:12 Expect : after parameter name"),
  );
});
