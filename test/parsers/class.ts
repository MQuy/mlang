import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  ClassStatement,
  VarStatement,
  FunctionStatement,
  BlockStatement,
} from "../../src/ast/statement";

it("class", () => {
  const source = `
  class A {
    var x;

    def hello(): A {}
  }
  `;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new ClassStatement(
        new Token(TokenType.IDENTIFIER, "A", undefined, 2),
        [new VarStatement(new Token(TokenType.IDENTIFIER, "x", undefined, 3))],
        [
          new FunctionStatement(
            new Token(TokenType.IDENTIFIER, "hello", undefined, 5),
            [],
            new BlockStatement([]),
            "A",
          ),
        ],
      ),
    ]),
  );
});

it("superclass", () => {
  const source = `class A extends B {}`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      new ClassStatement(
        new Token(TokenType.IDENTIFIER, "A", undefined, 1),
        [],
        [],
        new Token(TokenType.IDENTIFIER, "B", undefined, 1),
      ),
    ]),
  );
});

it("missing class name", () => {
  const source = "class {}";
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1: Expect class name"),
  );
});
