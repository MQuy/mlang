import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  ClassStatement,
  VarStatement,
  FunctionStatement,
  BlockStatement,
} from "../../src/ast/statement";
import { generateStatement } from "../helpers";

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
      generateStatement(
        new ClassStatement(
          new Token(TokenType.IDENTIFIER, "A", undefined, 2, 9),
          [
            generateStatement(
              new VarStatement(
                new Token(TokenType.IDENTIFIER, "x", undefined, 3, 9),
              ),
              { line: 3, column: 5 },
              { line: 3, column: 11 },
            ),
          ],
          [
            generateStatement(
              new FunctionStatement(
                new Token(TokenType.IDENTIFIER, "hello", undefined, 5, 9),
                [],
                generateStatement(
                  new BlockStatement([]),
                  { line: 5, column: 20 },
                  { line: 5, column: 22 },
                ),
                "A",
              ),
              { line: 5, column: 5 },
              { line: 5, column: 22 },
            ),
          ],
        ),
        { line: 2, column: 3 },
        { line: 6, column: 4 },
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
      generateStatement(
        new ClassStatement(
          new Token(TokenType.IDENTIFIER, "A", undefined, 1, 7),
          [],
          [],
          new Token(TokenType.IDENTIFIER, "B", undefined, 1, 17),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 21 },
      ),
    ]),
  );
});

it("missing class name", () => {
  const source = "class {}";
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:7 Expect class name"),
  );
});
