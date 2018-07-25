import { Lexer, Parser, Token, TokenType } from "../../src";
import { Program } from "../../src/ast/program";
import {
  ExpressionStatement,
  ParameterDeclaration,
} from "../../src/ast/statement";
import {
  BinaryExpression,
  VarExpression,
  LiteralExpression,
  GroupExpression,
  UnaryExpression,
  LogicalExpression,
  CallExpression,
  GetExpression,
  ArrayExpression,
  LambdaExpression,
  NewExpression,
} from "../../src/ast/expression";
import { generateExpression, generateStatement } from "../helpers";

it("multi over add", () => {
  const source = `x + def(y: String, z: Number): Boolean => 1; * 2.5;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ExpressionStatement(
          generateExpression(
            new BinaryExpression(
              generateExpression(
                new VarExpression(
                  new Token(TokenType.IDENTIFIER, "x", undefined, 1, 1),
                ),
                { line: 1, column: 1 },
                { line: 1, column: 2 },
              ),
              new Token(TokenType.PLUS, "+", undefined, 1, 3),
              generateExpression(
                new BinaryExpression(
                  generateExpression(
                    new LambdaExpression(
                      [
                        new ParameterDeclaration(
                          new Token(TokenType.IDENTIFIER, "y", undefined, 1, 9),
                          "String",
                        ),
                        new ParameterDeclaration(
                          new Token(
                            TokenType.IDENTIFIER,
                            "z",
                            undefined,
                            1,
                            20,
                          ),
                          "Number",
                        ),
                      ],
                      generateStatement(
                        new ExpressionStatement(
                          generateExpression(
                            new LiteralExpression(
                              new Token(TokenType.NUMBER, "1", 1, 1, 43),
                              "Number",
                            ),
                            { line: 1, column: 43 },
                            { line: 1, column: 44 },
                          ),
                        ),
                        { line: 1, column: 43 },
                        { line: 1, column: 45 },
                      ),
                      "Boolean",
                    ),
                    { line: 1, column: 5 },
                    { line: 1, column: 45 },
                  ),
                  new Token(TokenType.STAR, "*", undefined, 1, 46),
                  generateExpression(
                    new LiteralExpression(
                      new Token(TokenType.NUMBER, "2.5", 2.5, 1, 48),
                      "Number",
                    ),
                    { line: 1, column: 48 },
                    { line: 1, column: 51 },
                  ),
                ),
                { line: 1, column: 5 },
                { line: 1, column: 51 },
              ),
            ),
            { line: 1, column: 1 },
            { line: 1, column: 51 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 52 },
      ),
    ]),
  );
});

it("group unary over multi", () => {
  const source = `(false - new Base()) / -2.5;`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ExpressionStatement(
          generateExpression(
            new BinaryExpression(
              generateExpression(
                new GroupExpression(
                  generateExpression(
                    new BinaryExpression(
                      generateExpression(
                        new LiteralExpression(
                          new Token(TokenType.BOOLEAN, "false", false, 1, 2),
                          "Boolean",
                        ),
                        { line: 1, column: 2 },
                        { line: 1, column: 7 },
                      ),
                      new Token(TokenType.MINUS, "-", undefined, 1, 8),
                      generateExpression(
                        new NewExpression(
                          new Token(
                            TokenType.IDENTIFIER,
                            "Base",
                            undefined,
                            1,
                            14,
                          ),
                          [],
                        ),
                        { line: 1, column: 10 },
                        { line: 1, column: 20 },
                      ),
                    ),
                    { line: 1, column: 2 },
                    { line: 1, column: 20 },
                  ),
                ),
                { line: 1, column: 1 },
                { line: 1, column: 21 },
              ),
              new Token(TokenType.SLASH, "/", undefined, 1, 22),
              generateExpression(
                new UnaryExpression(
                  new Token(TokenType.MINUS, "-", undefined, 1, 24),
                  generateExpression(
                    new LiteralExpression(
                      new Token(TokenType.NUMBER, "2.5", 2.5, 1, 25),
                      "Number",
                    ),
                    { line: 1, column: 25 },
                    { line: 1, column: 28 },
                  ),
                ),
                { line: 1, column: 24 },
                { line: 1, column: 28 },
              ),
            ),
            { line: 1, column: 1 },
            { line: 1, column: 28 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 29 },
      ),
    ]),
  );
});

it("get over logical", () => {
  const source = `x.y(1) and x > [1];`;
  const tokens = new Lexer(source).scan();
  const program = new Parser(tokens).parse();

  expect(program).toEqual(
    new Program([
      generateStatement(
        new ExpressionStatement(
          generateExpression(
            new LogicalExpression(
              generateExpression(
                new CallExpression(
                  generateExpression(
                    new GetExpression(
                      generateExpression(
                        new VarExpression(
                          new Token(TokenType.IDENTIFIER, "x", undefined, 1, 1),
                        ),
                        { line: 1, column: 1 },
                        { line: 1, column: 2 },
                      ),
                      new Token(TokenType.IDENTIFIER, "y", undefined, 1, 3),
                    ),
                    { line: 1, column: 1 },
                    { line: 1, column: 4 },
                  ),
                  [
                    generateExpression(
                      new LiteralExpression(
                        new Token(TokenType.NUMBER, "1", 1, 1, 5),
                        "Number",
                      ),
                      { line: 1, column: 5 },
                      { line: 1, column: 6 },
                    ),
                  ],
                ),
                { line: 1, column: 1 },
                { line: 1, column: 7 },
              ),
              new Token(TokenType.AND, "and", undefined, 1, 8),
              generateExpression(
                new LogicalExpression(
                  generateExpression(
                    new VarExpression(
                      new Token(TokenType.IDENTIFIER, "x", undefined, 1, 12),
                    ),
                    { line: 1, column: 12 },
                    { line: 1, column: 13 },
                  ),
                  new Token(TokenType.GREAT, ">", undefined, 1, 14),
                  generateExpression(
                    new ArrayExpression([
                      generateExpression(
                        new LiteralExpression(
                          new Token(TokenType.NUMBER, "1", 1, 1, 17),
                          "Number",
                        ),
                        { line: 1, column: 17 },
                        { line: 1, column: 18 },
                      ),
                    ]),
                    { line: 1, column: 16 },
                    { line: 1, column: 19 },
                  ),
                ),
                { line: 1, column: 12 },
                { line: 1, column: 19 },
              ),
            ),
            { line: 1, column: 1 },
            { line: 1, column: 19 },
          ),
        ),
        { line: 1, column: 1 },
        { line: 1, column: 20 },
      ),
    ]),
  );
});

it("missing operator", () => {
  const source = `x 1`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:3 Expect ;"),
  );
});

it("missing ]", () => {
  const source = `[x;`;
  const tokens = new Lexer(source).scan();

  expect(() => new Parser(tokens).parse()).toThrow(
    new Error("Line 1:3 Expect expression"),
  );
});
