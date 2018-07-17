import { Lexer, Token, TokenType } from "../src";

it("var statement", () => {
  const source = `var name: string = "mqlang";`;
  const tokens = new Lexer(source).scan();

  expect(tokens).toEqual([
    new Token(TokenType.VAR, "var", undefined, 0),
    new Token(TokenType.IDENTIFIER, "name", undefined, 0),
    new Token(TokenType.COLON, ":", undefined, 0),
    new Token(TokenType.IDENTIFIER, "string", undefined, 0),
    new Token(TokenType.EQUAL, "=", undefined, 0),
    new Token(TokenType.STRING, '"mqlang"', "mqlang", 0),
    new Token(TokenType.SEMICOLON, ";", undefined, 0),
    new Token(TokenType.EOF, ""),
  ]);
});

it("if statement", () => {
  const source = `
  if (x == 2) {
    // haha
  }`;
  const tokens = new Lexer(source).scan();

  expect(tokens).toEqual([
    new Token(TokenType.IF, "if", undefined, 1),
    new Token(TokenType.LEFT_PAREN, "(", undefined, 1),
    new Token(TokenType.IDENTIFIER, "x", undefined, 1),
    new Token(TokenType.EQUAL_EQUAL, "==", undefined, 1),
    new Token(TokenType.NUMBER, "2", 2, 1),
    new Token(TokenType.RIGHT_PAREN, ")", undefined, 1),
    new Token(TokenType.LEFT_BRACE, "{", undefined, 1),
    new Token(TokenType.COMMENT, "// haha", undefined, 2),
    new Token(TokenType.RIGHT_BRACE, "}", undefined, 3),
    new Token(TokenType.EOF, ""),
  ]);
});

it("while statement", () => {
  const source = `while(true) 1 + 2 / 4 ** 4`;
  const tokens = new Lexer(source).scan();

  expect(tokens).toEqual([
    new Token(TokenType.WHILE, "while", undefined, 0),
    new Token(TokenType.LEFT_PAREN, "(", undefined, 0),
    new Token(TokenType.BOOLEAN, "true", true, 0),
    new Token(TokenType.RIGHT_PAREN, ")", undefined, 0),
    new Token(TokenType.NUMBER, "1", 1, 0),
    new Token(TokenType.PLUS, "+", undefined, 0),
    new Token(TokenType.NUMBER, "2", 2, 0),
    new Token(TokenType.SLASH, "/", undefined, 0),
    new Token(TokenType.NUMBER, "4", 4, 0),
    new Token(TokenType.STAR_STAR, "**", undefined, 0),
    new Token(TokenType.NUMBER, "4", 4, 0),
    new Token(TokenType.EOF, ""),
  ]);
});
