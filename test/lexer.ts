import { Lexer, Token, TokenType } from "../src";

it("var statement", () => {
  const source = `var name: string = "mqlang";`;
  const tokens = new Lexer(source).scan();

  expect(tokens).toEqual([
    new Token(TokenType.VAR, "var", undefined, 1),
    new Token(TokenType.IDENTIFIER, "name", undefined, 1),
    new Token(TokenType.COLON, ":", undefined, 1),
    new Token(TokenType.IDENTIFIER, "string", undefined, 1),
    new Token(TokenType.EQUAL, "=", undefined, 1),
    new Token(TokenType.STRING, '"mqlang"', "mqlang", 1),
    new Token(TokenType.SEMICOLON, ";", undefined, 1),
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
    new Token(TokenType.IF, "if", undefined, 2),
    new Token(TokenType.LEFT_PAREN, "(", undefined, 2),
    new Token(TokenType.IDENTIFIER, "x", undefined, 2),
    new Token(TokenType.EQUAL_EQUAL, "==", undefined, 2),
    new Token(TokenType.NUMBER, "2", 2, 2),
    new Token(TokenType.RIGHT_PAREN, ")", undefined, 2),
    new Token(TokenType.LEFT_BRACE, "{", undefined, 2),
    new Token(TokenType.COMMENT, "// haha", undefined, 3),
    new Token(TokenType.RIGHT_BRACE, "}", undefined, 4),
    new Token(TokenType.EOF, ""),
  ]);
});

it("while statement", () => {
  const source = `while(true) break; 1 + 2 / 4 ** 4`;
  const tokens = new Lexer(source).scan();

  expect(tokens).toEqual([
    new Token(TokenType.WHILE, "while", undefined, 1),
    new Token(TokenType.LEFT_PAREN, "(", undefined, 1),
    new Token(TokenType.BOOLEAN, "true", true, 1),
    new Token(TokenType.RIGHT_PAREN, ")", undefined, 1),
    new Token(TokenType.BREAK, "break", undefined, 1),
    new Token(TokenType.SEMICOLON, ";", undefined, 1),
    new Token(TokenType.NUMBER, "1", 1, 1),
    new Token(TokenType.PLUS, "+", undefined, 1),
    new Token(TokenType.NUMBER, "2", 2, 1),
    new Token(TokenType.SLASH, "/", undefined, 1),
    new Token(TokenType.NUMBER, "4", 4, 1),
    new Token(TokenType.STAR_STAR, "**", undefined, 1),
    new Token(TokenType.NUMBER, "4", 4, 1),
    new Token(TokenType.EOF, ""),
  ]);
});

it("class statement", () => {
  const source = `
  class Lexer extends Base {
    var names: string[] => !name and false;

    def toString return new this;
  }
  `;
  const tokens = new Lexer(source).scan();

  expect(tokens).toEqual([
    new Token(TokenType.CLASS, "class", undefined, 2),
    new Token(TokenType.IDENTIFIER, "Lexer", undefined, 2),
    new Token(TokenType.EXTENDS, "extends", undefined, 2),
    new Token(TokenType.IDENTIFIER, "Base", undefined, 2),
    new Token(TokenType.LEFT_BRACE, "{", undefined, 2),
    new Token(TokenType.VAR, "var", undefined, 3),
    new Token(TokenType.IDENTIFIER, "names", undefined, 3),
    new Token(TokenType.COLON, ":", undefined, 3),
    new Token(TokenType.IDENTIFIER, "string", undefined, 3),
    new Token(TokenType.LEFT_BRACKET, "[", undefined, 3),
    new Token(TokenType.RIGHT_BRACKET, "]", undefined, 3),
    new Token(TokenType.ARROW, "=>", undefined, 3),
    new Token(TokenType.BANG, "!", undefined, 3),
    new Token(TokenType.IDENTIFIER, "name", undefined, 3),
    new Token(TokenType.AND, "and", undefined, 3),
    new Token(TokenType.BOOLEAN, "false", false, 3),
    new Token(TokenType.SEMICOLON, ";", undefined, 3),
    new Token(TokenType.DEF, "def", undefined, 5),
    new Token(TokenType.IDENTIFIER, "toString", undefined, 5),
    new Token(TokenType.RETURN, "return", undefined, 5),
    new Token(TokenType.NEW, "new", undefined, 5),
    new Token(TokenType.THIS, "this", undefined, 5),
    new Token(TokenType.SEMICOLON, ";", undefined, 5),
    new Token(TokenType.RIGHT_BRACE, "}", undefined, 6),
    new Token(TokenType.EOF, ""),
  ]);
});
