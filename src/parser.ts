import { TType, Token } from "./token";

interface AST {
  left?: AST | Token;
  operator: Token;
  right: AST | Token;
}

/**
 * expression = factor ((PLUS|MINUS) factor)*
 * factor = term ((MUL|DIV) term) *
 * term = NUMBER | OPEN_PAREN expression CLOSE_PAREN | (PLUS|MINUS) expression
 */
export class Parser {
  tokens: Token[];
  cPointer: number;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.cPointer = 0;
  }
  expression(): AST | Token {
    let node = this.factor();
    let token = this.currentToken;

    while (token && (token.type === TType.PLUS || token.type === TType.MINUS)) {
      this.cPointer += 1;
      node = { left: node, operator: token, right: this.factor() };
      token = this.currentToken;
    }

    return node;
  }
  factor(): AST | Token {
    let node = this.term();
    let token = this.currentToken;

    while (token && (token.type === TType.MUL || token.type === TType.DIV)) {
      this.cPointer += 1;
      node = { left: node, operator: token, right: this.factor() };
      token = this.currentToken;
    }

    return node;
  }
  term(): AST | Token {
    let token = this.currentToken;

    if (token.type === TType.OPEN_PAREN) {
      this.cPointer += 1;
      const node = this.expression();
      this.cPointer += 1;
      return node;
    } else if (token.type === TType.PLUS || token.type === TType.MINUS) {
      this.cPointer += 1;
      return { operator: token, right: this.expression() };
    } else {
      this.cPointer += 1;
      return token;
    }
  }

  get currentToken() {
    return this.tokens[this.cPointer];
  }
  get nextToken() {
    return this.tokens[this.cPointer + 1];
  }
}
