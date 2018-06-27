import { TType, Token } from "./token";
import { TreeNode } from "./ast";

/**
 * program : PROGRAM variable SEMI block dot
 * block : declarations compound_statement
 * declarations : VAR (variable_declaration SEMI)+ | empty
 * variable_declaration : variable (COMMA ID)* COLON typespec
 * typespec : INTEGER | REAL
 * compound_statement : BEGIN statement_list END
 * statement_list : statement (SEMI statement)*
 * statement : compound_statement | assignment_statement | empty
 * assignment_statement : variable ASSIGN expr
 * empty :
 * expr : term ((PLUS | MINUS) term)*
 * term : factor ((MUL | DIV) factor)*
 * factor : (PLUS | MINUS) expr | INTEGER_CONST | REAL_CONST | LPAREN expr RPAREN | variable
 * variable : ID
 */

export class Parser {
  tokens: Token[];
  cPointer: number;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.cPointer = 0;
  }

  execute() {
    return this.expression();
  }

  expression(): TreeNode {
    let node = this.factor();
    let token = this.currentToken;

    while (token && (token.type === TType.PLUS || token.type === TType.MINUS)) {
      this.cPointer += 1;
      node = new TreeNode({
        left: node,
        operator: token,
        right: this.factor()
      });
      token = this.currentToken;
    }

    return node;
  }

  factor(): TreeNode {
    let node = this.term();
    let token = this.currentToken;

    while (token && (token.type === TType.MUL || token.type === TType.DIV)) {
      this.cPointer += 1;
      node = new TreeNode({
        left: node,
        operator: token,
        right: this.factor()
      });
      token = this.currentToken;
    }

    return node;
  }

  term(): TreeNode {
    let token = this.currentToken;

    if (token.type === TType.OPEN_PAREN) {
      this.cPointer += 1;
      const node = this.expression();
      this.cPointer += 1;
      return node;
    } else if (token.type === TType.PLUS || token.type === TType.MINUS) {
      this.cPointer += 1;
      return new TreeNode({ operator: token, right: this.expression() });
    } else {
      this.cPointer += 1;
      return new TreeNode({
        operator: { type: TType.PLUS, value: "+" },
        right: token
      });
    }
  }

  get currentToken() {
    return this.tokens[this.cPointer];
  }

  get nextToken() {
    return this.tokens[this.cPointer + 1];
  }
}
