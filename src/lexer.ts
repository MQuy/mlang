import { TType, Token } from "./token";

export class Lexer {
  source: string;
  cPointer: number;

  constructor(source: string) {
    this.source = source;
    this.cPointer = 0;
  }

  execute() {
    const tokens: Token[] = [];
    let char = this.source[this.cPointer];
    let token, cPointer;

    while (char) {
      if (char === undefined) {
        break;
      } else if (/[\d]/.test(char)) {
        let { token } = this.getNextToken(/[\d\.]/, true);
        tokens.push({
          value: token,
          type: /\d+/.test(token) ? TType.INTEGER_CONST : TType.REAL_CONST
        });
        char = this.source[this.cPointer];
      } else if (
        char === "P" &&
        ({ token, cPointer } = this.getNextToken(/[PROGRAM]/)) &&
        token === TType.PROGRAM
      ) {
        tokens.push({ value: token, type: TType.PROGRAM });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        char === "I" &&
        ({ token, cPointer } = this.getNextToken(/[INTEGER]/)) &&
        token === TType.INTEGER
      ) {
        tokens.push({ value: token, type: TType.INTEGER });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        char === "R" &&
        ({ token, cPointer } = this.getNextToken(/[REAL]/)) &&
        token === TType.REAL
      ) {
        tokens.push({ value: token, type: TType.REAL });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        char === "V" &&
        ({ token, cPointer } = this.getNextToken(/[VAR]/)) &&
        token === TType.VAR
      ) {
        tokens.push({ value: token, type: TType.VAR });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        char === "B" &&
        ({ token, cPointer } = this.getNextToken(/[BEGIN]/)) &&
        token === TType.BEGIN
      ) {
        tokens.push({ value: token, type: TType.BEGIN });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        char === "E" &&
        ({ token, cPointer } = this.getNextToken(/[END]/)) &&
        token === TType.END
      ) {
        tokens.push({ value: token, type: TType.END });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        char === ":" &&
        ({ token, cPointer } = this.getNextToken(/[:=]/)) &&
        token === ":="
      ) {
        tokens.push({ value: token, type: TType.ASSIGN });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else if (
        /[a-z]/.test(char) &&
        ({ token, cPointer } = this.getNextToken(/[a-z]/))
      ) {
        tokens.push({ value: token, type: TType.VARIABLE_NAME });
        this.cPointer = cPointer;
        char = this.source[this.cPointer];
      } else {
        if (char === "+") {
          tokens.push({ value: char, type: TType.PLUS });
        } else if (char === "-") {
          tokens.push({ value: char, type: TType.MINUS });
        } else if (char === "*") {
          tokens.push({ value: char, type: TType.MUL });
        } else if (char === "/") {
          tokens.push({ value: char, type: TType.DIV });
        } else if (char === "(") {
          tokens.push({ value: char, type: TType.LPAREN });
        } else if (char === ")") {
          tokens.push({ value: char, type: TType.RPAREN });
        } else if (char === ".") {
          tokens.push({ value: char, type: TType.DOT });
        } else if (char === ";") {
          tokens.push({ value: char, type: TType.SEMI });
        } else if (char === ":") {
          tokens.push({ value: char, type: TType.COLON });
        } else if (char === ",") {
          tokens.push({ value: char, type: TType.COMMA });
        } else if (!/\s/.test(char)) {
          throw Error(`Wrong syntax: ${char}`);
        }
        char = this.source[++this.cPointer];
      }
    }
    return tokens;
  }

  getNextToken(pattern: RegExp, move: boolean = false) {
    let runner = this.cPointer;
    let char = this.source[runner];
    let token = "";

    while (pattern.test(char)) {
      token += char;
      char = this.source[++runner];
    }

    if (move) {
      this.cPointer = runner;
    }

    return { token, cPointer: runner };
  }
}
