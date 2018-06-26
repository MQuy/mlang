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

    while (char) {
      if (char === undefined) {
        break;
      } else if (/[\d\.]/.test(char)) {
        tokens.push({ value: this.extractNumber(), type: TType.NUMBER });
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
          tokens.push({ value: char, type: TType.OPEN_PAREN });
        } else if (char === ")") {
          tokens.push({ value: char, type: TType.CLOSE_PAREN });
        } else if (!/\s/.test(char)) {
          throw Error("Wrong syntax");
        }
        char = this.source[++this.cPointer];
      }
    }
    return tokens;
  }

  extractNumber() {
    let char = this.source[this.cPointer];
    let number = "";

    while (/[\d\.]/.test(char)) {
      number += char;
      char = this.source[++this.cPointer];
    }

    return parseFloat(number);
  }
}
