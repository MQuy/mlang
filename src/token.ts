export enum TokenType {
  LEFT_PAREN = "LEFT_PAREN",
  RIGHT_PAREN = "RIGHT_PAREN",
  LEFT_BRACE = "LEFT_BRACE",
  RIGHT_BRACE = "RIGHT_BRACE",
  LEFT_BRACKET = "LEFT_BRACKET",
  RIGHT_BRACKET = "RIGHT_BRACKET",

  DEF = "DEF",
  CLASS = "CLASS",
  EXTENDS = "EXTENDS",
  VAR = "VAR",
  NEW = "NEW",
  ARROW = "ARROW",

  IF = "IF",
  ELSE = "ELSE",
  FOR = "FOR",
  WHILE = "WHILE",

  BREAK = "BREAK",
  CONTINUE = "CONTINUE",
  RETURN = "RETURN",

  COMMA = "COMMA",
  SEMICOLON = "SEMICOLON",
  COLON = "COLON",
  DOT = "DOT",

  EQUAL = "EQUAL",
  BANG_EQUAL = "BANG_EQUAL",
  EQUAL_EQUAL = "EQUAL_EQUAL",
  GREAT = "GREAT",
  GREAT_THAN = "GREAT_THAN",
  LESS = "LESS",
  LESS_THAN = "LESS_THAN",

  AND = "AND",
  OR = "OR",

  PLUS = "PLUS",
  MINUS = "MINUS",
  STAR = "STAR",
  STAR_STAR = "STAR_STAR",
  SLASH = "SLASH",
  PLUS_PLUS = "PLUS_PLUS",
  MINUS_MINUS = "MINUS_MINUS",
  BANG = "BANG",

  NUMBER = "NUMBER",
  STRING = "STRING",
  BOOLEAN = "BOOLEAN",
  NULL = "NULL",

  THIS = "THIS",
  SUPER = "SUPER",
  IDENTIFIER = "IDENTIFIER",
  COMMENT = "COMMENT",

  EOF = "EOF",
}

export type Literal = number | string | boolean | null;

export class Token {
  type: TokenType;
  lexeme: string;
  literal: Literal | undefined;
  line: number;
  column: number;

  constructor(
    type: TokenType,
    lexeme: string,
    literal: Literal | undefined,
    line: number,
    column: number,
  ) {
    this.type = type;
    this.lexeme = lexeme;
    this.literal = literal;
    this.line = line;
    this.column = column;
  }

  toString() {
    return `Line ${this.line}:${this.column}`;
  }
}
