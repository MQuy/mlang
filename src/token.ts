export enum TType {
  INTEGER = "INTEGER",
  REAL = "REAL",
  INTEGER_CONST = "INTEGER_CONST",
  REAL_CONST = "REAL_CONST",
  PLUS = "PLUS",
  MINUS = "MINUS",
  MUL = "MUL",
  DIV = "DIV",
  LPAREN = "LPAREN",
  RPAREN = "RPAREN",
  VARIABLE_NAME = "VARIABLE_NAME",
  ASSIGN = "ASSIGN",
  BEGIN = "BEGIN",
  END = "END",
  SEMI = "SEMI",
  DOT = "DOT",
  PROGRAM = "PROGRAM",
  VAR = "VAR",
  COLON = "COLON",
  COMMA = "COMMA",
  PROCEDURE = "PROCEDURE"
}

export interface Token {
  type: TType;
  value: string;
}
