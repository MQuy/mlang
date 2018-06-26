export enum TType {
  NUMBER,
  PLUS,
  MINUS,
  MUL,
  DIV,
  OPEN_PAREN,
  CLOSE_PAREN
}

export interface Token {
  type: TType;
  value: string | number;
}
