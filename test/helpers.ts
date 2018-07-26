import { Statement } from "../src/ast/statement";
import { Token } from "../src";
import { IRPosition } from "../src/ast/types";
import { Expression } from "../src/ast/expression";

export function generateStatement<T extends Statement>(
  statement: T,
  start: Token | IRPosition,
  end: Token | IRPosition,
): T {
  statement.pStart = { line: start.line, column: start.column };
  statement.pEnd = { line: end.line, column: end.column };
  return statement;
}

export function generateExpression<T extends Expression>(
  statement: T,
  start: Token | IRPosition,
  end: Token | IRPosition,
): T {
  statement.pStart = { line: start.line, column: start.column };
  statement.pEnd = { line: end.line, column: end.column };
  return statement;
}

export function error(x: number, y: number, type1: string, type2: string) {
  return new Error(
    `Line ${x}:${y} Type '${type1}' is not assignable to type '${type2}'`,
  );
}
