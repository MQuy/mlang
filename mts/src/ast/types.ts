export class IRNode {
  pStart: IRPosition;
  pEnd: IRPosition;
}

export type IRPosition = { line: number; column: number };
