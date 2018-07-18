import { Statement } from "./statement";

export class Program {
  statements: Statement[];

  constructor(statements: Statement[]) {
    this.statements = statements;
  }
}
