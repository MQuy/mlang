export abstract class AstNode {}

export class ReturnError {
  value: any;

  constructor(value: any) {
    this.value = value;
  }
}
