export class BreakCall {}

export class ContinueCall {}

export class ReturnCall {
  value: any;

  constructor(value: any) {
    this.value = value;
  }
}
