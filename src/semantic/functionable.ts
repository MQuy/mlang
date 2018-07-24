export class Functionable {
  name?: string;
  parameters: any[];
  returnType: any;

  constructor(name?: string, returnType?: any) {
    this.name = name;
    this.parameters = [];
    this.returnType = returnType;
  }
}
