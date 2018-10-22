import { Classable } from "./classable";
import { Functionable } from "./functionable";
import { SymbolTable } from "./symbolTable";

export class Instance {
  fields: { [name: string]: any };
  methods: { [name: string]: Functionable };
  klass: Classable;
  superInstance?: Instance;

  constructor(klass: Classable) {
    if (klass.superclass) {
      this.superInstance = new Instance(klass.superclass);
    }

    this.klass = klass;
    this.fields = Object.keys(klass.properties).reduce((acc, iter) => {
      acc[iter] = klass.properties[iter];
      return acc;
    }, {});
    this.methods = Object.keys(klass.methods).reduce((acc, iter) => {
      const klassMethod = klass.methods[iter];
      const scope = new SymbolTable(klassMethod.closure);
      const method = new Functionable(
        klassMethod.parameters,
        klassMethod.body,
        scope,
        klassMethod.name,
      );

      scope.define("this", this);
      if (this.superInstance) {
        scope.define("super", this.superInstance);
      }
      acc[method.name] = method;
      return acc;
    }, {});
  }

  get(name: string) {
    if (Object.keys(this.fields).includes(name)) {
      return this.fields[name];
    } else if (Object.keys(this.methods).includes(name)) {
      return this.methods[name];
    } else if (this.superInstance) {
      return this.superInstance.get(name);
    }
    throw new Error(`${this.klass.name} doesn't have ${name}`);
  }

  set(name: string, value: any) {
    if (Object.keys(this.fields).includes(name)) {
      this.fields[name] = value;
    } else if (this.superInstance) {
      this.superInstance.set(name, value);
    } else {
      throw new Error(`${this.klass.name} doesn't have property named ${name}`);
    }
  }
}
