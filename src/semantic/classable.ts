import { Functionable } from "./functionable";

export class Classable {
  name: string;
  properties: { [name: string]: any };
  methods: { [name: string]: Functionable };
  superclass?: Classable;

  constructor(name: string) {
    this.name = name;
    this.properties = {};
    this.methods = {};
  }

  get(name: string) {
    if (Object.keys(this.properties).includes(name)) {
      return this.properties[name];
    } else if (Object.keys(this.methods).includes(name)) {
      return this.methods[name];
    } else {
      throw new Error(`${name} is not defined`);
    }
  }
}
