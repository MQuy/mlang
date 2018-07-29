import { Functionable } from "./functionable";

export class Classable {
  name: string;
  properties: { [name: string]: any };
  methods: { [name: string]: Functionable };
  superclass?: Classable;

  constructor(name: string) {
    this.name = name;
  }
}
