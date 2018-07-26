import { IRPosition } from "../ast/types";
import { Token } from "../token";
import { error } from "../utils/print";

export enum BuiltinTypes {
  String = "String",
  Number = "Number",
  Null = "Null",
  Boolean = "Boolean",
}

export type Types = Classable | Functionable | BuiltinTypes;

export class Functionable {
  name?: string;
  parameters: Types[];
  returnType: Types;

  constructor(returnType: Types, name?: string) {
    this.name = name;
    this.parameters = [];
    this.returnType = returnType;
  }
}

export class Classable {
  name: string;
  properties: { [name: string]: Types };
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

export function checkInheritance(
  type1: Types,
  type2: Types,
  position: IRPosition | Token,
) {
  if (type1 === type2) {
    return true;
  } else if (type1 instanceof Classable && type2 instanceof Classable) {
    let superclass = type1.superclass;

    while (superclass) {
      if (superclass === type2) {
        return true;
      }
      superclass = superclass.superclass;
    }
  }
  error(
    position,
    `Type '${getTypeName(type1)}' is not assignable to type '${getTypeName(
      type2,
    )}'`,
  );
}

function getTypeName(type: Types) {
  if (type instanceof Classable || type instanceof Functionable) {
    return type.name;
  } else {
    return type;
  }
}
