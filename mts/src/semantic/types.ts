import { IRPosition } from "../ast/types";
import { Token } from "../token";
import { error } from "../utils/print";

export enum BuiltinTypes {
  String = "String",
  Number = "Number",
  Null = "Null",
  Boolean = "Boolean",
  void = "void",
}

export type Types = Classable | Functionable | BuiltinTypes | BuiltinTypes[];

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

export function checkTypeAssignable(
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
  } else if (
    Array.isArray(type1) &&
    Array.isArray(type2) &&
    type1.length === type2.length
  ) {
    for (let i = 0; i < type1.length; ++i) {
      checkTypeAssignable(type1[i], type2[i], position);
    }
  }
  error(
    position,
    `Type '${getTypeName(type1)}' is not assignable to type '${getTypeName(
      type2,
    )}'`,
  );
}

export function leastUpperBound(type1: Types, type2: Types) {
  if (type1 === type2) {
    return type1;
  } else if (type1 === BuiltinTypes.Null) {
    return type2;
  } else if (type2 === BuiltinTypes.Null) {
    return type1;
  } else if (type1 instanceof Classable && type2 instanceof Classable) {
    const type1Line = getInheritanceLine(type1);
    const type2Line = getInheritanceLine(type2);

    for (let i = 0; i < type1Line.length; ++i) {
      for (let j = 0; j < type2Line.length; ++j) {
        if (type1Line[i] === type2Line[j]) {
          return type1Line[i];
        }
      }
    }
  }
}

function getInheritanceLine(type: Classable) {
  const line = [type];
  for (let trace = type.superclass; trace; trace = trace.superclass) {
    line.push(trace);
  }
  return line;
}

function getTypeName(type: Types) {
  if (type instanceof Classable || type instanceof Functionable) {
    return type.name;
  } else {
    return type;
  }
}
