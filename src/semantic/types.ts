import { Classable } from "./classable";

export function checkInheritance(type1: any, type2: any) {
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
  throw new Error(`Cannot assign ${type1} to ${type2}`);
}

export enum BuiltinTypes {
  String = "String",
  Number = "Number",
  Null = "Null",
  Boolean = "Boolean",
}
