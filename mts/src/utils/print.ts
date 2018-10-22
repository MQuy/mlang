import { Token } from "../token";
import { IRPosition } from "../ast/types";

export function error(token: Token | IRPosition, errorMessage: string): never {
  throw new Error(`Line ${token.line}:${token.column} ${errorMessage}`);
}
