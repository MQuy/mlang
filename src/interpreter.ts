import { TType, Token } from "./token";
import { TreeNode } from "./ast";

export class Interpreter {
  ast: TreeNode;

  constructor(ast: TreeNode) {
    this.ast = ast;
  }

  execute() {
    return this.visit(this.ast);
  }

  visit(node: TreeNode | Token) {
    if (node instanceof TreeNode) {
      if (node.left) {
        if (node.operator.type === TType.PLUS) {
          return this.visit(node.left) + this.visit(node.right);
        } else if (node.operator.type === TType.MINUS) {
          return this.visit(node.left) - this.visit(node.right);
        } else if (node.operator.type === TType.MUL) {
          return this.visit(node.left) * this.visit(node.right);
        } else {
          return this.visit(node.left) / this.visit(node.right);
        }
      } else if (node.right instanceof TreeNode) {
        return this.visit(node.right);
      } else {
        if (node.operator.type === TType.PLUS) {
          return node.right.value;
        } else {
          return -node.right.value;
        }
      }
    } else {
      return node.value;
    }
  }
}
