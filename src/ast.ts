import { Token } from "./token";

export class TreeNode {
  left?: TreeNode | Token;
  operator: Token;
  right: TreeNode | Token;

  constructor({ left, operator, right }: TreeNode) {
    this.left = left;
    this.operator = operator;
    this.right = right;
  }
}
