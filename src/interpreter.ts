import { TType } from "./token";
import * as AST from "./ast";
import { Visitor } from "./visitor";

export class Interpreter extends Visitor {
  ast: AST.ProgramNode;
  symbolTable: { [key: string]: number };

  constructor(ast: AST.ProgramNode) {
    super();

    this.ast = ast;
    this.symbolTable = {};
  }

  execute() {
    this.visit(this.ast);
  }

  visitAssignment(node: AST.AssignmentNode) {
    let name = node.variable.token.value;
    this.symbolTable[name] = +this.visit(node.expression);
  }

  visitExpression(node: AST.ExpressionNode) {
    if (node.operator.type === TType.PLUS) {
      return +this.visit(node.left) + +this.visit(node.right);
    } else if (node.operator.type === TType.MINUS) {
      return +this.visit(node.left) - +this.visit(node.right);
    } else if (node.operator.type === TType.MUL) {
      return +this.visit(node.left) * +this.visit(node.right);
    } else {
      return +this.visit(node.left) / +this.visit(node.right);
    }
  }

  visitUnary(node: AST.UnaryNode) {
    if (node.operator.type === TType.PLUS) {
      return this.visit(node.operand);
    } else {
      return -this.visit(node.operand);
    }
  }

  visitVariable(node: AST.TokenNode) {
    return this.symbolTable[node.token.value];
  }

  visitConstant(node: AST.TokenNode) {
    return +node.token.value;
  }
}
