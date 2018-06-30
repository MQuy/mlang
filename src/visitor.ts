import * as AST from "./ast";
import { TType } from "./token";

export class Visitor {
  visitProgram(node: AST.ProgramNode) {
    this.visitBlock(node.block);
  }

  visitBlock(node: AST.BlockNode) {
    node.declaration.children.forEach(child =>
      this.visitVariableDeclaration(child)
    );
    this.visitCompound(node.compound);
  }

  visitVariableDeclaration(node: AST.VariableDeclarationNode) {}

  visitCompound(node: AST.CompoundNode) {
    node.children.forEach(child => this.visit(child));
  }

  visitAssignment(node: AST.AssignmentNode) {
    this.visitVariable(node.variable);
    this.visitExpression(node.expression);
  }

  visitVariable(node: AST.TokenNode) {}

  visitExpression(node: AST.ExpressionNode | AST.UnaryNode | AST.TokenNode) {
    if (node instanceof AST.ExpressionNode) {
      this.visit(node.left);
      this.visit(node.right);
    } else if (node instanceof AST.UnaryNode) {
      this.visit(node.operand);
    }
  }

  visitUnary(node: AST.UnaryNode) {}

  visitConstant(node: AST.TokenNode) {}

  visit(node: AST.Node) {
    if (node instanceof AST.ProgramNode) {
      return this.visitProgram(node);
    } else if (node instanceof AST.BlockNode) {
      return this.visitBlock(node);
    } else if (node instanceof AST.VariableDeclarationNode) {
      return this.visitVariableDeclaration(node);
    } else if (node instanceof AST.CompoundNode) {
      return this.visitCompound(node);
    } else if (node instanceof AST.AssignmentNode) {
      return this.visitAssignment(node);
    } else if (node instanceof AST.ExpressionNode) {
      return this.visitExpression(node);
    } else if (node instanceof AST.UnaryNode) {
      return this.visitUnary(node);
    } else if (node instanceof AST.TokenNode) {
      if (node.token.type === TType.VARIABLE_NAME) {
        return this.visitVariable(node);
      } else if (
        node.token.type === TType.INTEGER_CONST ||
        node.token.type === TType.REAL_CONST
      ) {
        return this.visitConstant(node);
      }
    } else {
      throw new Error("Cannot find suitable visit");
    }
  }
}
