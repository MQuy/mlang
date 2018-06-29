import { Token } from "./token";

export class Node {}

export class ProgramNode extends Node {
  block: BlockNode;
  name: string;

  constructor(name: string, block: BlockNode) {
    super();

    this.name = name;
    this.block = block;
  }
}

export class BlockNode extends Node {
  declaration: DeclarationNode;
  compound: CompoundNode;

  constructor(declaration: DeclarationNode, compound: CompoundNode) {
    super();

    this.declaration = declaration;
    this.compound = compound;
  }
}

export class DeclarationNode extends Node {
  children: VariableDeclarationNode[];

  constructor(children: VariableDeclarationNode[]) {
    super();

    this.children = children;
  }
}

export class VariableDeclarationNode extends Node {
  name: TokenNode;
  type: TokenNode;

  constructor(name: TokenNode, type: TokenNode) {
    super();

    this.name = name;
    this.type = type;
  }
}

export class TokenNode extends Node {
  token: Token;

  constructor(token: Token) {
    super();

    this.token = token;
  }
}

export class CompoundNode extends Node {
  children: (CompoundNode | AssignmentNode)[];

  constructor(children: (CompoundNode | AssignmentNode)[]) {
    super();

    this.children = children;
  }
}

export class AssignmentNode extends Node {
  variable: TokenNode;
  expression: ExpressionNode | UnaryNode | TokenNode;

  constructor(
    variable: TokenNode,
    expression: ExpressionNode | UnaryNode | TokenNode
  ) {
    super();

    this.variable = variable;
    this.expression = expression;
  }
}

export class ExpressionNode extends Node {
  left: ExpressionNode | UnaryNode | TokenNode;
  operator: Token;
  right: ExpressionNode | UnaryNode | TokenNode;

  constructor(
    left: ExpressionNode | UnaryNode | TokenNode,
    operator: Token,
    right: ExpressionNode | UnaryNode | TokenNode
  ) {
    super();

    this.left = left;
    this.operator = operator;
    this.right = right;
  }
}

export class UnaryNode extends Node {
  operator: Token;
  operand: ExpressionNode | UnaryNode | TokenNode;

  constructor(
    operator: Token,
    operand: ExpressionNode | UnaryNode | TokenNode
  ) {
    super();

    this.operator = operator;
    this.operand = operand;
  }
}
