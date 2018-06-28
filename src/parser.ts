import { TType, Token } from "./token";
import * as AST from "./ast";

/**
 * program : PROGRAM variable SEMI block dot
 * block : declaration compound_statement
 * declaration : VAR (variable_declaration SEMI)+ | empty
 * variable_declaration : variable (COMMA ID)* COLON typespec
 * typespec : INTEGER | REAL
 * compound_statement : BEGIN statement (SEMI statement)* END
 * statement : compound_statement | assignment_statement
 * assignment_statement : variable ASSIGN expression
 * empty :
 * expression : term ((PLUS | MINUS) term)*
 * term : factor ((MUL | DIV) factor)*
 * factor : (PLUS | MINUS) expression | INTEGER_CONST | REAL_CONST | LPAREN expression RPAREN | variable
 * variable : ID
 */

export class Parser {
  tokens: Token[];
  cPointer: number;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.cPointer = 0;
  }

  execute() {
    return this.expression();
  }

  eat(type: TType) {
    let token = this.tokens[this.cPointer];

    if (token.type === type) {
      this.cPointer += 1;
      return token;
    } else {
      throw new Error("Wrong syntax");
    }
  }

  program(): AST.ProgramNode {
    this.eat(TType.PROGRAM);
    let variableNode = this.variable();
    this.eat(TType.SEMI);
    let blockNode = this.block();
    this.eat(TType.DOT);
    return new AST.ProgramNode(variableNode.token.value.toString(), blockNode);
  }

  variable(): AST.TokenNode {
    let token = this.eat(TType.VARIABLE_NAME);

    return new AST.TokenNode(token);
  }

  block(): AST.BlockNode {
    let declarationNode = this.declaration();
    let compoundNode = this.compound();

    return new AST.BlockNode(declarationNode, compoundNode);
  }

  declaration(): AST.DeclarationNode {
    let declarations: AST.VariableDeclarationNode[] = [];

    if (this.getCurrentToken().type === TType.VAR) {
      this.eat(TType.VAR);
      while (this.getCurrentToken().type === TType.VARIABLE_NAME) {
        let nodes = this.variableDeclaration();
        declarations.push(...nodes);
        this.eat(TType.SEMI);
      }
    }
    return new AST.DeclarationNode(declarations);
  }

  variableDeclaration(): AST.VariableDeclarationNode[] {
    let variableName = this.eat(TType.VARIABLE_NAME);
    let declarations = [new AST.TokenNode(variableName)];

    while (this.getCurrentToken().type === TType.COMMA) {
      this.eat(TType.COMMA);
      variableName = this.eat(TType.VARIABLE_NAME);
      declarations.push(new AST.TokenNode(variableName));
    }
    this.eat(TType.COLON);
    let type = this.type();
    return declarations.map(
      declaration => new AST.VariableDeclarationNode(declaration, type)
    );
  }

  type(): AST.TokenNode {
    let token = this.getCurrentToken();

    if (token.type === TType.INTEGER) {
      return new AST.TokenNode(this.eat(TType.INTEGER));
    } else if (token.type === TType.REAL) {
      return new AST.TokenNode(this.eat(TType.REAL));
    } else {
      throw new Error("Wrong syntax");
    }
  }

  compound(): AST.CompoundNode {
    this.eat(TType.BEGIN);
    let statement = this.statement();
    let statements = [statement];

    while (this.getCurrentToken().type === TType.SEMI) {
      this.eat(TType.SEMI);
      statement = this.statement();
      statements.push(statement);
    }
    return new AST.CompoundNode(statements);
  }

  statement(): AST.CompoundNode | AST.AssignmentNode {
    let token = this.getCurrentToken();

    if (token.type === TType.BEGIN) {
      return this.compound();
    } else if (token.type === TType.VARIABLE_NAME) {
      return this.assignment();
    } else {
      throw new Error("Wrong syntax");
    }
  }

  assignment(): AST.AssignmentNode {
    let variable = this.eat(TType.VARIABLE_NAME);
    this.eat(TType.ASSIGN);
    let expression = this.expression();

    return new AST.AssignmentNode(new AST.TokenNode(variable), expression);
  }

  expression(): AST.ExpressionNode {
    let node = this.term();
    let token = this.getCurrentToken();

    while (token && (token.type === TType.PLUS || token.type === TType.MINUS)) {
      this.cPointer += 1;
      node = new AST.ExpressionNode(node, token, this.term());
      token = this.getCurrentToken();
    }

    return node;
  }

  term(): AST.ExpressionNode {
    let node = this.term();
    let token = this.getCurrentToken();

    while (token && (token.type === TType.MUL || token.type === TType.DIV)) {
      this.cPointer += 1;
      node = new AST.ExpressionNode(node, token, this.factor());
      token = this.getCurrentToken();
    }

    return node;
  }

  factor(): AST.ExpressionNode | AST.UnaryNode | AST.TokenNode {
    let token = this.getCurrentToken();

    if (token.type === TType.LPAREN) {
      this.eat(TType.LPAREN);
      const node = this.expression();
      this.eat(TType.RPAREN);
      return node;
    } else if (token.type === TType.PLUS) {
      this.eat(TType.PLUS);
      return new AST.UnaryNode(token, this.expression());
    } else if (token.type === TType.MINUS) {
      this.eat(TType.MINUS);
      return new AST.UnaryNode(token, this.expression());
    } else if (token.type === TType.REAL_CONST) {
      this.eat(TType.REAL_CONST);
      return new AST.TokenNode(token);
    } else if (token.type === TType.INTEGER_CONST) {
      this.eat(TType.INTEGER_CONST);
      return new AST.TokenNode(token);
    } else {
      return this.variable();
    }
  }

  getCurrentToken() {
    return this.tokens[this.cPointer];
  }
}
