'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var TType;
(function (TType) {
    TType["INTEGER"] = "INTEGER";
    TType["REAL"] = "REAL";
    TType["INTEGER_CONST"] = "INTEGER_CONST";
    TType["REAL_CONST"] = "REAL_CONST";
    TType["PLUS"] = "PLUS";
    TType["MINUS"] = "MINUS";
    TType["MUL"] = "MUL";
    TType["DIV"] = "DIV";
    TType["LPAREN"] = "LPAREN";
    TType["RPAREN"] = "RPAREN";
    TType["VARIABLE_NAME"] = "VARIABLE_NAME";
    TType["ASSIGN"] = "ASSIGN";
    TType["BEGIN"] = "BEGIN";
    TType["END"] = "END";
    TType["SEMI"] = "SEMI";
    TType["DOT"] = "DOT";
    TType["PROGRAM"] = "PROGRAM";
    TType["VAR"] = "VAR";
    TType["COLON"] = "COLON";
    TType["COMMA"] = "COMMA";
})(TType || (TType = {}));

class Lexer {
    constructor(source) {
        this.source = source;
        this.cPointer = 0;
    }
    execute() {
        const tokens = [];
        let char = this.source[this.cPointer];
        let token, cPointer;
        while (char) {
            if (char === undefined) {
                break;
            }
            else if (/[\d]/.test(char)) {
                let { token } = this.getNextToken(/[\d\.]/, true);
                tokens.push({
                    value: token,
                    type: /\d+/.test(token) ? TType.INTEGER_CONST : TType.REAL_CONST
                });
                char = this.source[this.cPointer];
            }
            else if (char === "P" &&
                ({ token, cPointer } = this.getNextToken(/[PROGRAM]/)) &&
                token === TType.PROGRAM) {
                tokens.push({ value: token, type: TType.PROGRAM });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (char === "I" &&
                ({ token, cPointer } = this.getNextToken(/[INTEGER]/)) &&
                token === TType.INTEGER) {
                tokens.push({ value: token, type: TType.INTEGER });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (char === "R" &&
                ({ token, cPointer } = this.getNextToken(/[REAL]/)) &&
                token === TType.REAL) {
                tokens.push({ value: token, type: TType.REAL });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (char === "V" &&
                ({ token, cPointer } = this.getNextToken(/[VAR]/)) &&
                token === TType.VAR) {
                tokens.push({ value: token, type: TType.VAR });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (char === "B" &&
                ({ token, cPointer } = this.getNextToken(/[BEGIN]/)) &&
                token === TType.BEGIN) {
                tokens.push({ value: token, type: TType.BEGIN });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (char === "E" &&
                ({ token, cPointer } = this.getNextToken(/[END]/)) &&
                token === TType.END) {
                tokens.push({ value: token, type: TType.END });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (char === ":" &&
                ({ token, cPointer } = this.getNextToken(/[:=]/)) &&
                token === ":=") {
                tokens.push({ value: token, type: TType.ASSIGN });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else if (/[a-z]/.test(char) &&
                ({ token, cPointer } = this.getNextToken(/[a-z]/))) {
                tokens.push({ value: token, type: TType.VARIABLE_NAME });
                this.cPointer = cPointer;
                char = this.source[this.cPointer];
            }
            else {
                if (char === "+") {
                    tokens.push({ value: char, type: TType.PLUS });
                }
                else if (char === "-") {
                    tokens.push({ value: char, type: TType.MINUS });
                }
                else if (char === "*") {
                    tokens.push({ value: char, type: TType.MUL });
                }
                else if (char === "/") {
                    tokens.push({ value: char, type: TType.DIV });
                }
                else if (char === "(") {
                    tokens.push({ value: char, type: TType.LPAREN });
                }
                else if (char === ")") {
                    tokens.push({ value: char, type: TType.RPAREN });
                }
                else if (char === ".") {
                    tokens.push({ value: char, type: TType.DOT });
                }
                else if (char === ";") {
                    tokens.push({ value: char, type: TType.SEMI });
                }
                else if (char === ":") {
                    tokens.push({ value: char, type: TType.COLON });
                }
                else if (char === ",") {
                    tokens.push({ value: char, type: TType.COMMA });
                }
                else if (!/\s/.test(char)) {
                    throw Error(`Wrong syntax: ${char}`);
                }
                char = this.source[++this.cPointer];
            }
        }
        return tokens;
    }
    getNextToken(pattern, move = false) {
        let runner = this.cPointer;
        let char = this.source[runner];
        let token = "";
        while (pattern.test(char)) {
            token += char;
            char = this.source[++runner];
        }
        if (move) {
            this.cPointer = runner;
        }
        return { token, cPointer: runner };
    }
}

class Node {
}
class ProgramNode extends Node {
    constructor(name, block) {
        super();
        this.name = name;
        this.block = block;
    }
}
class BlockNode extends Node {
    constructor(declaration, compound) {
        super();
        this.declaration = declaration;
        this.compound = compound;
    }
}
class DeclarationNode extends Node {
    constructor(children) {
        super();
        this.children = children;
    }
}
class VariableDeclarationNode extends Node {
    constructor(name, type) {
        super();
        this.name = name;
        this.type = type;
    }
}
class TokenNode extends Node {
    constructor(token) {
        super();
        this.token = token;
    }
}
class CompoundNode extends Node {
    constructor(children) {
        super();
        this.children = children;
    }
}
class AssignmentNode extends Node {
    constructor(variable, expression) {
        super();
        this.variable = variable;
        this.expression = expression;
    }
}
class ExpressionNode extends Node {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
}
class UnaryNode extends Node {
    constructor(operator, operand) {
        super();
        this.operator = operator;
        this.operand = operand;
    }
}

/**
 * program : PROGRAM variable SEMI block dot
 * block : declaration compound_statement
 * declaration : VAR (variable_declaration SEMI)+ | empty
 * variable_declaration : variable (COMMA ID)* COLON typespec
 * typespec : INTEGER | REAL
 * compound_statement : BEGIN statement (SEMI statement)* SEMI END
 * statement : compound_statement | assignment_statement
 * assignment_statement : variable ASSIGN expression
 * empty :
 * expression : term ((PLUS | MINUS) term)*
 * term : factor ((MUL | DIV) factor)*
 * factor : (PLUS | MINUS) expression | INTEGER_CONST | REAL_CONST | LPAREN expression RPAREN | variable
 * variable : ID
 */
class Parser {
    constructor(tokens) {
        this.tokens = tokens;
        this.cPointer = 0;
    }
    execute() {
        return this.program();
    }
    eat(type) {
        let token = this.tokens[this.cPointer];
        if (token.type === type) {
            this.cPointer += 1;
            return token;
        }
        else {
            throw new Error("Wrong syntax");
        }
    }
    program() {
        this.eat(TType.PROGRAM);
        let variableNode = this.variable();
        this.eat(TType.SEMI);
        let blockNode = this.block();
        this.eat(TType.DOT);
        return new ProgramNode(variableNode.token.value.toString(), blockNode);
    }
    variable() {
        let token = this.eat(TType.VARIABLE_NAME);
        return new TokenNode(token);
    }
    block() {
        let declarationNode = this.declaration();
        let compoundNode = this.compound();
        return new BlockNode(declarationNode, compoundNode);
    }
    declaration() {
        let declarations = [];
        if (this.getCurrentToken().type === TType.VAR) {
            this.eat(TType.VAR);
            while (this.getCurrentToken().type === TType.VARIABLE_NAME) {
                let nodes = this.variableDeclaration();
                declarations.push(...nodes);
                this.eat(TType.SEMI);
            }
        }
        return new DeclarationNode(declarations);
    }
    variableDeclaration() {
        let variableName = this.eat(TType.VARIABLE_NAME);
        let declarations = [new TokenNode(variableName)];
        while (this.getCurrentToken().type === TType.COMMA) {
            this.eat(TType.COMMA);
            variableName = this.eat(TType.VARIABLE_NAME);
            declarations.push(new TokenNode(variableName));
        }
        this.eat(TType.COLON);
        let type = this.type();
        return declarations.map(declaration => new VariableDeclarationNode(declaration, type));
    }
    type() {
        let token = this.getCurrentToken();
        if (token.type === TType.INTEGER) {
            return new TokenNode(this.eat(TType.INTEGER));
        }
        else if (token.type === TType.REAL) {
            return new TokenNode(this.eat(TType.REAL));
        }
        else {
            throw new Error("Wrong syntax");
        }
    }
    compound() {
        this.eat(TType.BEGIN);
        let statement;
        let statements = [];
        while (true) {
            let token = this.getCurrentToken();
            if (token.type === TType.BEGIN) {
                statement = this.compound();
                statements.push(statement);
                this.eat(TType.SEMI);
            }
            else if (token.type === TType.VARIABLE_NAME) {
                statement = this.assignment();
                statements.push(statement);
                this.eat(TType.SEMI);
            }
            else if (token.type === TType.END) {
                this.eat(TType.END);
                break;
            }
        }
        return new CompoundNode(statements);
    }
    statement() {
        let token = this.getCurrentToken();
        if (token.type === TType.BEGIN) {
            return this.compound();
        }
        else if (token.type === TType.VARIABLE_NAME) {
            return this.assignment();
        }
        else {
            throw new Error("Wrong syntax");
        }
    }
    assignment() {
        let variable = this.eat(TType.VARIABLE_NAME);
        this.eat(TType.ASSIGN);
        let expression = this.expression();
        return new AssignmentNode(new TokenNode(variable), expression);
    }
    expression() {
        let node = this.term();
        let token = this.getCurrentToken();
        while (token && (token.type === TType.PLUS || token.type === TType.MINUS)) {
            this.cPointer += 1;
            node = new ExpressionNode(node, token, this.term());
            token = this.getCurrentToken();
        }
        return node;
    }
    term() {
        let node = this.factor();
        let token = this.getCurrentToken();
        while (token && (token.type === TType.MUL || token.type === TType.DIV)) {
            this.cPointer += 1;
            node = new ExpressionNode(node, token, this.factor());
            token = this.getCurrentToken();
        }
        return node;
    }
    factor() {
        let token = this.getCurrentToken();
        if (token.type === TType.LPAREN) {
            this.eat(TType.LPAREN);
            const node = this.expression();
            this.eat(TType.RPAREN);
            return node;
        }
        else if (token.type === TType.PLUS) {
            this.eat(TType.PLUS);
            return new UnaryNode(token, this.expression());
        }
        else if (token.type === TType.MINUS) {
            this.eat(TType.MINUS);
            return new UnaryNode(token, this.expression());
        }
        else if (token.type === TType.REAL_CONST) {
            this.eat(TType.REAL_CONST);
            return new TokenNode(token);
        }
        else if (token.type === TType.INTEGER_CONST) {
            this.eat(TType.INTEGER_CONST);
            return new TokenNode(token);
        }
        else {
            return this.variable();
        }
    }
    getCurrentToken() {
        return this.tokens[this.cPointer];
    }
}

class Visitor {
    visitProgram(node) {
        this.visitBlock(node.block);
    }
    visitBlock(node) {
        node.declaration.children.forEach(child => this.visitVariableDeclaration(child));
        this.visitCompound(node.compound);
    }
    visitVariableDeclaration(node) { }
    visitCompound(node) {
        node.children.forEach(child => this.visit(child));
    }
    visitAssignment(node) {
        this.visitVariable(node.variable);
        this.visitExpression(node.expression);
    }
    visitVariable(node) { }
    visitExpression(node) {
        if (node instanceof ExpressionNode) {
            this.visit(node.left);
            this.visit(node.right);
        }
        else if (node instanceof UnaryNode) {
            this.visit(node.operand);
        }
    }
    visitUnary(node) { }
    visitConstant(node) { }
    visit(node) {
        if (node instanceof ProgramNode) {
            return this.visitProgram(node);
        }
        else if (node instanceof BlockNode) {
            return this.visitBlock(node);
        }
        else if (node instanceof VariableDeclarationNode) {
            return this.visitVariableDeclaration(node);
        }
        else if (node instanceof CompoundNode) {
            return this.visitCompound(node);
        }
        else if (node instanceof AssignmentNode) {
            return this.visitAssignment(node);
        }
        else if (node instanceof ExpressionNode) {
            return this.visitExpression(node);
        }
        else if (node instanceof UnaryNode) {
            return this.visitUnary(node);
        }
        else if (node instanceof TokenNode) {
            if (node.token.type === TType.VARIABLE_NAME) {
                return this.visitVariable(node);
            }
            else if (node.token.type === TType.INTEGER_CONST ||
                node.token.type === TType.REAL_CONST) {
                return this.visitConstant(node);
            }
        }
        else {
            throw new Error("Cannot find suitable visit");
        }
    }
}

class VarSymbol {
    constructor(name, type) {
        this.name = name;
        this.type = type;
    }
}
class BuiltinSymbol {
    constructor(name) {
        this.name = name;
    }
    static getBuiltinSymbols() {
        return {
            INTEGER: new BuiltinSymbol("INTEGER"),
            REAL: new BuiltinSymbol("REAL")
        };
    }
}
class SymbolTable {
    constructor() {
        this.builtins = BuiltinSymbol.getBuiltinSymbols();
        this.symbols = {};
    }
    define(name, type) {
        this.symbols[name] = new VarSymbol(name, type);
    }
    lookup(name) {
        return this.symbols[name];
    }
}
class SemanticAnalyzer extends Visitor {
    constructor(ast) {
        super();
        this.ast = ast;
        this.symbolTable = new SymbolTable();
    }
    execute() {
        this.visit(this.ast);
    }
    visitVariableDeclaration({ name, type }) {
        if (this.symbolTable.lookup(name.token.value)) {
            throw new Error(`${name.token.value} is already declared`);
        }
        let typeSymbol = this.symbolTable.builtins[type.token.value];
        this.symbolTable.define(name.token.value, typeSymbol);
    }
    visitAssignment({ variable, expression }) {
        if (!this.symbolTable.lookup(variable.token.value)) {
            throw new Error(`${variable.token.value} is not defined`);
        }
        this.visit(expression);
    }
    visitVariable(node) {
        if (!this.symbolTable.lookup(node.token.value)) {
            throw new Error(`${node.token.value} is not defined`);
        }
    }
}

class Interpreter extends Visitor {
    constructor(ast) {
        super();
        this.ast = ast;
        this.symbolTable = {};
    }
    execute() {
        this.visit(this.ast);
    }
    visitAssignment(node) {
        let name = node.variable.token.value;
        this.symbolTable[name] = +this.visit(node.expression);
    }
    visitExpression(node) {
        if (node.operator.type === TType.PLUS) {
            return +this.visit(node.left) + +this.visit(node.right);
        }
        else if (node.operator.type === TType.MINUS) {
            return +this.visit(node.left) - +this.visit(node.right);
        }
        else if (node.operator.type === TType.MUL) {
            return +this.visit(node.left) * +this.visit(node.right);
        }
        else {
            return +this.visit(node.left) / +this.visit(node.right);
        }
    }
    visitUnary(node) {
        if (node.operator.type === TType.PLUS) {
            return this.visit(node.operand);
        }
        else {
            return -this.visit(node.operand);
        }
    }
    visitVariable(node) {
        return this.symbolTable[node.token.value];
    }
    visitConstant(node) {
        return +node.token.value;
    }
}

exports.Lexer = Lexer;
exports.Parser = Parser;
exports.SemanticAnalyzer = SemanticAnalyzer;
exports.Interpreter = Interpreter;
