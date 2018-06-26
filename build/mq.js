'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var TType;
(function (TType) {
    TType[TType["NUMBER"] = 0] = "NUMBER";
    TType[TType["PLUS"] = 1] = "PLUS";
    TType[TType["MINUS"] = 2] = "MINUS";
    TType[TType["MUL"] = 3] = "MUL";
    TType[TType["DIV"] = 4] = "DIV";
    TType[TType["OPEN_PAREN"] = 5] = "OPEN_PAREN";
    TType[TType["CLOSE_PAREN"] = 6] = "CLOSE_PAREN";
})(TType || (TType = {}));

class Lexer {
    constructor(source) {
        this.source = source;
        this.cPointer = 0;
    }
    execute() {
        const tokens = [];
        let char = this.source[this.cPointer];
        while (char) {
            if (char === undefined) {
                break;
            }
            else if (/[\d\.]/.test(char)) {
                tokens.push({ value: this.extractNumber(), type: TType.NUMBER });
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
                    tokens.push({ value: char, type: TType.OPEN_PAREN });
                }
                else if (char === ")") {
                    tokens.push({ value: char, type: TType.CLOSE_PAREN });
                }
                else if (!/\s/.test(char)) {
                    throw Error("Wrong syntax");
                }
                char = this.source[++this.cPointer];
            }
        }
        return tokens;
    }
    extractNumber() {
        let char = this.source[this.cPointer];
        let number = "";
        while (/[\d\.]/.test(char)) {
            number += char;
            char = this.source[++this.cPointer];
        }
        return parseFloat(number);
    }
}

class TreeNode {
    constructor({ left, operator, right }) {
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
}

/**
 * expression = factor ((PLUS|MINUS) factor)*
 * factor = term ((MUL|DIV) term) *
 * term = NUMBER | OPEN_PAREN expression CLOSE_PAREN | (PLUS|MINUS) expression
 */
class Parser {
    constructor(tokens) {
        this.tokens = tokens;
        this.cPointer = 0;
    }
    execute() {
        return this.expression();
    }
    expression() {
        let node = this.factor();
        let token = this.currentToken;
        while (token && (token.type === TType.PLUS || token.type === TType.MINUS)) {
            this.cPointer += 1;
            node = new TreeNode({
                left: node,
                operator: token,
                right: this.factor()
            });
            token = this.currentToken;
        }
        return node;
    }
    factor() {
        let node = this.term();
        let token = this.currentToken;
        while (token && (token.type === TType.MUL || token.type === TType.DIV)) {
            this.cPointer += 1;
            node = new TreeNode({
                left: node,
                operator: token,
                right: this.factor()
            });
            token = this.currentToken;
        }
        return node;
    }
    term() {
        let token = this.currentToken;
        if (token.type === TType.OPEN_PAREN) {
            this.cPointer += 1;
            const node = this.expression();
            this.cPointer += 1;
            return node;
        }
        else if (token.type === TType.PLUS || token.type === TType.MINUS) {
            this.cPointer += 1;
            return new TreeNode({ operator: token, right: this.expression() });
        }
        else {
            this.cPointer += 1;
            return new TreeNode({
                operator: { type: TType.PLUS, value: "+" },
                right: token
            });
        }
    }
    get currentToken() {
        return this.tokens[this.cPointer];
    }
    get nextToken() {
        return this.tokens[this.cPointer + 1];
    }
}

class Interpreter {
    constructor(ast) {
        this.ast = ast;
    }
    execute() {
        return this.visit(this.ast);
    }
    visit(node) {
        if (node instanceof TreeNode) {
            if (node.left) {
                if (node.operator.type === TType.PLUS) {
                    return this.visit(node.left) + this.visit(node.right);
                }
                else if (node.operator.type === TType.MINUS) {
                    return this.visit(node.left) - this.visit(node.right);
                }
                else if (node.operator.type === TType.MUL) {
                    return this.visit(node.left) * this.visit(node.right);
                }
                else {
                    return this.visit(node.left) / this.visit(node.right);
                }
            }
            else if (node.right instanceof TreeNode) {
                return this.visit(node.right);
            }
            else {
                if (node.operator.type === TType.PLUS) {
                    return node.right.value;
                }
                else {
                    return -node.right.value;
                }
            }
        }
        else {
            return node.value;
        }
    }
}

exports.Lexer = Lexer;
exports.Parser = Parser;
exports.Interpreter = Interpreter;
