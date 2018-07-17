'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var TokenType;
(function (TokenType) {
    TokenType["LEFT_PAREN"] = "LEFT_PAREN";
    TokenType["RIGHT_PAREN"] = "RIGHT_PAREN";
    TokenType["LEFT_BRACE"] = "LEFT_BRACE";
    TokenType["RIGHT_BRACE"] = "RIGHT_BRACE";
    TokenType["LEFT_BRACKET"] = "LEFT_BRACKET";
    TokenType["RIGHT_BRACKET"] = "RIGHT_BRACKET";
    TokenType["DEF"] = "DEF";
    TokenType["CLASS"] = "CLASS";
    TokenType["VAR"] = "VAR";
    TokenType["NEW"] = "NEW";
    TokenType["ARROW"] = "ARROW";
    TokenType["IF"] = "IF";
    TokenType["ELSE"] = "ELSE";
    TokenType["FOR"] = "FOR";
    TokenType["WHILE"] = "WHILE";
    TokenType["BREAK"] = "BREAK";
    TokenType["CONTINUE"] = "CONTINUE";
    TokenType["RETURN"] = "RETURN";
    TokenType["COMMA"] = "COMMA";
    TokenType["SEMICOLON"] = "SEMICOLON";
    TokenType["COLON"] = "COLON";
    TokenType["DOT"] = "DOT";
    TokenType["EQUAL"] = "EQUAL";
    TokenType["BANG_EQUAL"] = "BANG_EQUAL";
    TokenType["EQUAL_EQUAL"] = "EQUAL_EQUAL";
    TokenType["GREAT"] = "GREAT";
    TokenType["GREAT_THAN"] = "GREAT_THAN";
    TokenType["LESS"] = "LESS";
    TokenType["LESS_THAN"] = "LESS_THAN";
    TokenType["AND"] = "AND";
    TokenType["OR"] = "OR";
    TokenType["PLUS"] = "PLUS";
    TokenType["MINUS"] = "MINUS";
    TokenType["STAR"] = "STAR";
    TokenType["STAR_STAR"] = "STAR_STAR";
    TokenType["SLASH"] = "SLASH";
    TokenType["PLUS_PLUS"] = "PLUS_PLUS";
    TokenType["MINUS_MINUS"] = "MINUS_MINUS";
    TokenType["BANG"] = "BANG";
    TokenType["NUMBER"] = "NUMBER";
    TokenType["STRING"] = "STRING";
    TokenType["TRUE"] = "TRUE";
    TokenType["FALSE"] = "FALSE";
    TokenType["NULL"] = "NULL";
    TokenType["THIS"] = "THIS";
    TokenType["SUPER"] = "SUPER";
    TokenType["IDENTIFIER"] = "IDENTIFIER";
    TokenType["COMMENT"] = "COMMENT";
    TokenType["EOF"] = "EOF";
})(TokenType || (TokenType = {}));
class Token {
    constructor(type, lexeme, literal, line) {
        this.type = type;
        this.lexeme = lexeme;
        this.literal = literal;
        this.line = line;
    }
}

const reservedWords = {
    def: TokenType.DEF,
    class: TokenType.CLASS,
    var: TokenType.VAR,
    new: TokenType.NEW,
    if: TokenType.IF,
    else: TokenType.ELSE,
    for: TokenType.FOR,
    while: TokenType.WHILE,
    break: TokenType.BREAK,
    continue: TokenType.CONTINUE,
    return: TokenType.RETURN,
    and: TokenType.AND,
    or: TokenType.OR,
    null: TokenType.NULL,
    this: TokenType.THIS,
    super: TokenType.SUPER,
};
class Lexer {
    constructor(source) {
        this.source = source;
    }
    scan() {
        const tokens = [];
        this.runner = this.line = 0;
        while (this.notAtEnd()) {
            this.current = this.runner;
            const token = this.scanToken();
            if (token)
                tokens.push(token);
        }
        tokens.push(new Token(TokenType.EOF, ""));
        return tokens;
    }
    scanToken() {
        const char = this.advance();
        switch (char) {
            case "{":
                return this.generateToken(TokenType.LEFT_BRACE);
            case "}":
                return this.generateToken(TokenType.RIGHT_BRACE);
            case "(":
                return this.generateToken(TokenType.LEFT_PAREN);
            case ")":
                return this.generateToken(TokenType.RIGHT_PAREN);
            case "[":
                return this.generateToken(TokenType.LEFT_BRACKET);
            case "]":
                return this.generateToken(TokenType.RIGHT_BRACKET);
            case ".":
                return this.generateToken(TokenType.DOT);
            case ";":
                return this.generateToken(TokenType.SEMICOLON);
            case ",":
                return this.generateToken(TokenType.COMMA);
            case ":":
                return this.generateToken(TokenType.COLON);
            case "/":
                if (this.match("/")) {
                    return this.comment();
                }
                else {
                    return this.generateToken(TokenType.SLASH);
                }
            case "*":
                if (this.match("*")) {
                    return this.generateToken(TokenType.STAR_STAR);
                }
                else {
                    return this.generateToken(TokenType.STAR);
                }
            case "+":
                if (this.match("+")) {
                    return this.generateToken(TokenType.PLUS_PLUS);
                }
                else {
                    return this.generateToken(TokenType.PLUS);
                }
            case "-":
                if (this.match("-")) {
                    return this.generateToken(TokenType.MINUS_MINUS);
                }
                else {
                    return this.generateToken(TokenType.MINUS);
                }
            case "=":
                if (this.match("=")) {
                    return this.generateToken(TokenType.EQUAL_EQUAL);
                }
                else {
                    return this.generateToken(TokenType.EQUAL);
                }
            case "!":
                if (this.match("=")) {
                    return this.generateToken(TokenType.BANG_EQUAL);
                }
                else {
                    return this.generateToken(TokenType.BANG);
                }
            case ">":
                if (this.match("=")) {
                    return this.generateToken(TokenType.GREAT_THAN);
                }
                else {
                    return this.generateToken(TokenType.GREAT);
                }
            case "<":
                if (this.match("=")) {
                    return this.generateToken(TokenType.LESS_THAN);
                }
                else {
                    return this.generateToken(TokenType.LESS);
                }
            case "\n":
                this.line += 1;
                return;
            case '"':
                return this.string();
            default:
                if (this.isDigit(char)) {
                    return this.number();
                }
                else if (this.isAlpha(char)) {
                    return this.identifier();
                }
                else if (/^\s$/.test(char)) {
                    return;
                }
                else {
                    throw new Error(`${this.line}: Unexpected character ${char}.`);
                }
        }
    }
    comment() {
        while (this.peek() !== "\n" && this.notAtEnd())
            this.advance();
        return this.generateToken(TokenType.COMMENT);
    }
    string() {
        while (this.peek() !== '"' && this.notAtEnd()) {
            if (this.peek() === "\n")
                this.line += 1;
            this.advance();
        }
        if (!this.notAtEnd()) {
            throw new Error(`${this.line}: Unterminated string.`);
        }
        this.advance();
        return this.generateToken(TokenType.STRING, this.getLexeme(this.current + 1, this.runner - 1));
    }
    number() {
        this.runner -= 1;
        while (/^[0-9]$/.test(this.peek()) && this.notAtEnd())
            this.advance();
        if (this.match(".")) {
            while (/[0-9]/.test(this.peek()) && this.notAtEnd())
                this.advance();
        }
        return this.generateToken(TokenType.NUMBER, parseFloat(this.getLexeme()));
    }
    identifier() {
        while (/^\w$/.test(this.peek()) && this.notAtEnd())
            this.advance();
        const lexeme = this.getLexeme();
        if (reservedWords[lexeme]) {
            return this.generateToken(reservedWords[lexeme]);
        }
        else {
            return this.generateToken(TokenType.IDENTIFIER);
        }
    }
    generateToken(type, literal) {
        return new Token(type, this.getLexeme(), literal, this.line);
    }
    getLexeme(start = this.current, end = this.runner) {
        return this.source.substring(start, end);
    }
    isDigit(char) {
        return /^[0-9]$/.test(char);
    }
    isAlpha(char) {
        return /^\w$/.test(char);
    }
    match(char) {
        if (this.peek() !== char)
            return false;
        this.runner += 1;
        return true;
    }
    peek() {
        return this.source[this.runner];
    }
    advance() {
        return this.source[this.runner++];
    }
    notAtEnd() {
        return this.runner < this.source.length;
    }
}

exports.Lexer = Lexer;
