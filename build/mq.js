'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var TokenType;
(function (TokenType) {
    // Single-character tokens.
    TokenType["LEFT_PAREN"] = "LEFT_PAREN";
    TokenType["RIGHT_PAREN"] = "RIGHT_PAREN";
    TokenType["LEFT_BRACE"] = "LEFT_BRACE";
    TokenType["RIGHT_BRACE"] = "RIGHT_BRACE";
    TokenType["COMMA"] = "COMMA";
    TokenType["DOT"] = "DOT";
    TokenType["MINUS"] = "MINUS";
    TokenType["PLUS"] = "PLUS";
    TokenType["SEMICOLON"] = "SEMICOLON";
    TokenType["SLASH"] = "SLASH";
    TokenType["STAR"] = "STAR";
    // One or two character tokens.
    TokenType["BANG"] = "BANG";
    TokenType["BANG_EQUAL"] = "BANG_EQUAL";
    TokenType["EQUAL"] = "EQUAL";
    TokenType["EQUAL_EQUAL"] = "EQUAL_EQUAL";
    TokenType["GREATER"] = "GREATER";
    TokenType["GREATER_EQUAL"] = "GREATER_EQUAL";
    TokenType["LESS"] = "LESS";
    TokenType["LESS_EQUAL"] = "LESS_EQUAL";
    // Literals.
    TokenType["IDENTIFIER"] = "IDENTIFIER";
    TokenType["STRING"] = "STRING";
    TokenType["NUMBER"] = "NUMBER";
    // Keywords.
    TokenType["AND"] = "AND";
    TokenType["CLASS"] = "CLASS";
    TokenType["ELSE"] = "ELSE";
    TokenType["FALSE"] = "FALSE";
    TokenType["FUN"] = "FUN";
    TokenType["FOR"] = "FOR";
    TokenType["IF"] = "IF";
    TokenType["NIL"] = "NIL";
    TokenType["OR"] = "OR";
    TokenType["PRINT"] = "PRINT";
    TokenType["RETURN"] = "RETURN";
    TokenType["SUPER"] = "SUPER";
    TokenType["THIS"] = "THIS";
    TokenType["TRUE"] = "TRUE";
    TokenType["VAR"] = "VAR";
    TokenType["WHILE"] = "WHILE";
    TokenType["EOF"] = "EOF";
})(TokenType || (TokenType = {}));
class Token {
    constructor(type, lexeme, liternal, line) {
        this.type = type;
        this.lexeme = lexeme;
        this.liternal = liternal;
        this.line = line;
    }
}

const reservedWords = {
    and: TokenType.AND,
    class: TokenType.CLASS,
    else: TokenType.ELSE,
    false: TokenType.FALSE,
    for: TokenType.FOR,
    fun: TokenType.FUN,
    if: TokenType.IF,
    nil: TokenType.NIL,
    or: TokenType.OR,
    print: TokenType.PRINT,
    return: TokenType.RETURN,
    super: TokenType.SUPER,
    this: TokenType.THIS,
    true: TokenType.TRUE,
    var: TokenType.VAR,
    while: TokenType.WHILE
};
class Lexer {
    constructor(source) {
        this.source = source;
        this.current = 0;
        this.line = 0;
        this.start = 0;
    }
    scan() {
        this.tokens = [];
        while (!this.isAtEnd()) {
            this.start = this.current;
            this.scanToken();
        }
        this.tokens.push(new Token(TokenType.EOF, "", undefined, this.line));
        return;
    }
    scanToken() {
        let c = this.advance();
        switch (c) {
            case "(":
                this.addToken(TokenType.LEFT_PAREN);
                break;
            case ")":
                this.addToken(TokenType.RIGHT_PAREN);
                break;
            case "{":
                this.addToken(TokenType.LEFT_BRACE);
                break;
            case "}":
                this.addToken(TokenType.RIGHT_BRACE);
                break;
            case ",":
                this.addToken(TokenType.COMMA);
                break;
            case ".":
                this.addToken(TokenType.DOT);
                break;
            case "-":
                this.addToken(TokenType.MINUS);
                break;
            case "+":
                this.addToken(TokenType.PLUS);
                break;
            case ";":
                this.addToken(TokenType.SEMICOLON);
                break;
            case "*":
                this.addToken(TokenType.STAR);
                break;
            case "!":
                this.addToken(this.match("=") ? TokenType.BANG_EQUAL : TokenType.BANG);
                break;
            case "=":
                this.addToken(this.match("=") ? TokenType.EQUAL_EQUAL : TokenType.EQUAL);
                break;
            case "<":
                this.addToken(this.match("=") ? TokenType.LESS_EQUAL : TokenType.LESS);
                break;
            case ">":
                this.addToken(this.match("=") ? TokenType.GREATER_EQUAL : TokenType.GREATER);
                break;
            case "/":
                if (this.match("/")) {
                    // A comment goes until the end of the line.
                    while (this.peek() !== "\n" && !this.isAtEnd())
                        this.advance();
                }
                else {
                    this.addToken(TokenType.SLASH);
                }
                break;
            case " ":
            case "\r":
            case "\t":
                // Ignore whitespace.
                break;
            case "\n":
                this.line++;
                break;
            case '"':
                this.string();
                break;
            default:
                if (this.isDigit(c)) {
                    this.number();
                }
                else if (this.isAlpha(c)) {
                    this.identifier();
                }
                else {
                    throw new Error(`${this.line}: Unexpected character ${c}.`);
                }
        }
    }
    identifier() {
        while (this.isAlphaNumeric(this.peek()))
            this.advance();
        let text = this.source.substring(this.start, this.current);
        let type = reservedWords[text];
        if (type == null)
            type = TokenType.IDENTIFIER;
        this.addToken(type);
    }
    isAlphaNumeric(c) {
        return this.isAlpha(c) || this.isDigit(c);
    }
    isAlpha(c) {
        return (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "_";
    }
    peekNext() {
        if (this.current + 1 >= this.source.length)
            return "\0";
        return this.source.charAt(this.current + 1);
    }
    number() {
        while (this.isDigit(this.peek()))
            this.advance();
        // Look for a fractional part.
        if (this.peek() == "." && this.isDigit(this.peekNext())) {
            // Consume the "."
            this.advance();
            while (this.isDigit(this.peek()))
                this.advance();
        }
        this.addToken(TokenType.NUMBER, parseFloat(this.source.substring(this.start, this.current)));
    }
    isDigit(c) {
        return c >= "0" && c <= "9";
    }
    string() {
        while (this.peek() !== '"' && !this.isAtEnd()) {
            if (this.peek() === "\n")
                this.line++;
            this.advance();
        }
        // Unterminated string.
        if (this.isAtEnd()) {
            throw new Error(`${this.line}: Unterminated string.`);
            return;
        }
        // The closing ".
        this.advance();
        // Trim the surrounding quotes.
        let value = this.source.substring(this.start + 1, this.current - 1);
        this.addToken(TokenType.STRING, value);
    }
    peek() {
        if (this.isAtEnd())
            return "\0";
        return this.source[this.current];
    }
    match(char) {
        if (this.isAtEnd())
            return false;
        if (this.source[this.current] !== char)
            return false;
        this.current += 1;
        return true;
    }
    addToken(type, liternal) {
        let text = this.source.substring(this.start, this.current);
        this.tokens.push(new Token(type, text, liternal, this.line));
    }
    isAtEnd() {
        return this.current >= this.source.length;
    }
    advance() {
        return this.source[this.current++];
    }
}

exports.Lexer = Lexer;
