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
    toString() {
        return `Line: ${this.line} - `;
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
        return this.tokens;
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

class AstNode {
}
class Statement extends AstNode {
}
class Expression extends Statement {
}

class BlockStatement extends Statement {
    constructor(statements) {
        super();
        this.statements = statements;
    }
}
class FunctionStatement extends Statement {
    constructor(name, parameters, methods) {
        super();
        this.name = name;
        this.parameters = parameters;
        this.methods = methods;
    }
}
class ClassStatement extends Statement {
    constructor(name, methods, superclass) {
        super();
        this.name = name;
        this.methods = methods;
        this.superclass = superclass;
    }
}
class ExpressionStatement extends Statement {
    constructor(expression) {
        super();
        this.expression = expression;
    }
}
class IfStatement extends Statement {
    constructor(condition, thenBranch, elseBranch) {
        super();
        this.condition = condition;
        this.thenBranch = thenBranch;
        this.elseBranch = elseBranch;
    }
}
class PrintStatement extends Statement {
    constructor(expression) {
        super();
        this.expression = expression;
    }
}
class ReturnStatement extends Statement {
    constructor(value) {
        super();
        this.value = value;
    }
}
class VarStatement extends Statement {
    constructor(name, initializer) {
        super();
        this.name = name;
        this.initializer = initializer;
    }
}
class WhileStatement extends Statement {
    constructor(body, condition) {
        super();
        this.body = body;
        this.condition = condition;
    }
}

class AssignExpression extends Expression {
    constructor(name, expression) {
        super();
        this.name = name;
        this.expression = expression;
    }
}
class BinaryExpression extends Expression {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
}
class CallExpression extends Expression {
    constructor(callee, args) {
        super();
        this.callee = callee;
        this.arguments = args;
    }
}
class GetExpression extends Expression {
    constructor(object, name) {
        super();
        this.object = object;
        this.name = name;
    }
}
class GroupingExpression extends Expression {
    constructor(expression) {
        super();
        this.expression = expression;
    }
}
class LiteralExpression extends Expression {
    constructor(value) {
        super();
        this.value = value;
    }
}
class LogicalExpression extends Expression {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.opeartor = operator;
        this.right = right;
    }
}
class SetExpression extends Expression {
    constructor(object, name, expression) {
        super();
        this.object = object;
        this.name = name;
        this.expression = expression;
    }
}
class SuperExpression extends Expression {
    constructor(name, method) {
        super();
        this.name = name;
        this.method = method;
    }
}
class ThisExpression extends Expression {
    constructor(keyword) {
        super();
        this.keyword = keyword;
    }
}
class UnaryExpression extends Expression {
    constructor(operator, right) {
        super();
        this.opeartor = operator;
        this.right = right;
    }
}
class VarExpression extends Expression {
    constructor(name) {
        super();
        this.name = name;
    }
}

// program = (declaration)*
// declaration = classDeclaration | function | varDeclaration | statement
// classDeclaration = IDENTIFIER-name (LESS IDENTIFIER-super)? LEFT_BRACE (function-method)* RIGHT_BRACE
// statement = forStatement | ifStatement | printStatement | returnStatement | whileStatement | block | expressionStatement
// forStatement = LEFT_PAREN (varDeclaration | expressionStatement | SEMICOLON) expression? SEMICOLON expression? RIGHT_PAREN statement
// ifStatement = LEFT_PAREN expression RIGHT_PAREN statement (ELSE statement)?
// expression = assignment
// printStatement = expression SEMICOLON
// returnStatement = expression? SEMICOLON
// varDeclaration = IDENTIFIER (EQUAL expression)? SEMICOLON
// whileDeclaration = LEFT_PAREN expression RIGHT_PAREN statement
// expressionStatement = expression SEMICOLON
// function = IDENTIFIER-name LEFT_PAREN (IDENTIFIER-param COMMA)* RIGHT_PAREN block
// block = (declaration)* RIGHT_BRACE
// assignment = or (EQUAL or)?
// or = and (OR and)*
// and = equality (AND equality)*
// equality = comparison ((BANG_EQUAL | EQUAL_EQUAL) comparison)*
// comparison = addition ((GREATER | GREATER_EQUAL | LESS | LESS_EQUAL) addition)*
// addition = multiplication ((MINUS | PLUS) multiplication)*
// multiplication = unary ((SLASH | STAR) unary)*
// unary = ((BANG | MINUS) unary) | call
// call = primary ((LEFT_PAREN finish_call) | (DOT IDENTIFIER))*
// finish_call = (expression COMMA)* RIGHT_PAREN
// primary = TRUE | FALSE | NIL | NUMBER | STRING | SUPER | THIS | IDENTIFIER | LEFT_PAREN expression RIGHT_PAREN
class Parser {
    constructor(tokens) {
        this.tokens = tokens;
    }
    parse() {
        this.current = 0;
        return this.program();
    }
    program() {
        const statements = [];
        while (this.isNotEnd()) {
            statements.push(this.declaration());
        }
        return statements;
    }
    declaration() {
        if (this.match(TokenType.CLASS)) {
            return this.classDeclaration();
        }
        else if (this.match(TokenType.FUN)) {
            return this.function("function");
        }
        else if (this.match(TokenType.VAR)) {
            return this.varDeclaration();
        }
        else {
            return this.statement();
        }
    }
    classDeclaration() {
        const name = this.consume(TokenType.IDENTIFIER, "Expect class name");
        let superclass;
        let body = [];
        if (this.match(TokenType.LESS)) {
            superclass = new VarExpression(this.consume(TokenType.IDENTIFIER, "Expect superclass name"));
        }
        this.consume(TokenType.LEFT_BRACE, "Expect { before class body");
        while (!this.match(TokenType.RIGHT_BRACE) && this.isNotEnd()) {
            body.push(this.function("method"));
        }
        return new ClassStatement(name, body, superclass);
    }
    function(kind) {
        const name = this.consume(TokenType.IDENTIFIER, `Expect ${kind} name`);
        const parameters = [];
        this.consume(TokenType.LEFT_PAREN, `Expect ( after ${kind} name`);
        while (!this.match(TokenType.RIGHT_PAREN) && this.isNotEnd()) {
            parameters.push(this.consume(TokenType.IDENTIFIER, "Expect parameter name"));
        }
        this.consume(TokenType.LEFT_BRACE, `Expect { before ${kind} body`);
        const block = this.block();
        return new FunctionStatement(name, parameters, block);
    }
    block() {
        const statements = [];
        while (!this.match(TokenType.RIGHT_BRACE) && this.isNotEnd()) {
            statements.push(this.declaration());
        }
        return statements;
    }
    varDeclaration() {
        const name = this.consume(TokenType.IDENTIFIER, "Expect variable name");
        let initializer;
        if (this.match(TokenType.EQUAL)) {
            initializer = this.expression();
        }
        this.consume(TokenType.SEMICOLON, "Expect ; after variable declaration");
        return new VarStatement(name, initializer);
    }
    expression() {
        return this.assignment();
    }
    statement() {
        if (this.match(TokenType.FOR)) {
            return this.forStatement();
        }
        else if (this.match(TokenType.IF)) {
            return this.ifStatement();
        }
        else if (this.match(TokenType.RETURN)) {
            return this.returnStatement();
        }
        else if (this.match(TokenType.WHILE)) {
            return this.whileStatement();
        }
        else if (this.match(TokenType.LEFT_BRACE)) {
            return new BlockStatement(this.block());
        }
        else if (this.match(TokenType.PRINT)) {
            return this.printStatement();
        }
        else {
            return this.expressionStatement();
        }
    }
    forStatement() {
        this.consume(TokenType.LEFT_PAREN, "Expect ( after for");
        let initializer;
        let condition;
        let increment;
        if (this.match(TokenType.VAR)) {
            initializer = this.varDeclaration();
        }
        else if (this.match(TokenType.SEMICOLON)) {
            initializer = undefined;
        }
        else {
            initializer = this.expressionStatement();
        }
        if (!this.match(TokenType.SEMICOLON)) {
            condition = this.expression();
            this.consume(TokenType.SEMICOLON, "Expect ; after loop conidtion");
        }
        if (!this.match(TokenType.SEMICOLON)) {
            increment = this.expression();
        }
        this.consume(TokenType.RIGHT_PAREN, "Expect ) after for clauses");
        let block = this.statement();
        if (initializer) {
            block.shift(initializer);
        }
        if (increment) {
            block.push(increment);
        }
        return new WhileStatement(block, condition);
    }
    ifStatement() {
        this.consume(TokenType.LEFT_PAREN, "Expect ( after if");
        const condition = this.expression();
        this.consume(TokenType.RIGHT_PAREN, "Expect ) after if condition");
        const thenBranch = this.statement();
        let elseBranch;
        if (this.match(TokenType.ELSE)) {
            elseBranch = this.statement();
        }
        new IfStatement(condition, thenBranch, elseBranch);
    }
    whileStatement() {
        this.consume(TokenType.LEFT_PAREN, "Expect ( after while");
        const condition = this.expression();
        this.consume(TokenType.RIGHT_PAREN, "Expect ) fater while condition");
        const statement = this.statement();
        return new WhileStatement(statement, condition);
    }
    returnStatement() {
        let expression;
        if (!this.match(TokenType.SEMICOLON)) {
            expression = this.expression();
        }
        this.consume(TokenType.SEMICOLON, "Expect ; after return value");
        return new ReturnStatement(expression);
    }
    expressionStatement() {
        const expression = this.expression();
        this.consume(TokenType.SEMICOLON, "Expect ; after expression");
        return new ExpressionStatement(expression);
    }
    printStatement() {
        const expression = this.expression();
        this.consume(TokenType.SEMICOLON, "Expect ; after print value");
        return new PrintStatement(expression);
    }
    assignment() {
        const or = this.or();
        if (this.match(TokenType.EQUAL)) {
            const assignment = this.assignment();
            if (or instanceof VarExpression) {
                return new AssignExpression(or.name, assignment);
            }
            else if (or instanceof GetExpression) {
                return new SetExpression(or.object, or.name, assignment);
            }
        }
        return or;
    }
    or() {
        let and = this.and();
        while (this.match(TokenType.OR)) {
            and = new LogicalExpression(and, this.previous(), this.or());
        }
        return and;
    }
    and() {
        let equality = this.equality();
        while (this.match(TokenType.AND)) {
            equality = new LogicalExpression(equality, this.previous(), this.and());
        }
        return equality;
    }
    equality() {
        let comparison = this.comparison();
        while (this.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
            comparison = new LogicalExpression(comparison, this.previous(), this.comparison());
        }
        return comparison;
    }
    comparison() {
        let addition = this.addition();
        while (this.match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
            addition = new LogicalExpression(addition, this.previous(), this.addition());
        }
        return addition;
    }
    addition() {
        let multiplication = this.multiplication();
        while (this.match(TokenType.MINUS, TokenType.PLUS)) {
            multiplication = new BinaryExpression(multiplication, this.previous(), this.multiplication());
        }
        return multiplication;
    }
    multiplication() {
        let unary = this.unary();
        while (this.match(TokenType.SLASH, TokenType.STAR)) {
            unary = new BinaryExpression(unary, this.previous(), this.unary());
        }
        return unary;
    }
    unary() {
        if (this.match(TokenType.BANG, TokenType.MINUS)) {
            return new UnaryExpression(this.previous(), this.unary());
        }
        return this.call();
    }
    call() {
        let primary = this.primary();
        while (true) {
            if (this.match(TokenType.LEFT_PAREN)) {
                primary = this.finishCall(primary);
            }
            else if (this.match(TokenType.DOT)) {
                const name = this.consume(TokenType.IDENTIFIER, "Expect property name after .");
                primary = new GetExpression(primary, name);
            }
            else {
                break;
            }
        }
        return primary;
    }
    finishCall(token) {
        const args = [];
        if (!this.match(TokenType.RIGHT_PAREN)) {
            do {
                args.push(this.expression());
            } while (this.match(TokenType.COMMA));
        }
        return new CallExpression(token, args);
    }
    primary() {
        if (this.match(TokenType.FALSE))
            return new LiteralExpression(false);
        if (this.match(TokenType.TRUE))
            return new LiteralExpression(true);
        if (this.match(TokenType.NIL))
            return new LiteralExpression(undefined);
        if (this.match(TokenType.NUMBER, TokenType.STRING)) {
            return new LiteralExpression(this.previous().liternal);
        }
        if (this.match(TokenType.SUPER)) {
            let keyword = this.previous();
            this.consume(TokenType.DOT, "Expect . after super");
            let method = this.consume(TokenType.IDENTIFIER, "Expect variable after super.");
            return new SuperExpression(keyword, method);
        }
        if (this.match(TokenType.THIS))
            return new ThisExpression(this.previous());
        if (this.match(TokenType.IDENTIFIER)) {
            return new VarExpression(this.previous());
        }
        if (this.match(TokenType.LEFT_PAREN)) {
            let expr = this.expression();
            this.consume(TokenType.RIGHT_PAREN, "Expect ) after expression");
            return new GroupingExpression(expr);
        }
        return this.error(this.peek(), "Expect expression");
    }
    match(...types) {
        return types.some(type => {
            if (this.check(type)) {
                this.advance();
                return true;
            }
            return false;
        });
    }
    consume(type, errorMessage) {
        if (this.check(type)) {
            return this.advance();
        }
        return this.error(this.peek(), errorMessage);
    }
    check(type) {
        if (this.isAtEnd())
            return false;
        return this.peek().type === type;
    }
    peek() {
        return this.tokens[this.current];
    }
    advance() {
        if (this.isNotEnd())
            this.current++;
        return this.previous();
    }
    previous() {
        return this.tokens[this.current - 1];
    }
    isAtEnd() {
        return !this.isNotEnd();
    }
    isNotEnd() {
        return (this.current < this.tokens.length && this.peek().type !== TokenType.EOF);
    }
    error(token, errorMessage) {
        throw new Error(`${token.toString()} ${errorMessage}.`);
    }
}

exports.Lexer = Lexer;
exports.Parser = Parser;
