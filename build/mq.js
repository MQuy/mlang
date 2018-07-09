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
class ReturnError {
    constructor(value) {
        this.value = value;
    }
}

let incrementValue = 0;
class Statement extends AstNode {
    constructor() {
        super();
        this.hash = incrementValue++;
    }
    accept(vistor) { }
}
class BlockStatement extends Statement {
    constructor(statements) {
        super();
        this.statements = statements;
    }
    accept(vistor) {
        return vistor.visitBlockStatement(this);
    }
}
class FunctionStatement extends Statement {
    constructor(name, parameters, methods) {
        super();
        this.name = name;
        this.parameters = parameters;
        this.methods = methods;
    }
    accept(vistor) {
        return vistor.visitFunctionStatement(this);
    }
}
class ClassStatement extends Statement {
    constructor(name, methods, superclass) {
        super();
        this.name = name;
        this.methods = methods;
        this.superclass = superclass;
    }
    accept(vistor) {
        return vistor.visitClassStatement(this);
    }
}
class ExpressionStatement extends Statement {
    constructor(expression) {
        super();
        this.expression = expression;
    }
    accept(vistor) {
        return vistor.visitExpressionStatement(this);
    }
}
class IfStatement extends Statement {
    constructor(condition, thenBranch, elseBranch) {
        super();
        this.condition = condition;
        this.thenBranch = thenBranch;
        this.elseBranch = elseBranch;
    }
    accept(vistor) {
        return vistor.visitIfStatement(this);
    }
}
class PrintStatement extends Statement {
    constructor(expression) {
        super();
        this.expression = expression;
    }
    accept(vistor) {
        return vistor.visitPrintStatement(this);
    }
}
class ReturnStatement extends Statement {
    constructor(value) {
        super();
        this.value = value;
    }
    accept(vistor) {
        return vistor.visitReturnStatement(this);
    }
}
class VarStatement extends Statement {
    constructor(name, initializer) {
        super();
        this.name = name;
        this.initializer = initializer;
    }
    accept(vistor) {
        return vistor.visitVarStatement(this);
    }
}
class WhileStatement extends Statement {
    constructor(body, condition) {
        super();
        this.body = body;
        this.condition = condition;
    }
    accept(vistor) {
        return vistor.visitWhileStatement(this);
    }
}

let incrementValue$1 = 0;
class Expression extends AstNode {
    constructor() {
        super();
        this.hash = incrementValue$1++;
    }
    accept(vistor) { }
}
class AssignExpression extends Expression {
    constructor(name, expression) {
        super();
        this.name = name;
        this.expression = expression;
    }
    accept(vistor) {
        return vistor.visitAssignExpression(this);
    }
}
class BinaryExpression extends Expression {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.operator = operator;
        this.right = right;
    }
    accept(vistor) {
        return vistor.visitBinaryExpression(this);
    }
}
class CallExpression extends Expression {
    constructor(callee, args) {
        super();
        this.callee = callee;
        this.arguments = args;
    }
    accept(vistor) {
        return vistor.visitCallExpression(this);
    }
}
class GetExpression extends Expression {
    constructor(object, name) {
        super();
        this.object = object;
        this.name = name;
    }
    accept(vistor) {
        return vistor.visitGetExpression(this);
    }
}
class GroupingExpression extends Expression {
    constructor(expression) {
        super();
        this.expression = expression;
    }
    accept(vistor) {
        return vistor.visitGroupingExpression(this);
    }
}
class LiteralExpression extends Expression {
    constructor(value) {
        super();
        this.value = value;
    }
    accept(vistor) {
        return vistor.visitLiternalExpression(this);
    }
}
class LogicalExpression extends Expression {
    constructor(left, operator, right) {
        super();
        this.left = left;
        this.opeartor = operator;
        this.right = right;
    }
    accept(vistor) {
        return vistor.visitLogicalExpression(this);
    }
}
class SetExpression extends Expression {
    constructor(object, name, expression) {
        super();
        this.object = object;
        this.name = name;
        this.expression = expression;
    }
    accept(vistor) {
        return vistor.visitSetExpression(this);
    }
}
class SuperExpression extends Expression {
    constructor(name, method) {
        super();
        this.name = name;
        this.method = method;
    }
    accept(vistor) {
        return vistor.visitSuperExpression(this);
    }
}
class ThisExpression extends Expression {
    constructor(keyword) {
        super();
        this.keyword = keyword;
    }
    accept(vistor) {
        return vistor.visitThisExpression(this);
    }
}
class UnaryExpression extends Expression {
    constructor(operator, right) {
        super();
        this.opeartor = operator;
        this.right = right;
    }
    accept(vistor) {
        return vistor.visitUnaryExpression(this);
    }
}
class VarExpression extends Expression {
    constructor(name) {
        super();
        this.name = name;
    }
    accept(vistor) {
        return vistor.visitVarExpression(this);
    }
}

// program = (declaration)* EOF
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

class SymbolTable {
    constructor(enclosing) {
        this.enclosing = enclosing;
        this.symbols = {};
    }
    define(token, value) {
        this.symbols[token.lexeme] = value;
    }
    assign(token, value, onlyCurrentScope = false) {
        const key = token.lexeme;
        if (Object.keys(this.symbols).includes(key)) {
            this.symbols[key] = value;
        }
        else if (this.enclosing && !onlyCurrentScope) {
            this.enclosing.assign(token, value);
        }
        else {
            throw new Error(`Undefined variable ${key}.`);
        }
    }
    ancestor(distance) {
        let scope = this;
        for (let i = 0; i < distance; ++i) {
            if (scope.enclosing) {
                scope = scope.enclosing;
            }
        }
        return scope;
    }
    getAt(distance, token) {
        return this.ancestor(distance).lookup(token, true);
    }
    assignAt(distance, token, value) {
        this.ancestor(distance).assign(token, value, true);
    }
    lookup(token, onlyCurrentScope = false) {
        const key = token.lexeme;
        if (Object.keys(this.symbols).includes(key)) {
            return this.symbols[key];
        }
        else if (this.enclosing && !onlyCurrentScope) {
            return this.enclosing.lookup(token);
        }
        else {
            throw new Error(`Undefined variable ${key}.`);
        }
    }
}

class Functionable {
    constructor(name, declaration, closure) {
        this.name = name;
        this.declaration = declaration;
        this.closure = closure;
    }
    invoke(interpreter, args) {
        const closure = new SymbolTable(this.closure);
        this.declaration.parameters.forEach((parameter, index) => closure.define(parameter, args[index]));
        try {
            interpreter.executeBlock(this.declaration.methods, closure);
        }
        catch (e) {
            return e.value;
        }
    }
    bind(instance) {
        const closure = new SymbolTable(this.closure);
        closure.define(new Token(TokenType.THIS, "this", undefined, 0), instance);
        return new Functionable(this.name, this.declaration, closure);
    }
}

class Instance {
    constructor(klass) {
        this.klass = klass;
        this.fields = {};
    }
    get(token) {
        if (Object.keys(this.fields).includes(token.lexeme)) {
            return this.fields[token.lexeme];
        }
        const method = this.klass.lookupMethod(this, token);
        if (method)
            return method;
        throw new Error(`Undefined property ${token.lexeme} in ${this.klass.name}`);
    }
    set(token, value) {
        this.fields[token.lexeme] = value;
    }
}

class Classable {
    constructor(name, methods) {
        this.name = name;
        this.methods = methods;
    }
    invoke(interpreter, args) {
        return new Instance(this);
    }
    lookupMethod(instance, token) {
        if (this.methods[token.lexeme]) {
            return this.methods[token.lexeme].bind(instance);
        }
    }
}

class Interpreter {
    constructor(statements) {
        this.statements = statements;
        this.symbolTable = new SymbolTable();
        this.locals = {};
    }
    interpret() {
        this.statements.forEach(statement => this.execute(statement));
    }
    resolve(expression, depth) {
        this.locals[expression.hash] = depth;
    }
    visitLiternalExpression(liternal) {
        return liternal.value;
    }
    visitGroupingExpression(group) {
        return this.evaluate(group.expression);
    }
    visitUnaryExpression(unary) {
        const right = this.evaluate(unary.right);
        switch (unary.opeartor.type) {
            case TokenType.MINUS:
                return -right;
            case TokenType.BANG:
                return !right;
            default:
                return right;
        }
    }
    visitBinaryExpression(binary) {
        const left = this.evaluate(binary.left);
        const right = this.evaluate(binary.right);
        switch (binary.operator.type) {
            case TokenType.MINUS:
                return left + right;
            case TokenType.PLUS:
                return left + right;
            case TokenType.STAR:
                return left * right;
            case TokenType.SLASH:
                return left / right;
            case TokenType.EQUAL_EQUAL:
                return left === right;
            case TokenType.BANG_EQUAL:
                return left !== right;
            case TokenType.GREATER:
                return left > right;
            case TokenType.GREATER_EQUAL:
                return left >= right;
            case TokenType.LESS:
                return left < right;
            case TokenType.LESS_EQUAL:
                return left <= right;
        }
    }
    visitLogicalExpression(logical) {
        const left = this.evaluate(logical.left);
        const right = this.evaluate(logical.right);
        switch (logical.opeartor.type) {
            case TokenType.AND:
                return left && right;
            case TokenType.OR:
                return left || right;
        }
    }
    visitVarExpression(varExpression) {
        return this.lookupVariable(varExpression.name, varExpression);
    }
    visitAssignExpression(assign) {
        const value = this.evaluate(assign.expression);
        this.symbolTable.assignAt(assign.hash, assign.name, value);
        return value;
    }
    visitExpressionStatement(expressionStatement) {
        return this.evaluate(expressionStatement.expression);
    }
    visitVarStatement(varStatement) {
        let value;
        if (varStatement.initializer) {
            value = this.evaluate(varStatement.initializer);
        }
        this.symbolTable.define(varStatement.name, value);
    }
    visitBlockStatement(blockStatement) {
        this.executeBlock(blockStatement.statements, new SymbolTable(this.symbolTable));
    }
    visitPrintStatement(printStatement) {
        const value = this.evaluate(printStatement.expression);
        console.log(value);
    }
    visitIfStatement(ifStatement) {
        if (this.evaluate(ifStatement.condition)) {
            this.execute(ifStatement.thenBranch);
        }
        else if (ifStatement.elseBranch) {
            this.execute(ifStatement.elseBranch);
        }
    }
    visitWhileStatement(whileStatement) {
        while (whileStatement.condition &&
            this.evaluate(whileStatement.condition)) {
            this.execute(whileStatement.body);
        }
    }
    visitCallExpression(callExpression) {
        debugger;
        const callee = this.evaluate(callExpression.callee);
        const args = callExpression.arguments.map(argument => this.evaluate(argument));
        return callee.invoke(this, args);
    }
    visitFunctionStatement(functionStatement) {
        const func = new Functionable(functionStatement.name.lexeme, functionStatement, this.symbolTable);
        this.symbolTable.define(functionStatement.name, func);
    }
    visitReturnStatement(returnStatement) {
        let value;
        if (returnStatement.value) {
            value = this.evaluate(returnStatement.value);
        }
        throw new ReturnError(value);
    }
    visitClassStatement(classStatement) {
        this.symbolTable.define(classStatement.name, undefined);
        const methods = {};
        classStatement.methods.forEach(method => {
            methods[method.name.lexeme] = new Functionable(method.name.lexeme, method, this.symbolTable);
        });
        const klass = new Classable(classStatement.name.lexeme, methods);
        this.symbolTable.assign(classStatement.name, klass);
    }
    visitGetExpression(getExpression) {
        const object = this.evaluate(getExpression.object);
        if (object instanceof Instance) {
            return object.get(getExpression.name);
        }
        throw new Error(`${getExpression.name.lexeme} only instances have properties.`);
    }
    visitSetExpression(setExpression) {
        const object = this.evaluate(setExpression.object);
        if (object instanceof Instance) {
            const value = this.evaluate(setExpression.expression);
            return object.set(setExpression.name, value);
        }
        throw new Error(`Only instances have fields.`);
    }
    evaluate(expression) {
        return expression.accept(this);
    }
    execute(statement) {
        statement.accept(this);
    }
    executeBlock(statements, symbolTable) {
        const currentScope = this.symbolTable;
        try {
            this.symbolTable = symbolTable;
            statements.forEach(statement => this.execute(statement));
        }
        finally {
            this.symbolTable = currentScope;
        }
    }
    lookupVariable(token, expression) {
        const distance = this.locals[expression.hash];
        if (distance != null) {
            return this.symbolTable.getAt(distance, token);
        }
        else {
            throw new Error(`Cannot find ${token.toString()}`);
        }
    }
    visitThisExpression(thisExpression) {
        return this.lookupVariable(thisExpression.keyword, thisExpression);
    }
    visitSuperExpression(superExpression) { }
}
/*
tokens = new Lexer(`
class Cake {
  taste() {
    var adjective = "delicious";
    print "The " + this.flavor + " cake is " + adjective + "!";
  }
}

var cake = Cake();
cake.flavor = "German chocolate";
cake.taste(); // Prints "The German chocolate cake is delicious!".
`).scan();
ast = new Parser(tokens).parse();
interpreter = new Interpreter(ast);
resolver = new Resolver(interpreter);
resolver.resolve();
interpreter.interpret();
*/

class Resolver {
    constructor(interpreter) {
        this.interpreter = interpreter;
    }
    resolve() {
        this.scopes = [];
        this.beginScope();
        this.resolveStatements(this.interpreter.statements);
        this.endScope();
    }
    resolveStatements(statements) {
        statements.forEach(statement => this.resolveStatement(statement));
    }
    visitBlockStatement(stms) {
        this.beginScope();
        this.resolveStatements(stms.statements);
        this.endScope();
    }
    visitExpressionStatement(stms) {
        this.resolveExpression(stms.expression);
    }
    visitFunctionStatement(stms) {
        this.declare(stms.name);
        this.define(stms.name);
        this.resolveFunction(stms);
    }
    visitIfStatement(stms) {
        this.resolveExpression(stms.condition);
        this.resolveStatement(stms.thenBranch);
        if (stms.elseBranch)
            this.resolveStatement(stms.elseBranch);
    }
    visitPrintStatement(stms) {
        this.resolveExpression(stms.expression);
    }
    visitReturnStatement(stms) {
        if (stms.value)
            this.resolveExpression(stms.value);
    }
    visitVarStatement(smts) {
        this.declare(smts.name);
        if (smts.initializer) {
            this.resolveExpression(smts.initializer);
        }
        this.define(smts.name);
    }
    visitWhileStatement(stms) {
        if (stms.condition)
            this.resolveExpression(stms.condition);
        this.resolveStatement(stms.body);
    }
    visitAssignExpression(expr) {
        this.resolveExpression(expr.expression);
        this.resolveLocal(expr, expr.name);
    }
    visitBinaryExpression(expr) {
        this.resolveExpression(expr.left);
        this.resolveExpression(expr.right);
    }
    visitCallExpression(expr) {
        this.resolveExpression(expr.callee);
        expr.arguments.forEach(arg => this.resolveExpression(arg));
    }
    visitGetExpression(expr) {
        this.resolveExpression(expr.object);
    }
    visitGroupingExpression(expr) {
        this.resolveExpression(expr.expression);
    }
    visitLogicalExpression(expr) {
        this.resolveExpression(expr.left);
        this.resolveExpression(expr.right);
    }
    visitSetExpression(expr) {
        this.resolveExpression(expr.expression);
        this.resolveExpression(expr.object);
    }
    visitUnaryExpression(expr) {
        this.resolveExpression(expr.right);
    }
    visitVarExpression(expr) {
        this.resolveLocal(expr, expr.name);
    }
    resolveFunction(stms) {
        this.beginScope();
        stms.parameters.forEach(param => {
            this.declare(param);
            this.define(param);
        });
        this.resolveStatements(stms.methods);
        this.endScope();
    }
    declare(token) {
        const scope = this.scopes[this.scopes.length - 1];
        if (scope) {
            if (scope[token.lexeme]) {
                throw new Error(`Variable ${token.lexeme} is already declared in this scope.`);
            }
            else {
                scope[token.lexeme] = false;
            }
        }
    }
    define(token) {
        const scope = this.scopes[this.scopes.length - 1];
        if (scope) {
            scope[token.lexeme] = true;
        }
    }
    beginScope() {
        this.scopes.push({});
    }
    endScope() {
        this.scopes.pop();
    }
    resolveStatement(statement) {
        statement.accept(this);
    }
    resolveExpression(expression) {
        expression.accept(this);
    }
    resolveLocal(expression, token) {
        for (let i = this.scopes.length - 1; i >= 0; i--) {
            if (this.scopes[i][token.lexeme]) {
                this.interpreter.resolve(expression, this.scopes.length - 1 - i);
                return;
            }
        }
    }
    visitClassStatement(stms) {
        this.declare(stms.name);
        this.define(stms.name);
        this.beginScope();
        this.scopes[this.scopes.length - 1]["this"] = true;
        stms.methods.forEach(method => this.resolveFunction(method));
        this.endScope();
    }
    visitThisExpression(expr) {
        this.resolveLocal(expr, expr.keyword);
    }
    visitLiternalExpression(expr) { }
    visitSuperExpression(expr) { }
}

exports.Lexer = Lexer;
exports.Parser = Parser;
exports.Interpreter = Interpreter;
exports.Resolver = Resolver;
