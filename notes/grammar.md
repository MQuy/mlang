```js
statement = controlStatement
            | iterationStatement
            | declarationStatement
            | assignmentStatement
            | emptyStatement

controlStatement = ifStatement
                    | blockStatement
                    | breakStatement
                    | continueStatement
ifStatement = IF
                LEFT_PAREN expression RIGHT_PAREN
                  statement
                (ELSE statement)?
blockStatement = LEFT_BRACE statements RIGHT_BRACE
breakStatement = BREAK COMMA
continueStatement = CONTINUE COMMA

iterationStatement = whileStatement | forStatement
forStatement = FOR
                LEFT_PAREN
                  (
                    varStatement (COMMA varStatement)*
                    | assignmentExpression (COMMA assignmentExpression)*
                  )? SEMICOLON
                  expression? SEMICOLON
                  expression?
                RIGHT_PAREN
                  statement
whileStatement = WHILE LEFT_PAREN expression? RIGHT_PAREN statement

declarationStatement = varStatement
                        | classStatement
                        | functionStatement
                        | returnStatement
varStatement = VAR
                IDENTIFIER COLON IDENTIFIER
                EQUAL
                expression COMMA
classStatement = CLASS IDENTIFIER (extends IDENTIFIER)?
                  LEFT_BRACE
                    (varStatement | functionStatement)*
                  RIGHT_BRACE
functionStatement = DEF IDENTIFIER
                      LEFT_PAREN
                        (IDENTIFIER COLON IDENTIFIER (COMMA IDENTIFIER COLON IDENTIFIER)*)?
                      RIGHT_PAREN
                      COLON IDENTIFIER
                      statement
returnStatement = RETURN expression? COMMA

emptyStatement = COMMA
assignmentStatement = assigmentExpression COMMA

expression = assignmentExpression

assignmentExpression = logicalExpression (EQUAL logicalExpression)*
logicalExpression = equalityExpression ((AND | OR) equalityExpression)*
equalityExpression = compareExpression ((EQUAL_EQUAL | BANG_EQUAL) compareExpression)*
compareExpression = additionExpression ((GREAT | GREAT_THAN | LESS | LESS_THAN) additionExpression)*
additionExpression = multiplicationExpression ((PLUS | MINUS) multiplicationExpression)*
multiplicationExpression = exponentiationExpression ((STAR | SLASH) exponentiationExpression)*
exponentiationExpression = unaryExpression (** unaryExpression)*
unaryExpression = (PLUS | MINUS | PLUS_PLUS | MINUS_MINUS)? callExpression
callExpression = memberAccessExpression (LEFT_PAREN IDENTIFIER (COMMA IDENTIFIER)* RIGHT_PAREN)*
memberAccessExpression = primaryExpression
                          (
                            DOT primaryExpression
                            | LEFT_BRACKET NUMBER RIGHT_BRACKET
                          )*

primaryExpression = literalExpression
                    | lambdaExpression
                    | groupExpression
                    | newExpression
                    | arrayExpression

literalExpression = TRUE | FALSE | NUMBER | STRING | NULL | THIS | SUPER | IDENTIFIER
lambdaExpression = LEFT_PAREN
                      (IDENTIFIER COLON IDENTIFIER (COMMA IDENTIFIER COLON IDENTIFIER)*)?
                    RIGHT_PAREN
                    COLON IDENTIFIER
                    ARROW statement
groupExpression = LEFT_PAREN expression RIGHT_PAREN
newExpression = NEW IDENTIFIER
                  LEFT_PAREN
                    (IDENTIFIER (COMMA IDENTIFIER)*)?
                  RIGHT_PAREN
arrayExpression = LEFT_BRACKET (expression (COMMA expression)*)? RIGHT_BRACKET

// rules for IDENTIFER
// + class or type: Pascal case
// + variable: camel case
IDENTIFIER = ALPHA (ALPHA | DIGIT)*
NUMBER = DIGIT+ (DOT DIGIT+)?
STRING = DOUBLE_QUOTE [^DOUBLE_QUOTE]+ DOUBLE_QUOTE
ALPHA = [a-z] | [A-Z] | _
DIGIT= [0-9]
```
