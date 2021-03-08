translation_unit = { external_declaration };

external_declaration = function_definition
                      | declaration;

function_definition = [ declaration_specifiers ], declarator, [ declaration_list ], compound_statement;

declaration = declaration_specifiers, [ init_declarator_list ], ";";

declaration_list = { declaration };

declaration_specifiers = storage_class_specifier, [ declaration_specifiers ]
                        | type_specifier, [ declaration_specifiers ]
                        | type_qualifier, [ declaration_specifiers ];

storage_class_specifier = "auto" | "register" | "static" | "extern" | "typedef";

type_specifier = "void" | "char" | "short" | "int" | "long" | "float" | "double" | "signed" | "unsigned"
                  | struct_or_union_specifier
                  | enum_specifier
                  | typedef_name;

type_qualifier = "const" | "volatile" | "restrict";

struct_or_union_specifier = struct_or_union, [ identifier ], "{", struct_declaration_list, "}"
                            | struct_or_union, identifier;

struct_or_union = "struct" | "union";

struct_declaration_list = { struct_declaration };

init_declarator_list = init_declarator, { ",", initializer };

init_declarator = declarator, [ "=", initializer ];

struct_declaration = specifier_qualifier_list, struct_declarator_list, ";";

specifier_qualifier_list = type_specifier, [ specifier_qualifier_list ]
                          | type_qualifier, [ specifier_qualifier_list ];

struct_declarator_list = struct_declarator, { ",", struct_declarator };

struct_declarator = declarator
                    | [ declarator ], ":", constant_expression;

enum_specifier = "enum", [ identifier ], "{", enumerator_list, "}"
                | "enum", identifier;

enumerator_list = enumerator, { ",", enumerator };

enumerator = identifier, [ "=", constant_expression ];

declarator = [ pointer ], direct_declarator;

direct_declarator = identifier
                    | "(", declarator, ")"
                    | direct_declarator, "[", [ constant_expression ], "]"
                    | direct_declarator, "(", parameter_type_list, ")"
                    | direct_declarator, "(", [ identifier_list ], ")";

pointer = "*" [ type_qualifier_list]
          | "*", [ type_qualifier_list ], pointer;

type_qualifier_list = { type_qualifier };

parameter_type_list = parameter_list, [ ",", "..." ];

parameter_list = parameter_declaration, { ",", parameter_declaration };

parameter_declaration = declaration_specifiers, declarator
                        | declaration_specifiers, [ abstract_declarator ];

identifier_list = identifier, { ",", identifier };

initializer = assignment_expression
            | "{", initializer_list, [","], "}";

initializer_list = initializer, { ",", initializer };

type_name = specifier_qualifier_list, [ abstract_declarator ];

abstract_declarator = pointer
                    | [ pointer ], direct_abstract_declarator;

direct_abstract_declarator = "(", abstract_declarator, ")"
                            | [direct_abstract_declarator], "[", [constant_expression], "]"
                            | [direct_abstract_declarator], "(", [parameter_type_list], ")";

typedef_name = identifier;

statement = labeled_statement
          | expression_statement
          | compound_statement
          | selection_statement
          | iteration_statement
          | jump_statement;

labeled_statement = identifier, ":", statement
                    | "case", constant_expression, ":", statement 
                    | "default", ":", statement;

expression_statement = [ expression ], ";";

compound_statement = "{", [declaration_list], [statement_list], "}";

statement_list = { statement };

selection_statement = "if", "(", expression, ")", statement, [ "else", statement ]
                      | "switch", "(", expression, ")", statement;

iteration_statement = "while", "(", expression, ")", statement
                    | "do", statement, "while", "(", expression, ")", ";"
                    | "for", "(", [expression], ";", [expression], ";", [expression], ")", statement;

jump_statement = "goto", identifier, ";"
                | "continue", ";"
                | "break", ";"
                | "return", [expression], ";";

expression = assignment_expression, { ",", assignment_expression };

assignment_expression = conditional_expression
                      | unary_expression, assignment_operator, assignment_expression;

assignment_operator = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "¦=";

conditional_expression = logical_OR_expression
                        | logical_OR_expression, "?", expression, ":", conditional_expression;

constant_expression = conditional_expression;

logical_OR_expression = logical_AND_expression
                      | logical_OR_expression, "¦¦", logical_AND_expression;

logical_AND_expression = inclusive_OR_expression
                        | logical_AND_expression, "&&", inclusive_OR_expression;

inclusive_OR_expression = exclusive_OR_expression
                        | inclusive_OR_expression, "¦", exclusive_OR_expression;

exclusive_OR_expression = AND_expression
                        | exclusive_OR_expression, "^", AND_expression;

AND_expression = equality_expression
                | AND_expression, "&", equality_expression;

equality_expression = relational_expression
                    | equality_expression, "==", relational_expression
                    | equality_expression, "!=", relational_expression;

relational_expression = shift_expression
                      | relational_expression, "<", shift_expression
                      | relational_expression, ">", shift_expression
                      | relational_expression, "<=", shift_expression
                      | relational_expression, ">=", shift_expression;

shift_expression = additive_expression
                  | shift_expression, "<<", additive_expression
                  | shift_expression, ">>", additive_expression;

additive_expression = multiplicative_expression
                    | additive_expression, "+", multiplicative_expression
                    | additive_expression, "-", multiplicative_expression;

multiplicative_expression = cast_expression
                          | multiplicative_expression, "*", cast_expression
                          | multiplicative_expression, "/", cast_expression
                          | multiplicative_expression, "%", cast_expression;

cast_expression = unary_expression
                  | "(", type_name, ")", cast_expression;

unary_expression = postfix_expression
                  | "++", unary_expression
                  | "--", unary_expression
                  | unary_operator, cast_expression
                  | "sizeof", unary_expression
                  | "sizeof", "(", type_name, ")";

unary_operator = "&" | "*" | "+" | "-" | "~" | "!";

postfix_expression = primary_expression
                    | postfix_expression, "[", expression, "]"
                    | postfix_expression, "(", [argument_expression_list], ")"
                    | postfix_expression, ".", identifier
                    | postfix_expression, "->", identifier
                    | postfix_expression, "++"
                    | postfix_expression, "--";

primary_expression = identifier
                    | constant
                    | string
                    | "(", expression, ")";

argument_expression_list = assignment_expression
                          | argument_expression_list, ",", assignment_expression;

constant = integer_constant
          | character_constant
          | floating_constant
          | enumeration_constant;
