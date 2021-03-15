#include "parser.h"

#include "ast.h"
#include "utils.h"

// NOTE: MQ 2021-03-14 top level only supports declaration or definition
std::shared_ptr<Program> Parser::parse()
{
	auto token = tokens->at(current);

	while (current < tokens_length)
	{
		auto [declaration, type] = parse_function_definition();
		if (!declaration)
			declaration = parse_declaration(type);
		program->add_declaration_stmt(declaration);
	}
}

/*
1. parse type and mark current
2. parse declarator
3. if next token == '=' | ',' | ';' -> go 8
4. next token != '{' -> raise exception
5. parse component statement
6. return function def
7. runner = current and return (null, type)
*/
std::pair<std::shared_ptr<DclAST>, std::shared_ptr<TypeAST>> Parser::parse_function_definition()
{
	std::shared_ptr<TypeAST> type = parse_declaration_specifiers();
	long anchor = current;

	auto [declarator_name, declarator_type] = parse_declarator(type);

	std::shared_ptr<Token> token = tokens->at(runner);
	assert(token);

	if (token->type == TokenType::tk_symbol)
	{
		std::shared_ptr<TokenSymbol> token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		if (token_symbol->name == TokenName::tk_equal
			|| token_symbol->name == TokenName::tk_comma
			|| token_symbol->name == TokenName::tk_semicolon)
		{
			runner = current;
			return std::make_pair(nullptr, type);
		}
		else if (token_symbol->name != TokenName::tk_left_brace)
			throw ParserError("{ is expected after function declarator");
	}

	auto func_type = std::dynamic_pointer_cast<FunctionTypeAST>(declarator_type);
	std::shared_ptr<CompoundStmtAST> body = parse_compound_stmt();
	std::shared_ptr<DclAST> def = std::make_shared<DclAST>(FunctionDefStmtAST(func_type, declarator_name, body));
	return std::make_pair(def, type);
}

void Parser::parse_storage_or_qualifier(StorageSpecifier &storage_specifier,
										std::set<TypeQualifier> &type_qualifiers)
{
	for (std::shared_ptr<Token> token = tokens->at(runner);
		 runner < tokens_length
		 && token && token->type == TokenType::tk_symbol;
		 runner++)
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		assert(token_symbol);

		switch (token_symbol->name)
		{
		case TokenName::tk_auto:
			storage_specifier = StorageSpecifier::auto_;
			break;
		case TokenName::tk_register:
			storage_specifier = StorageSpecifier::register_;
			break;
		case TokenName::tk_static:
			storage_specifier = StorageSpecifier::static_;
			break;
		case TokenName::tk_extern:
			storage_specifier = StorageSpecifier::extern_;
			break;

		case TokenName::tk_const:
			type_qualifiers.insert(TypeQualifier::const_);
			break;
		case TokenName::tk_volatile:
			type_qualifiers.insert(TypeQualifier::volatile_);
			break;
		case TokenName::tk_restrict:
			type_qualifiers.insert(TypeQualifier::restrict);
			break;

		default:
			return;
		}
	}
}

std::shared_ptr<TypeAST> Parser::parse_declaration_specifiers()
{
	StorageSpecifier storage_specifier;
	std::set<TypeQualifier> type_qualifiers;

	auto make_declaration_ast = [&storage_specifier, &type_qualifiers, this](BuiltinTypeName name, unsigned size) {
		parse_storage_or_qualifier(storage_specifier, type_qualifiers);
		return std::make_shared<TypeAST>(BuiltinTypeAST(std::make_shared<BuiltinTypeName>(name), size, size, type_qualifiers));
	};

	parse_storage_or_qualifier(storage_specifier, type_qualifiers);

	std ::shared_ptr<Token> token = advance();
	assert(token);

	if (token->type == TokenType::tk_symbol)
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		assert(token_symbol);

		switch (token_symbol->name)
		{
		case TokenName::tk_bool:
			return make_declaration_ast(BuiltinTypeName::_Bool, 1);
		case TokenName::tk_void:
			return make_declaration_ast(BuiltinTypeName::char_, 4);
		case TokenName::tk_char:
			return make_declaration_ast(BuiltinTypeName::char_, 1);
		case TokenName::tk_short:
			return make_declaration_ast(BuiltinTypeName::short_, 2);
		case TokenName::tk_int:
			return make_declaration_ast(BuiltinTypeName::int_, 4);
		case TokenName::tk_float:
			return make_declaration_ast(BuiltinTypeName::float_, 4);
		case TokenName::tk_double:
			return make_declaration_ast(BuiltinTypeName::double_, 8);
		case TokenName::tk_long:
		{
			if (match(TokenName::tk_double))
				return make_declaration_ast(BuiltinTypeName::long_double, 16);
			else if (match(TokenName::tk_long))
				return make_declaration_ast(BuiltinTypeName::long_long, 8);
			else
				return make_declaration_ast(BuiltinTypeName::long_, 8);
		}
		case TokenName::tk_signed:
		{
			if (match(TokenName::tk_char))
				return make_declaration_ast(BuiltinTypeName::signed_char, 1);
			else if (match(TokenName::tk_short))
				return make_declaration_ast(BuiltinTypeName::short_, 2);
			else if (match(TokenName::tk_int))
				return make_declaration_ast(BuiltinTypeName::int_, 4);
			else if (match(TokenName::tk_long))
			{
				if (match(TokenName::tk_long))
					return make_declaration_ast(BuiltinTypeName::long_long, 8);
				return make_declaration_ast(BuiltinTypeName::long_, 8);
			}
		}
		case TokenName::tk_unsigned:
		{
			if (match(TokenName::tk_char))
				return make_declaration_ast(BuiltinTypeName::unsigned_char, 1);
			else if (match(TokenName::tk_short))
				return make_declaration_ast(BuiltinTypeName::unsigned_short, 2);
			else if (match(TokenName::tk_int))
				return make_declaration_ast(BuiltinTypeName::unsigned_int, 4);
			else if (match(TokenName::tk_long))
			{
				if (match(TokenName::tk_long))
					return make_declaration_ast(BuiltinTypeName::unsigned_long_long, 8);
				return make_declaration_ast(BuiltinTypeName::unsigned_long, 8);
			}
		}

		case TokenName::tk_struct:
		case TokenName::tk_union:
			break;

		case TokenName::tk_enum:
			break;

		default:
			assert_not_reached();
			break;
		}
	}
}

std::shared_ptr<ArrayTypeAST> Parser::parse_array_type(std::shared_ptr<TypeAST> type)
{
	if (!match(TokenName::tk_left_paren))
		return nullptr;

	std::shared_ptr<ExprAST> expr = parse_expr();
	auto array_type = std::make_shared<ArrayTypeAST>(ArrayTypeAST(type, expr));
	match(TokenName::tk_right_paren, true);
	return array_type;
}

std::shared_ptr<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>> Parser::parser_parameters()
{
	if (!match(TokenName::tk_left_bracket))
		return nullptr;

	std::shared_ptr<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>> parameters;
	while (match(TokenName::tk_right_bracket))
	{
		std::shared_ptr<TypeAST> type = parse_declaration_specifiers();
		auto [identifier, inner_type] = parse_declarator(type);
		auto parameter = std::make_pair(identifier, inner_type);
		parameters->push_back(parameter);
	}
	return parameters;
}

std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>> Parser::parse_declarator(std::shared_ptr<TypeAST> type)
{
	if (match(TokenName::tk_asterisk))
	{
		std::set<TypeQualifier> qualifiers;
		parse_qualifier(qualifiers);
		std::shared_ptr<TypeAST> pointer = std::make_shared<TypeAST>(PointerTypeAST(type, qualifiers));
		return parse_declarator(pointer);
	}

	std::shared_ptr<TokenIdentifier> identifier;
	std::shared_ptr<TypeAST> inner_type;
	if (match(TokenName::tk_left_paren))
	{
		auto declarator = parse_declarator(type);
		identifier = declarator.first;
		inner_type = declarator.second;
		match(TokenName::tk_right_paren, true);
	}
	else
	{
		std::shared_ptr<Token> token = advance();
		identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);
		assert(identifier);
	}

	if (auto parameters_type = parser_parameters())
	{
		std::shared_ptr<FunctionTypeAST> func_type = std::make_shared<FunctionTypeAST>(FunctionTypeAST(*parameters_type, type));
		return std::make_pair(identifier, inner_type ? inner_type->redirect(func_type) : func_type);
	}
	else if (auto array_type = parse_array_type(type))
		return std::make_pair(identifier, inner_type ? inner_type->redirect(array_type) : array_type);
	else
		return std::make_pair(identifier, inner_type ? inner_type->redirect(type) : type);
}

bool Parser::match(TokenName name, bool strict = false)
{
	std::shared_ptr<TokenSymbol> token = std::dynamic_pointer_cast<TokenSymbol>(tokens->at(runner));
	assert(token);

	if (token->name == name)
	{
		runner++;
		return true;
	}
	else if (strict)
		throw ParserError("Expect token symbol");

	return false;
}

bool Parser::match(std::string name, bool strict = false)
{
	std::shared_ptr<TokenIdentifier> token = std::dynamic_pointer_cast<TokenIdentifier>(tokens->at(runner));
	assert(token);

	if (token->name == name)
	{
		runner++;
		return true;
	}
	else if (strict)
		throw ParserError("Expect token symbol");

	return false;
}

std::shared_ptr<Token> Parser::advance()
{
	return tokens->at(runner++);
}
