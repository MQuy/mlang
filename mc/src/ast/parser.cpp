#include "parser.h"

#include "ast.h"
#include "utils.h"

/*
top level only supports declaration or function definition
1. try parsing function definition
2. if exist -> add to list
   | otherwise -> parse declaration (there is the common specifier so we reuse it)
*/
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
3. if next token == '=' | ',' | ';' -> runner = current and return (null, type)
4. next token != '{' -> raise exception (don't support old style)
5. parse component statement
6. return function def
*/
std::pair<std::shared_ptr<ExternAST>, std::shared_ptr<TypeAST>> Parser::parse_function_definition()
{
	std::shared_ptr<TypeAST> type = parse_declaration_specifiers();
	long anchor = current;

	auto [declarator_name, declarator_type] = parse_declarator(type);

	std::shared_ptr<Token> token = tokens->at(runner);
	assert(token && token->type == TokenType::tk_symbol);

	std::shared_ptr<TokenSymbol> token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
	if (token_symbol->name == TokenName::tk_equal
		|| token_symbol->name == TokenName::tk_comma
		|| token_symbol->name == TokenName::tk_semicolon)
	{
		runner = current;
		return std::make_pair(nullptr, type);
	}
	match(TokenName::tk_left_brace, true, false);

	auto func_type = std::dynamic_pointer_cast<FunctionTypeAST>(declarator_type);
	std::shared_ptr<CompoundStmtAST> body = parse_compound_stmt();
	std::shared_ptr<ExternAST> def = std::make_shared<ExternAST>(FunctionDefinitionAST(func_type, declarator_name, body));
	return std::make_pair(def, nullptr);
}

std::shared_ptr<ExternAST> Parser::parse_declaration(std::shared_ptr<TypeAST> type)
{
	std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;

	if (!match(TokenName::tk_semicolon))
		while (true)
		{
			auto init_declarator = parser_init_declarator(type);
			declarators.push_back(init_declarator);

			if (match(TokenName::tk_semicolon))
				break;
			else
				match(TokenName::tk_comma, true);
		}

	return std::make_shared<ExternAST>(DeclarationAST(type, declarators));
}

std::shared_ptr<TypeAST> Parser::parse_declaration_specifiers(bool include_storage = true, bool include_qualifier = true)
{
	std::shared_ptr<StorageSpecifier> storage_specifier;
	std::set<TypeQualifier> type_qualifiers;

	auto parse_storage_or_qualifier = [&storage_specifier, &type_qualifiers, &include_storage, &include_qualifier, this]() {
		if (include_storage)
			parse_storage_specifier(storage_specifier);
		if (include_qualifier)
			parse_type_qualifier(type_qualifiers);
	};
	auto make_builtin_ast = [&parse_storage_or_qualifier, &type_qualifiers](BuiltinTypeName name, unsigned size) {
		parse_storage_or_qualifier();
		return std::make_shared<TypeAST>(BuiltinTypeAST(std::make_shared<BuiltinTypeName>(name), size, size, type_qualifiers));
	};

	parse_storage_or_qualifier();

	std ::shared_ptr<Token> token = advance();
	assert(token);

	if (token->type == TokenType::tk_symbol)
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		assert(token_symbol);

		switch (token_symbol->name)
		{
		case TokenName::tk_void:
			return make_builtin_ast(BuiltinTypeName::void_, 0);
		case TokenName::tk_bool:
			return make_builtin_ast(BuiltinTypeName::_Bool, 1);
		case TokenName::tk_char:
			return make_builtin_ast(BuiltinTypeName::char_, 1);
		case TokenName::tk_short:
			return make_builtin_ast(BuiltinTypeName::short_, 2);
		case TokenName::tk_int:
			return make_builtin_ast(BuiltinTypeName::int_, 4);
		case TokenName::tk_float:
			return make_builtin_ast(BuiltinTypeName::float_, 4);
		case TokenName::tk_double:
			return make_builtin_ast(BuiltinTypeName::double_, 8);
		case TokenName::tk_long:
		{
			if (match(TokenName::tk_double))
				return make_builtin_ast(BuiltinTypeName::long_double, 16);
			else if (match(TokenName::tk_long))
				return make_builtin_ast(BuiltinTypeName::long_long, 8);
			else
				return make_builtin_ast(BuiltinTypeName::long_, 8);
		}
		case TokenName::tk_signed:
		{
			if (match(TokenName::tk_char))
				return make_builtin_ast(BuiltinTypeName::signed_char, 1);
			else if (match(TokenName::tk_short))
				return make_builtin_ast(BuiltinTypeName::short_, 2);
			else if (match(TokenName::tk_int))
				return make_builtin_ast(BuiltinTypeName::int_, 4);
			else if (match(TokenName::tk_long))
			{
				if (match(TokenName::tk_long))
					return make_builtin_ast(BuiltinTypeName::long_long, 8);
				return make_builtin_ast(BuiltinTypeName::long_, 8);
			}
		}
		case TokenName::tk_unsigned:
		{
			if (match(TokenName::tk_char))
				return make_builtin_ast(BuiltinTypeName::unsigned_char, 1);
			else if (match(TokenName::tk_short))
				return make_builtin_ast(BuiltinTypeName::unsigned_short, 2);
			else if (match(TokenName::tk_int))
				return make_builtin_ast(BuiltinTypeName::unsigned_int, 4);
			else if (match(TokenName::tk_long))
			{
				if (match(TokenName::tk_long))
					return make_builtin_ast(BuiltinTypeName::unsigned_long_long, 8);
				return make_builtin_ast(BuiltinTypeName::unsigned_long, 8);
			}
		}

		case TokenName::tk_struct:
		case TokenName::tk_union:
		{
			std::shared_ptr<TokenIdentifier> token_identifier;
			std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members;
			AggregateKind kind = token_symbol->name == TokenName::tk_struct ? AggregateKind::struct_ : AggregateKind::union_;

			if (match(TokenType::tk_identifier, false, false))
				token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(advance());

			if (match(TokenName::tk_left_brace, true) && !match(TokenName::tk_right_brace))
			{
				do
				{
					auto declarators = parse_struct_declaration();
					for (auto declarator : declarators)
						members.push_back(declarator);
				} while (match(TokenName::tk_right_brace));
			}

			parse_storage_or_qualifier();
			return std::make_shared<TypeAST>(AggregateTypeAST(kind, token_identifier, members));
		}

		case TokenName::tk_enum:
		{
			std::shared_ptr<TokenIdentifier> token_identifier;
			std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members;

			if (match(TokenType::tk_identifier, false, false))
				token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(advance());

			if (match(TokenName::tk_left_brace, true) && !match(TokenName::tk_right_brace))
			{
				while (true)
				{
					auto enumerator = parse_enumerator();
					members.push_back(enumerator);
					if (match(TokenName::tk_right_brace))
						break;
					else
						match(TokenName::tk_comma, true);
				}
			}

			parse_storage_or_qualifier();
			return std::make_shared<TypeAST>(EnumTypeAST(token_identifier, members));
		}

		default:
			throw ParserError("Only support struct, union, enum or builtin types");
		}
	}
	else if (match(TokenType::tk_identifier))
	{
		std::shared_ptr<TokenIdentifier> token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);

		parse_storage_or_qualifier();
		return std::make_shared<TypeAST>(AliasTypeAST(token_identifier));
	}
	else
		throw ParserError("Only support typedef and struct/union/enum/builtin types");
}

void Parser::parse_storage_specifier(std::shared_ptr<StorageSpecifier> storage_specifier)
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
			storage_specifier = std::make_shared<StorageSpecifier>(StorageSpecifier::auto_);
			break;
		case TokenName::tk_register:
			storage_specifier = std::make_shared<StorageSpecifier>(StorageSpecifier::register_);
			break;
		case TokenName::tk_static:
			storage_specifier = std::make_shared<StorageSpecifier>(StorageSpecifier::static_);
			break;
		case TokenName::tk_extern:
			storage_specifier = std::make_shared<StorageSpecifier>(StorageSpecifier::extern_);
			break;

		default:
			return;
		}
	}
}

void Parser::parse_type_qualifier(std::set<TypeQualifier> &type_qualifiers)
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

std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>> Parser::parser_init_declarator(std::shared_ptr<TypeAST> type)
{
	auto [identifier, inner_type] = parse_declarator(type);
	std::shared_ptr<ExprAST> expr;
	if (match(TokenName::tk_equal))
		expr = parse_expr();
	return std::make_tuple(identifier, inner_type, expr);
}

std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> Parser::parse_struct_declaration()
{
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> declarators;
	auto type = parse_declaration_specifiers(false, true);
	while (true)
	{
		auto declarator = parse_declarator(type);
		declarators.push_back(declarator);
		if (match(TokenName::tk_semicolon))
			break;
		match(TokenName::tk_comma, true);
	}
	return declarators;
}

std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>> Parser::parse_enumerator()
{
	std::shared_ptr<TokenIdentifier> identifier = std::dynamic_pointer_cast<TokenIdentifier>(advance());
	std::shared_ptr<ExprAST> expr;

	if (match(TokenName::tk_equal))
		expr = parse_expr();

	return std::make_pair(identifier, expr);
}

std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>> Parser::parse_declarator(std::shared_ptr<TypeAST> type)
{
	if (match(TokenName::tk_asterisk))
	{
		std::set<TypeQualifier> qualifiers;
		parse_type_qualifier(qualifiers);
		std::shared_ptr<TypeAST> pointer = std::make_shared<TypeAST>(PointerTypeAST(type, qualifiers));
		return parse_declarator(pointer);
	}

	std::shared_ptr<TokenIdentifier> identifier;
	std::shared_ptr<TypeAST> outer_type;
	std::shared_ptr<TypeAST> inner_type;
	if (match(TokenName::tk_left_paren))
	{
		auto declarator = parse_declarator(nullptr);
		identifier = declarator.first;
		inner_type = declarator.second;
		match(TokenName::tk_right_paren, true);
	}
	else if (match(TokenType::tk_identifier))
	{
		std::shared_ptr<Token> token = advance();
		identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);
	}

	if (auto parameters_type = parser_declarator_parameters())
		outer_type = std::make_shared<FunctionTypeAST>(FunctionTypeAST(*parameters_type, type));
	else if (auto array_type = parse_declarator_array(type))
		outer_type = array_type;
	else
		outer_type = type;

	if (inner_type)
		inner_type->relate(outer_type);

	return std::make_pair(identifier, inner_type ?: outer_type);
}

std::shared_ptr<ArrayTypeAST> Parser::parse_declarator_array(std::shared_ptr<TypeAST> type)
{
	if (!match(TokenName::tk_left_bracket))
		return nullptr;

	std::shared_ptr<ExprAST> expr = parse_expr();
	auto array_type = std::make_shared<ArrayTypeAST>(ArrayTypeAST(type, expr));
	match(TokenName::tk_right_bracket, true);
	return array_type;
}

std::shared_ptr<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>> Parser::parser_declarator_parameters()
{
	if (!match(TokenName::tk_left_paren))
		return nullptr;

	std::shared_ptr<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>> parameters;
	while (match(TokenName::tk_right_paren))
	{
		std::shared_ptr<TypeAST> type = parse_declaration_specifiers();
		auto [identifier, inner_type] = parse_declarator(type);
		auto parameter = std::make_pair(identifier, inner_type);
		parameters->push_back(parameter);
	}
	return parameters;
}

bool Parser::match(TokenName name, bool strict = false, bool advance = true)
{
	std::shared_ptr<TokenSymbol> token = std::dynamic_pointer_cast<TokenSymbol>(tokens->at(runner));
	assert(token);

	if (token->name == name)
	{
		if (advance)
			runner++;
		return true;
	}
	else if (strict)
		throw ParserError("Expect token symbol");

	return false;
}

bool Parser::match(std::string name, bool strict = false, bool advance = true)
{
	std::shared_ptr<TokenIdentifier> token = std::dynamic_pointer_cast<TokenIdentifier>(tokens->at(runner));
	assert(token);

	if (token->name == name)
	{
		if (advance)
			runner++;
		return true;
	}
	else if (strict)
		throw ParserError("Expect token symbol");

	return false;
}

bool Parser::match(TokenType type, bool strict = false, bool advance = true)
{
	std::shared_ptr<Token> token = tokens->at(runner);
	assert(token);

	if (token->type == type)
	{
		if (advance)
			runner++;
		return true;
	}
	else if (strict)
		throw ParserError("Expect token type");

	return false;
}

std::shared_ptr<Token> Parser::advance()
{
	return tokens->at(runner++);
}
