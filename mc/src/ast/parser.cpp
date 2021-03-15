#include "parser.h"

#include "ast.h"
#include "utils.h"

// NOTE: MQ 2021-03-14 top level only supports declaration or definition
std::shared_ptr<Program> Parser::parse()
{
	auto token = tokens->at(current);

	while (current < tokens_length)
	{
		auto declaration_specifier = parse_declaration_specifiers();
		auto init_declarator_list = parser_init_declarator_list();

		if (init_declarator_list.size() > 0)
		{
			assert_token(TokenName::tk_semicolon);
			declaration_specifier->initialize(std::move(init_declarator_list));
		}
		else
		{
			auto defs = std::dynamic_pointer_cast<FunctionDefStmtAST>(declaration_specifier);
			assert(defs);

			std::shared_ptr<CompoundStmtAST> stmt = parse_compound_stmt();
			defs->initialize(std::move(*stmt));
		}
	}
}

void Parser::parse_storage_or_qualifier(StorageSpecifier &storage_specifier,
										std::set<TypeQualifier> &type_qualifiers)
{
	bool jump_out = false;
	for (Token token = tokens->at(current); current < tokens_length
											&& token.type == TokenType::tk_symbol
											&& !jump_out;
		 current++)
	{
		auto token_symbol = static_cast<TokenSymbol *>(&token);
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
			jump_out = true;
			break;
		}
	}
}

std::shared_ptr<DclAST> Parser::parse_declaration_specifiers()
{
	StorageSpecifier storage_specifier;
	std::set<TypeQualifier> type_qualifiers;

	auto make_declaration_ast = [&storage_specifier, &type_qualifiers, this](BuiltinTypeName name, unsigned size) {
		parse_storage_or_qualifier(storage_specifier, type_qualifiers);
		return std::make_shared<DclAST>(BasicDclAST(
			BuiltinTypeAST(name, size, size, nullptr, nullptr, type_qualifiers),
			std::make_shared<StorageSpecifier>(storage_specifier)));
	};

	parse_storage_or_qualifier(storage_specifier, type_qualifiers);

	Token token = tokens->at(current);
	if (token.type == TokenType::tk_symbol)
	{
		auto token_symbol = static_cast<TokenSymbol *>(&token);
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
			if (look_ahead_and_match(TokenName::tk_double))
				return make_declaration_ast(BuiltinTypeName::long_double, 16);
			else if (look_ahead_and_match(TokenName::tk_long))
				return make_declaration_ast(BuiltinTypeName::long_long, 8);
			else
				return make_declaration_ast(BuiltinTypeName::long_, 8);
		}
		case TokenName::tk_signed:
		{
			if (look_ahead_and_match(TokenName::tk_char))
				return make_declaration_ast(BuiltinTypeName::signed_char, 1);
			else if (look_ahead_and_match(TokenName::tk_short))
				return make_declaration_ast(BuiltinTypeName::short_, 2);
			else if (look_ahead_and_match(TokenName::tk_int))
				return make_declaration_ast(BuiltinTypeName::int_, 4);
			else if (look_ahead_and_match(TokenName::tk_long))
			{
				if (look_ahead_and_match(TokenName::tk_long))
					return make_declaration_ast(BuiltinTypeName::long_long, 8);
				return make_declaration_ast(BuiltinTypeName::long_, 8);
			}
		}
		case TokenName::tk_unsigned:
		{
			if (look_ahead_and_match(TokenName::tk_char))
				return make_declaration_ast(BuiltinTypeName::unsigned_char, 1);
			else if (look_ahead_and_match(TokenName::tk_short))
				return make_declaration_ast(BuiltinTypeName::unsigned_short, 2);
			else if (look_ahead_and_match(TokenName::tk_int))
				return make_declaration_ast(BuiltinTypeName::unsigned_int, 4);
			else if (look_ahead_and_match(TokenName::tk_long))
			{
				if (look_ahead_and_match(TokenName::tk_long))
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
