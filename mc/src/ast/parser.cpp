#include "parser.h"

#include "utils.h"

std::unordered_map<TokenName, BinaryOperator> binop_token;
std::unordered_map<TokenName, UnaryOperator> unaryop_token;

void init_operators()
{
	binop_token[TokenName::tk_equal] = BinaryOperator::assignment;
	binop_token[TokenName::tk_plus_equal] = BinaryOperator::addition_assigment;
	binop_token[TokenName::tk_minus_equal] = BinaryOperator::subtraction_assignment;
	binop_token[TokenName::tk_asterisk_equal] = BinaryOperator::multiplication_assigment;
	binop_token[TokenName::tk_slash_equal] = BinaryOperator::division_assignment;
	binop_token[TokenName::tk_percent_equal] = BinaryOperator::remainder_assignment;
	binop_token[TokenName::tk_ampersand_equal] = BinaryOperator::bitwise_and_assigment;
	binop_token[TokenName::tk_vertical_equal] = BinaryOperator::bitwise_or_assigment;
	binop_token[TokenName::tk_caret_equal] = BinaryOperator::bitwise_xor_assigment;
	binop_token[TokenName::tk_much_less_equal] = BinaryOperator::shift_left_assignment;
	binop_token[TokenName::tk_much_greater_equal] = BinaryOperator::shift_right_assignment;
	binop_token[TokenName::tk_plus] = BinaryOperator::addition;
	binop_token[TokenName::tk_minus] = BinaryOperator::subtraction;
	binop_token[TokenName::tk_asterisk] = BinaryOperator::multiplication;
	binop_token[TokenName::tk_slash] = BinaryOperator::division;
	binop_token[TokenName::tk_percent] = BinaryOperator::remainder;
	binop_token[TokenName::tk_ampersand] = BinaryOperator::bitwise_and;
	binop_token[TokenName::tk_vertical] = BinaryOperator::bitwise_or;
	binop_token[TokenName::tk_caret] = BinaryOperator::bitwise_xor;
	binop_token[TokenName::tk_much_less] = BinaryOperator::shift_left;
	binop_token[TokenName::tk_much_greater] = BinaryOperator::shift_right;
	binop_token[TokenName::tk_ampersand_ampersand] = BinaryOperator::and_;
	binop_token[TokenName::tk_vertical_vertical] = BinaryOperator::or_;
	binop_token[TokenName::tk_equal_equal] = BinaryOperator::equal;
	binop_token[TokenName::tk_bang_equal] = BinaryOperator::not_equal;
	binop_token[TokenName::tk_less] = BinaryOperator::less;
	binop_token[TokenName::tk_greater] = BinaryOperator::greater_than;
	binop_token[TokenName::tk_less_equal] = BinaryOperator::less_or_equal;
	binop_token[TokenName::tk_greater_equal] = BinaryOperator::greater_or_equal;
	binop_token[TokenName::tk_dot] = BinaryOperator::member_access;
	binop_token[TokenName::tk_comma] = BinaryOperator::comma;

	unaryop_token[TokenName::tk_ampersand] = UnaryOperator::address_of;
	unaryop_token[TokenName::tk_asterisk] = UnaryOperator::dereference;
	unaryop_token[TokenName::tk_plus] = UnaryOperator::plus;
	unaryop_token[TokenName::tk_minus] = UnaryOperator::minus;
	unaryop_token[TokenName::tk_tilde] = UnaryOperator::complement;
	unaryop_token[TokenName::tk_bang] = UnaryOperator::not_;
}

void Parser::reset()
{
	current = 0;
	runner = 0;
	declarations.clear();
}

/*
top level only supports declaration or function definition
1. mark current position
2. try parsing function definition
3. if exist -> add to list
   | otherwise -> revert back to that position and parse declaration
*/
std::vector<std::shared_ptr<ExternAST>> Parser::parse()
{
	reset();

	auto token = tokens.at(current);

	while (runner < tokens_length)
	{
		current = runner;

		auto declaration = parse_function_definition();
		if (!declaration)
			declaration = parse_declaration(true);

		declarations.push_back(declaration);
	}

	return declarations;
}

/*
1. parse type and mark current
2. parse declarator
3. if next token == '=' | ',' | ';' -> return null
4. next token != '{' -> raise exception (don't support old style)
5. parse component statement
6. return function def
*/
std::shared_ptr<ExternAST> Parser::parse_function_definition()
{
	auto type = parse_declaration_specifiers(true);
	if (!type)
		return parse_not_match();

	auto [declarator_name, declarator_type] = parse_declarator(type);
	if (!declarator_name)
		return parse_not_match();

	auto token = tokens.at(runner);
	assert(token && token->type == TokenType::tk_symbol);

	auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
	if (token_symbol->name == TokenName::tk_equal
		|| token_symbol->name == TokenName::tk_comma
		|| token_symbol->name == TokenName::tk_semicolon)
		return parse_not_match();
	match(TokenName::tk_left_brace, true);

	environment->define(declarator_name, SymbolType::declarator);

	auto func_type = std::static_pointer_cast<FunctionTypeAST>(declarator_type);
	for (auto [pname, ptype] : func_type->parameters)
		environment->define(pname, SymbolType::declarator);

	enter_scope();
	auto body = parse_compound_stmt();
	leave_scope();

	return std::make_shared<FunctionDefinitionAST>(FunctionDefinitionAST(func_type, declarator_name, body));
}

std::shared_ptr<ExternAST> Parser::parse_declaration(bool global_scope)
{
	auto type = parse_declaration_specifiers(global_scope);
	if (!type)
		return parse_not_match();

	std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;

	if (!match(TokenName::tk_semicolon))
		while (true)
		{
			auto init_declarator = parse_init_declarator(type);
			if (!std::get<0>(init_declarator))
				return nullptr;

			if (type->kind == TypeKind::alias)
			{
				auto atype = std::static_pointer_cast<AliasTypeAST>(type);
				if (environment->lookup(atype->name) == SymbolType::declarator)
					return parse_not_match();
			}

			auto symbol_type = environment->is_typedef(type) ? SymbolType::type : SymbolType::declarator;
			declarators.push_back(init_declarator);
			environment->define(std::get<0>(init_declarator), symbol_type);

			if (match(TokenName::tk_semicolon))
				break;
			else
				match(TokenName::tk_comma, true);
		}

	return std::make_shared<DeclarationAST>(DeclarationAST(type, declarators));
}

std::shared_ptr<TypeAST> Parser::parse_declaration_specifiers(bool global_scope, bool include_storage, bool include_qualifier)
{
	StorageSpecifier storage_specifier(global_scope ? StorageSpecifier::extern_ : StorageSpecifier::auto_);
	std::set<TypeQualifier> type_qualifiers;

	auto parse_storage_or_qualifier = [&storage_specifier, &type_qualifiers, &include_storage, &include_qualifier, this]()
	{
		std::vector<TokenName> tokenNames;
		if (include_storage)
			tokenNames.insert(tokenNames.end(), {TokenName::tk_auto, TokenName::tk_register, TokenName::tk_static, TokenName::tk_extern, TokenName::tk_typedef});
		if (include_qualifier)
			tokenNames.insert(tokenNames.end(), {TokenName::tk_const, TokenName::tk_volatile, TokenName::tk_static, TokenName::tk_restrict});

		while (tokens[runner]->match([&tokenNames](TokenName name)
									 {
										 return std::find(tokenNames.begin(), tokenNames.end(), name) != tokenNames.end();
									 }))
		{
			if (include_storage)
				parse_storage_specifier(storage_specifier);
			if (include_qualifier)
				parse_type_qualifier(type_qualifiers);
		}
	};
	auto make_builtin_ast = [&parse_storage_or_qualifier, &type_qualifiers, &storage_specifier](BuiltinTypeName name, unsigned size)
	{
		parse_storage_or_qualifier();
		return std::make_shared<BuiltinTypeAST>(BuiltinTypeAST(name, size, size, type_qualifiers, storage_specifier));
	};

	parse_storage_or_qualifier();

	auto token = advance();
	assert(token);

	if (token->type == TokenType::tk_symbol)
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);

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
			{
				match(TokenName::tk_int, false);
				return make_builtin_ast(BuiltinTypeName::long_long, 8);
			}
			else
			{
				match(TokenName::tk_int, false);
				return make_builtin_ast(BuiltinTypeName::long_, 8);
			}
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
			else
				return make_builtin_ast(BuiltinTypeName::int_, 4);
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
			else
				return make_builtin_ast(BuiltinTypeName::unsigned_int, 4);
		}

		case TokenName::tk_struct:
		case TokenName::tk_union:
		{
			std::shared_ptr<TokenIdentifier> token_identifier;
			std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members;
			auto kind = token_symbol->name == TokenName::tk_struct ? AggregateKind::struct_ : AggregateKind::union_;

			if (match(TokenType::tk_identifier, false, false))
				token_identifier = std::static_pointer_cast<TokenIdentifier>(advance());

			if (match(TokenName::tk_left_brace) && !match(TokenName::tk_right_brace))
			{
				do
				{
					auto declarators = parse_struct_declaration();
					for (auto declarator : declarators)
						members.push_back(declarator);
				} while (!match(TokenName::tk_right_brace));
			}

			parse_storage_or_qualifier();
			return std::make_shared<AggregateTypeAST>(AggregateTypeAST(kind, token_identifier, members, type_qualifiers, storage_specifier));
		}

		case TokenName::tk_enum:
		{
			std::shared_ptr<TokenIdentifier> token_identifier;
			std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members;

			if (match(TokenType::tk_identifier, false, false))
				token_identifier = std::static_pointer_cast<TokenIdentifier>(advance());

			if (match(TokenName::tk_left_brace) && !match(TokenName::tk_right_brace))
			{
				while (!match(TokenName::tk_right_brace))
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
			return std::make_shared<EnumTypeAST>(EnumTypeAST(token_identifier, members, type_qualifiers, storage_specifier));
		}

		default:
			return nullptr;
		}
	}
	else if (token->type == TokenType::tk_identifier)
	{
		auto token_identifier = std::static_pointer_cast<TokenIdentifier>(token);

		parse_storage_or_qualifier();
		return std::make_shared<AliasTypeAST>(AliasTypeAST(token_identifier, type_qualifiers, storage_specifier));
	}
	else
		return nullptr;
}

void Parser::parse_storage_specifier(StorageSpecifier& storage_specifier)
{
	for (; runner < tokens_length; runner++)
	{
		auto token = tokens.at(runner);
		if (!token->match(TokenType::tk_symbol))
			break;

		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);

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
		case TokenName::tk_typedef:
			storage_specifier = StorageSpecifier::typedef_;
			break;

		default:
			return;
		}
	}
}

void Parser::parse_type_qualifier(std::set<TypeQualifier>& type_qualifiers)
{
	for (; runner < tokens_length; runner++)
	{
		auto token = tokens.at(runner);
		if (!token->match(TokenType::tk_symbol))
			break;

		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);

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

std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>> Parser::parse_init_declarator(const std::shared_ptr<TypeAST>& type)
{
	auto [identifier, inner_type] = parse_declarator(type);

	std::shared_ptr<ExprAST> expr;
	if (identifier && match(TokenName::tk_equal))
		expr = parse_initializer();

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
	auto token = advance();
	std::shared_ptr<TokenIdentifier> token_identifier = nullptr;
	std::shared_ptr<ExprAST> expr = nullptr;

	if (token->type == TokenType::tk_identifier)
		token_identifier = std::static_pointer_cast<TokenIdentifier>(token);

	if (match(TokenName::tk_equal))
		expr = parse_tenary_expr();

	return std::make_pair(token_identifier, expr);
}

void Parser::revert_type_relation(const std::shared_ptr<TypeAST>& source_type, const std::shared_ptr<TypeAST> dest_type, const std::shared_ptr<TypeAST>& anchor_type)
{
	if (source_type != anchor_type)
	{
		if (source_type->kind == TypeKind::function)
		{
			auto ftype_src = std::static_pointer_cast<FunctionTypeAST>(source_type);
			if (dest_type->kind == TypeKind::function)
			{
				auto ftype_dest = std::static_pointer_cast<FunctionTypeAST>(dest_type);
				ftype_dest->returning = ftype_src->returning;
				ftype_src->returning = ftype_dest;
			}
			else if (dest_type->kind == TypeKind::array)
			{
				auto atype_dest = std::static_pointer_cast<ArrayTypeAST>(dest_type);
				atype_dest->underlay = ftype_src->returning;
				ftype_src->returning = atype_dest;
			}
			else
				assert_not_reached();
		}
		else if (source_type->kind == TypeKind::array)
		{
			auto atype_src = std::static_pointer_cast<ArrayTypeAST>(source_type);
			if (dest_type->kind == TypeKind::function)
			{
				auto ftype_dest = std::static_pointer_cast<FunctionTypeAST>(dest_type);
				ftype_dest->returning = atype_src->underlay;
				atype_src->underlay = ftype_dest;
			}
			else if (dest_type->kind == TypeKind::array)
			{
				auto atype_dest = std::static_pointer_cast<ArrayTypeAST>(dest_type);
				atype_dest->underlay = atype_src->underlay;
				atype_src->underlay = atype_dest;
			}
			else
				assert_not_reached();
		}
		else
			assert_not_reached();
	}
}

std::shared_ptr<TypeAST> Parser::parse_typename()
{
	auto number_of_parens = 0;
	while (match(TokenName::tk_left_paren))
		number_of_parens++;

	std::shared_ptr<TypeAST> type = parse_declaration_specifiers(false);
	if (!type)
		return nullptr;

	if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		if (environment->lookup(atype->name) == SymbolType::declarator)
			return nullptr;
	}

	auto [declarator_name, declarator_type] = parse_declarator(type, false);

	for (; number_of_parens > 0; number_of_parens--)
		if (!match(TokenName::tk_right_paren))
			return nullptr;

	return declarator_type;
}

std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>> Parser::parse_declarator(const std::shared_ptr<TypeAST>& type, bool abstract)
{
	if (match(TokenName::tk_asterisk))
	{
		std::set<TypeQualifier> qualifiers;
		parse_type_qualifier(qualifiers);

		auto pointer = std::make_shared<PointerTypeAST>(PointerTypeAST(type, qualifiers));
		return parse_declarator(pointer);
	}

	std::shared_ptr<TokenIdentifier> identifier;
	std::shared_ptr<TypeAST> outer_type = type;
	std::shared_ptr<TypeAST> inner_type;

	if (match(TokenType::tk_identifier, false, false) && !abstract)
	{
		auto token = advance();
		if (token->type == TokenType::tk_identifier)
			identifier = std::static_pointer_cast<TokenIdentifier>(token);
	}
	else if (match(TokenName::tk_left_paren))
	{
		auto declarator = parse_declarator(nullptr);
		identifier = declarator.first;
		inner_type = declarator.second;
		// NOTE: allow to rollback for function call like $ foo(1);
		match(TokenName::tk_right_paren);
	}

	for (std::shared_ptr<TypeAST> type_runner = type;;)
	{
		if (auto parameters = parse_declarator_parameters())
		{
			auto [parameters_type, is_variadic_args] = *parameters;
			auto ftype = std::make_shared<FunctionTypeAST>(FunctionTypeAST(parameters_type, type_runner, is_variadic_args));

			revert_type_relation(type_runner, ftype, type);
			if (type_runner == type)
				outer_type = ftype;
			type_runner = ftype;
		}
		else if (auto array_type = parse_declarator_array(type_runner))
		{
			revert_type_relation(type_runner, array_type, type);
			if (type_runner == type)
				outer_type = array_type;
			type_runner = array_type;
		}
		else
			break;
	}

	if (inner_type)
		inner_type->relate(outer_type);

	return std::make_pair(identifier, inner_type ? inner_type : outer_type);
}

std::shared_ptr<ArrayTypeAST> Parser::parse_declarator_array(const std::shared_ptr<TypeAST>& type)
{
	if (!match(TokenName::tk_left_bracket))
		return nullptr;

	std::shared_ptr<ExprAST> expr;
	if (!match(TokenName::tk_right_bracket, false, false))
		expr = parse_expr();
	match(TokenName::tk_right_bracket, true);

	return std::make_shared<ArrayTypeAST>(ArrayTypeAST(type, expr));
}

std::shared_ptr<std::pair<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>, bool>> Parser::parse_declarator_parameters()
{
	if (!match(TokenName::tk_left_paren))
		return nullptr;

	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters;

	bool is_variadic_args = false;
	if (!match(TokenName::tk_right_paren))
	{
		auto cond = true;
		if (match(TokenName::tk_ellipsis))
		{
			match(TokenName::tk_right_paren, true);
			cond = false;
			is_variadic_args = true;
		}
		else if (match(TokenName::tk_void))
			if (match(TokenName::tk_right_paren))
				cond = false;
			else
				runner--;

		while (cond)
		{
			auto type = parse_declaration_specifiers(false);
			auto [identifier, inner_type] = parse_declarator(type);
			auto parameter = std::make_pair(identifier, inner_type);
			parameters.push_back(parameter);

			if (match(TokenName::tk_right_paren))
				break;
			else
			{
				match(TokenName::tk_comma, true);
				if (match(TokenName::tk_ellipsis))
				{
					is_variadic_args = true;
					match(TokenName::tk_right_paren, true);
					break;
				}
			}
		}
	}

	return std::make_shared<std::pair<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>, bool>>(std::make_pair(parameters, is_variadic_args));
}

void Parser::enter_scope()
{
	environment = new NameEnvironment(environment);
}

void Parser::leave_scope()
{
	environment = environment->get_enclosing();
}

std::shared_ptr<StmtAST> Parser::parse_stmt()
{
	if (match(TokenName::tk_case))
		return parse_case_stmt();
	else if (match(TokenName::tk_default))
		return parse_default_stmt();
	else if (match(TokenName::tk_left_brace))
		return parse_compound_stmt();
	else if (match(TokenName::tk_if))
		return parse_if_stmt();
	else if (match(TokenName::tk_switch))
		return parse_switch_stmt();
	else if (match(TokenName::tk_while))
		return parse_while_stmt();
	else if (match(TokenName::tk_do))
		return parse_do_while_stmt();
	else if (match(TokenName::tk_for))
		return parse_for_stmt();
	else if (match(TokenName::tk_goto))
		return parse_goto_stmt();
	else if (match(TokenName::tk_continue))
		return parse_continue_stmt();
	else if (match(TokenName::tk_break))
		return parse_break_stmt();
	else if (match(TokenName::tk_return))
		return parse_return_stmt();
	else if (match(TokenType::tk_identifier, false, false))
	{
		auto pos = runner;
		auto token_identifier = std::static_pointer_cast<TokenIdentifier>(advance());
		if (match(TokenName::tk_colon))
			return parse_label_stmt(token_identifier);
		else
			runner = pos;
	}
	return parse_expr_stmt();
}

std::shared_ptr<LabelStmtAST> Parser::parse_label_stmt(const std::shared_ptr<TokenIdentifier>& identifier)
{
	auto stmt = parse_stmt();
	return std::make_shared<LabelStmtAST>(LabelStmtAST(identifier, stmt));
}

std::shared_ptr<CaseStmtAST> Parser::parse_case_stmt()
{
	auto constant = parse_expr();
	match(TokenName::tk_colon, true);

	auto stmt = parse_stmt();

	return std::make_shared<CaseStmtAST>(CaseStmtAST(constant, stmt));
}

std::shared_ptr<DefaultStmtAST> Parser::parse_default_stmt()
{
	match(TokenName::tk_colon, true);
	auto stmt = parse_stmt();

	return std::make_shared<DefaultStmtAST>(DefaultStmtAST(stmt));
}

std::shared_ptr<ExprStmtAST> Parser::parse_expr_stmt()
{
	std::shared_ptr<ExprAST> expr;
	if (!match(TokenName::tk_semicolon, false, false))
		expr = parse_expr();

	match(TokenName::tk_semicolon, true);
	return std::make_shared<ExprStmtAST>(ExprStmtAST(expr));
}

std::shared_ptr<CompoundStmtAST> Parser::parse_compound_stmt()
{
	std::vector<std::shared_ptr<FragmentAST>> stmts;

	enter_scope();
	while (!match(TokenName::tk_right_brace))
	{
		auto pos = runner;
		if (auto declaration = parse_declaration(false))
			stmts.push_back(declaration);
		else if (runner = pos; auto stmt = parse_stmt())
			stmts.push_back(stmt);
		else
			assert_not_reached();
	}
	leave_scope();

	return std::make_shared<CompoundStmtAST>(CompoundStmtAST(stmts));
}

std::shared_ptr<IfStmtAST> Parser::parse_if_stmt()
{
	match(TokenName::tk_left_paren, true);
	auto cond = parse_expr();
	match(TokenName::tk_right_paren, true);

	auto if_stmt = parse_stmt();

	std::shared_ptr<StmtAST> else_stmt;
	if (match(TokenName::tk_else))
		else_stmt = parse_stmt();

	return std::make_shared<IfStmtAST>(IfStmtAST(cond, if_stmt, else_stmt));
}

std::shared_ptr<SwitchStmtAST> Parser::parse_switch_stmt()
{
	match(TokenName::tk_left_paren, true);
	auto expr = parse_expr();
	match(TokenName::tk_right_paren, true);

	auto stmt = parse_stmt();

	return std::make_shared<SwitchStmtAST>(SwitchStmtAST(expr, stmt));
}

std::shared_ptr<WhileStmtAST> Parser::parse_while_stmt()
{
	match(TokenName::tk_left_paren, true);
	auto expr = parse_expr();
	match(TokenName::tk_right_paren, true);

	auto stmt = parse_stmt();

	return std::make_shared<WhileStmtAST>(WhileStmtAST(expr, stmt));
}

std::shared_ptr<DoWhileStmtAST> Parser::parse_do_while_stmt()
{
	auto stmt = parse_stmt();

	match(TokenName::tk_while, true);
	match(TokenName::tk_left_paren, true);
	auto expr = parse_expr();
	match(TokenName::tk_right_paren, true);
	match(TokenName::tk_semicolon, true);

	return std::make_shared<DoWhileStmtAST>(DoWhileStmtAST(expr, stmt));
}

std::shared_ptr<ForStmtAST> Parser::parse_for_stmt()
{
	match(TokenName::tk_left_paren, true);

	std::shared_ptr<ASTNode> init;
	if (!match(TokenName::tk_semicolon))
	{
		auto pos = runner;
		if (auto decl = parse_declaration(false))
			init = decl;
		else if (runner = pos; auto expr = parse_expr())
		{
			init = expr;
			match(TokenName::tk_semicolon, true);
		}
		else
			assert_not_reached();
	}

	std::shared_ptr<ExprAST> cond;
	if (!match(TokenName::tk_semicolon))
	{
		cond = parse_expr();
		match(TokenName::tk_semicolon, true);
	}

	std::shared_ptr<ExprAST> inc;
	if (!match(TokenName::tk_right_paren))
	{
		inc = parse_expr();
		match(TokenName::tk_right_paren, true);
	}

	auto stmt = parse_stmt();
	return std::make_shared<ForStmtAST>(ForStmtAST(init, cond, inc, stmt));
}

std::shared_ptr<JumpStmtAST> Parser::parse_goto_stmt()
{
	auto token = advance();
	std::shared_ptr<TokenIdentifier> identifier = nullptr;

	if (token->type == TokenType::tk_identifier)
		identifier = std::static_pointer_cast<TokenIdentifier>(token);

	assert(identifier);
	match(TokenName::tk_semicolon, true);

	return std::make_shared<JumpStmtAST>(JumpStmtAST(identifier));
}

std::shared_ptr<ContinueStmtAST> Parser::parse_continue_stmt()
{
	match(TokenName::tk_semicolon, true);
	return std::make_shared<ContinueStmtAST>(ContinueStmtAST());
}

std::shared_ptr<BreakStmtAST> Parser::parse_break_stmt()
{
	match(TokenName::tk_semicolon, true);
	return std::make_shared<BreakStmtAST>(BreakStmtAST());
}

std::shared_ptr<ReturnStmtAST> Parser::parse_return_stmt()
{
	std::shared_ptr<ExprAST> expr;
	if (!match(TokenName::tk_semicolon, false, false))
		expr = parse_expr();

	match(TokenName::tk_semicolon, true);
	return std::make_shared<ReturnStmtAST>(ReturnStmtAST(expr));
}

std::shared_ptr<ExprAST> Parser::parse_initializer()
{
	if (match(TokenName::tk_left_brace))
	{
		std::vector<std::shared_ptr<ExprAST>> exprs;
		while (!match(TokenName::tk_right_brace))
		{
			exprs.push_back(parse_initializer());
			if (match(TokenName::tk_right_brace))
				break;
			else
				match(TokenName::tk_comma, true);
		}

		return std::make_shared<InitializerExprAST>(InitializerExprAST(exprs));
	}
	else
		return parse_assignment_expr();
}

std::shared_ptr<ExprAST> Parser::parse_expr()
{
	auto expr = parse_assignment_expr();

	if (!match(TokenName::tk_semicolon, false, false) && match(TokenName::tk_comma))
	{
		auto right_expr = parse_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, right_expr, BinaryOperator::comma));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_assignment_expr()
{
	auto expr = parse_tenary_expr();

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_name)
			  {
				  return nxt_name == TokenName::tk_equal
						 || nxt_name == TokenName::tk_asterisk_equal
						 || nxt_name == TokenName::tk_slash_equal
						 || nxt_name == TokenName::tk_percent_equal
						 || nxt_name == TokenName::tk_plus_equal
						 || nxt_name == TokenName::tk_minus_equal
						 || nxt_name == TokenName::tk_much_less_equal
						 || nxt_name == TokenName::tk_much_greater_equal
						 || nxt_name == TokenName::tk_ampersand_equal
						 || nxt_name == TokenName::tk_vertical_equal
						 || nxt_name == TokenName::tk_caret_equal;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto right_expr = parse_assignment_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, right_expr, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_tenary_expr()
{
	auto expr = parse_logical_or_expr();

	if (match(TokenName::tk_question_mark))
	{
		auto expr1 = parse_tenary_expr();
		match(TokenName::tk_colon, true);
		auto expr2 = parse_tenary_expr();

		expr = std::make_shared<TenaryExprAST>(TenaryExprAST(expr, expr1, expr2));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_logical_or_expr()
{
	auto expr = parse_logical_and_expr();

	if (match(TokenName::tk_vertical_vertical))
	{
		auto expr1 = parse_logical_or_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::or_));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_logical_and_expr()
{
	auto expr = parse_bitwise_or_expr();

	if (match(TokenName::tk_ampersand_ampersand))
	{
		auto expr1 = parse_logical_and_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::and_));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_bitwise_or_expr()
{
	auto expr = parse_bitwise_xor_expr();

	if (match(TokenName::tk_vertical))
	{
		auto expr1 = parse_bitwise_or_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::bitwise_or));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_bitwise_xor_expr()
{
	auto expr = parse_bitwise_and_expr();

	if (match(TokenName::tk_caret))
	{
		auto expr1 = parse_bitwise_xor_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::bitwise_xor));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_bitwise_and_expr()
{
	auto expr = parse_equality_expr();

	if (match(TokenName::tk_ampersand))
	{
		auto expr1 = parse_bitwise_and_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::bitwise_and));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_equality_expr()
{
	auto expr = parse_relational_expr();

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_tk)
			  {
				  return nxt_tk == TokenName::tk_equal_equal || nxt_tk == TokenName::tk_bang_equal;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_equality_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_relational_expr()
{
	auto expr = parse_shift_expr();

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_tk)
			  {
				  return nxt_tk == TokenName::tk_less
						 || nxt_tk == TokenName::tk_less_equal
						 || nxt_tk == TokenName::tk_greater
						 || nxt_tk == TokenName::tk_greater_equal;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_relational_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_shift_expr()
{
	auto expr = parse_additive_expr();

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_tk)
			  {
				  return nxt_tk == TokenName::tk_much_less
						 || nxt_tk == TokenName::tk_much_greater;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_shift_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_additive_expr()
{
	auto expr = parse_multiplice_expr();

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_tk)
			  {
				  return nxt_tk == TokenName::tk_plus
						 || nxt_tk == TokenName::tk_minus;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_additive_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_multiplice_expr()
{
	auto expr = parse_cast_expr();

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_tk)
			  {
				  return nxt_tk == TokenName::tk_asterisk
						 || nxt_tk == TokenName::tk_slash
						 || nxt_tk == TokenName::tk_percent;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_multiplice_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_cast_expr()
{
	auto pos = runner;
	if (match(TokenName::tk_left_paren))
	{
		std::shared_ptr<TypeAST> type = parse_typename();
		if (type != nullptr && match(TokenName::tk_right_paren))
		{
			auto expr1 = parse_cast_expr();
			return std::make_shared<TypeCastExprAST>(TypeCastExprAST(type, expr1));
		}
		else
			runner = pos;
	}

	return parse_unary_expr();
}

std::shared_ptr<ExprAST> Parser::parse_unary_expr()
{
	std::shared_ptr<ExprAST> expr;

	auto token = tokens.at(runner);
	if (match([](TokenName nxt_tk)
			  {
				  return nxt_tk == TokenName::tk_plus_plus || nxt_tk == TokenName::tk_minus_minus;
			  }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_unary_expr();
		auto op = token_symbol->name == TokenName::tk_plus_plus ? UnaryOperator::prefix_increment : UnaryOperator::prefix_decrement;
		expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr1, op));
	}
	else if (match([](TokenName nxt_tk)
				   {
					   return nxt_tk == TokenName::tk_ampersand
							  || nxt_tk == TokenName::tk_asterisk
							  || nxt_tk == TokenName::tk_plus
							  || nxt_tk == TokenName::tk_minus
							  || nxt_tk == TokenName::tk_tilde
							  || nxt_tk == TokenName::tk_bang;
				   }))
	{
		auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_cast_expr();
		expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr1, unaryop_token[token_symbol->name]));
	}
	else if (match(TokenName::tk_sizeof))
	{
		auto pos = runner;
		if (match(TokenName::tk_left_paren))
		{
			auto type = parse_typename();
			auto is_type = type != nullptr;

			if (is_type && type->kind == TypeKind::alias)
			{
				auto atype = std::static_pointer_cast<AliasTypeAST>(type);
				is_type = is_type && environment->lookup(atype->name) == SymbolType::type;
			}

			if (is_type)
			{
				expr = std::make_shared<SizeOfExprAST>(SizeOfExprAST(type, nullptr));
				match(TokenName::tk_right_paren, true);
				expr->type = type;
				return expr;
			}
			else
				runner = pos;
		}

		auto expr1 = parse_unary_expr();
		expr = std::make_shared<SizeOfExprAST>(SizeOfExprAST(nullptr, expr1));
	}
	else if (match(TokenName::tk_alignof))
	{
		match(TokenName::tk_left_paren, true);
		auto type = parse_declaration_specifiers(false);
		assert(type);

		expr = std::make_shared<AlignOfExprAST>(AlignOfExprAST(type));
		match(TokenName::tk_right_paren, true);
		expr->type = type;
		return expr;
	}
	else
		expr = parse_postfix_expr();

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_postfix_expr()
{
	auto expr = parse_primary_expr();

	while (true)
	{
		auto token = tokens.at(runner);
		if (match(TokenName::tk_left_bracket))
		{
			auto expr1 = parse_expr();
			match(TokenName::tk_right_bracket, true);
			expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::array_subscript));
		}
		else if (match(TokenName::tk_left_paren))
		{
			std::vector<std::shared_ptr<ExprAST>> arguments;
			while (!match(TokenName::tk_right_paren, false, false))
			{
				auto arg = parse_assignment_expr();
				arguments.push_back(arg);

				if (match(TokenName::tk_right_paren, false, false))
					break;
				else
					match(TokenName::tk_comma, true);
			}
			match(TokenName::tk_right_paren, true);
			expr = std::make_shared<FunctionCallExprAST>(FunctionCallExprAST(expr, arguments));
		}
		else if (match(TokenName::tk_dot))
		{
			auto token = advance();
			std::shared_ptr<TokenIdentifier> token_identifier = nullptr;

			if (token->type == TokenType::tk_identifier)
				token_identifier = std::static_pointer_cast<TokenIdentifier>(token);

			assert(token_identifier);
			expr = std::make_shared<MemberAccessExprAST>(MemberAccessExprAST(expr, token_identifier, MemberAccessType::dot));
		}
		else if (match(TokenName::tk_arrow))
		{
			auto token = advance();
			std::shared_ptr<TokenIdentifier> token_identifier = nullptr;

			if (token->type == TokenType::tk_identifier)
				token_identifier = std::static_pointer_cast<TokenIdentifier>(token);

			assert(token_identifier);
			expr = std::make_shared<MemberAccessExprAST>(MemberAccessExprAST(expr, token_identifier, MemberAccessType::arrow));
		}
		else if (match([](TokenName nxt_tk)
					   {
						   return nxt_tk == TokenName::tk_plus_plus || nxt_tk == TokenName::tk_minus_minus;
					   }))
		{
			auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
			auto op = token_symbol->name == TokenName::tk_plus_plus ? UnaryOperator::postfix_increment : UnaryOperator::postfix_decrement;
			expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr, op));
		}
		else
			break;
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_primary_expr()
{
	std::shared_ptr<ExprAST> expr;

	auto token = tokens.at(runner);
	if (match(TokenName::tk_left_paren))
	{
		expr = parse_expr();
		match(TokenName::tk_right_paren, true);
	}
	else if (match([](TokenType nxt_type)
				   {
					   return nxt_type == TokenType::tk_identifier || nxt_type == TokenType::tk_literal;
				   }))
		expr = token->create_ast();
	else
		assert_not_reached();

	return expr;
}

bool Parser::match(TokenName name, bool strict, bool advance)
{
	return match([&name](TokenName nxt_name)
				 {
					 return nxt_name == name;
				 },
				 strict,
				 advance);
}

bool Parser::match(std::function<bool(TokenName)> comparator, bool strict, bool advance)
{
	auto token = tokens.at(runner);
	if (token->type != TokenType::tk_symbol)
		if (strict)
			throw ParserError("expect token symbol");
		else
			return false;

	auto token_symbol = std::static_pointer_cast<TokenSymbol>(token);
	if (comparator(token_symbol->name))
	{
		if (advance)
			runner++;
		return true;
	}
	else if (strict)
		throw ParserError("token symbol doesn't match");
	else
		return false;
}

bool Parser::match(std::string name, bool strict, bool advance)
{
	auto token = tokens.at(runner);
	if (token->type != TokenType::tk_identifier)
		if (strict)
			throw ParserError("expect token identifier");
		else
			return false;

	auto token_identifier = std::static_pointer_cast<TokenIdentifier>(tokens.at(runner));

	if (token_identifier->name == name)
	{
		if (advance)
			runner++;
		return true;
	}
	else if (strict)
		throw ParserError("token identifier " + token_identifier->name + " doesn't match " + name);
	else
		return false;
}

bool Parser::match(TokenType type, bool strict, bool advance)
{
	return match([&type](TokenType nxt_type)
				 {
					 return nxt_type == type;
				 },
				 strict,
				 advance);
}

bool Parser::match(std::function<bool(TokenType)> comparator, bool strict, bool advance)
{
	auto token = tokens.at(runner);
	assert(token);

	if (comparator(token->type))
	{
		if (advance)
			runner++;
		return true;
	}
	else if (strict)
		throw ParserError("Expect token type");
	else
		return false;
}

std::shared_ptr<Token> Parser::advance()
{
	return tokens.at(runner++);
}

std::nullptr_t Parser::parse_not_match()
{
	runner = current;
	return nullptr;
}
