#include "parser.h"

#include "ast.h"
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

/*
top level only supports declaration or function definition
1. mark current position
2. try parsing function definition
3. if exist -> add to list
   | otherwise -> revert back to that position and parse declaration
*/
std::shared_ptr<Program> Parser::parse()
{
	auto token = tokens->at(current);

	while (runner < tokens_length)
	{
		current = runner;

		auto declaration = parse_function_definition();
		if (!declaration)
			declaration = parse_declaration();

		program->add_declaration_stmt(declaration);
	}
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
	auto type = parse_declaration_specifiers();
	if (!type)
		return parse_not_match();

	auto [declarator_name, declarator_type] = parse_declarator(type);
	if (!declarator_name)
		return parse_not_match();

	auto token = tokens->at(runner);
	assert(token && token->type == TokenType::tk_symbol);

	auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
	if (token_symbol->name == TokenName::tk_equal
		|| token_symbol->name == TokenName::tk_comma
		|| token_symbol->name == TokenName::tk_semicolon)
		return parse_not_match();
	match(TokenName::tk_left_brace, true, false);

	auto func_type = std::dynamic_pointer_cast<FunctionTypeAST>(declarator_type);
	auto body = parse_compound_stmt();
	return std::make_shared<ExternAST>(FunctionDefinitionAST(func_type, declarator_name, body));
}

std::shared_ptr<ExternAST> Parser::parse_declaration()
{
	auto type = parse_declaration_specifiers();
	if (!type)
		return parse_not_match();

	std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;

	if (!match(TokenName::tk_semicolon))
		while (true)
		{
			auto init_declarator = parser_init_declarator(type);
			if (!std::get<0>(init_declarator))
				return nullptr;

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

	auto token = advance();
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
			auto kind = token_symbol->name == TokenName::tk_struct ? AggregateKind::struct_ : AggregateKind::union_;

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
		auto token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);

		parse_storage_or_qualifier();
		return std::make_shared<TypeAST>(AliasTypeAST(token_identifier));
	}
	else
		return nullptr;
}

void Parser::parse_storage_specifier(std::shared_ptr<StorageSpecifier> storage_specifier)
{
	for (auto token = tokens->at(runner); runner < tokens_length && token && token->type == TokenType::tk_symbol; runner++)
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
	for (auto token = tokens->at(runner); runner < tokens_length && token && token->type == TokenType::tk_symbol; runner++)
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
	auto identifier = std::dynamic_pointer_cast<TokenIdentifier>(advance());
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

		auto pointer = std::make_shared<TypeAST>(PointerTypeAST(type, qualifiers));
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
		auto token = advance();
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

	auto expr = parse_expr();
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
		auto type = parse_declaration_specifiers();
		auto [identifier, inner_type] = parse_declarator(type);
		auto parameter = std::make_pair(identifier, inner_type);
		parameters->push_back(parameter);
	}
	return parameters;
}

std::shared_ptr<StmtAST> Parser::parse_stmt()
{
	auto token = advance();

	if (token->type == TokenType::tk_symbol)
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		switch (token_symbol->name)
		{
		case TokenName::tk_case:
			return parse_case_stmt();

		case TokenName::tk_default:
			return parse_default_stmt();

		case TokenName::tk_left_brace:
			return parse_compound_stmt();

		case TokenName::tk_if:
			return parse_if_stmt();

		case TokenName::tk_switch:
			return parse_switch_stmt();

		case TokenName::tk_while:
			return parse_while_stmt();

		case TokenName::tk_do:
			return parse_do_while_stmt();

		case TokenName::tk_for:
			return parse_for_stmt();

		case TokenName::tk_goto:
			return parse_goto_stmt();

		case TokenName::tk_continue:
			return parse_continue_stmt();

		case TokenName::tk_break:
			return parse_break_stmt();

		case TokenName::tk_return:
			return parse_return_stmt();

		default:
			break;
		}
	}
	else if (token->type == TokenType::tk_identifier)
	{
		auto token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);
		if (match(TokenName::tk_colon))
			return parse_label_stmt(token_identifier);
	}
	return parse_expr_stmt();
}

std::shared_ptr<LabelStmtAST> Parser::parse_label_stmt(std::shared_ptr<TokenIdentifier> identifier)
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
	if (!match(TokenName::tk_semicolon, true))
		expr = parse_expr();

	return std::make_shared<ExprStmtAST>(ExprStmtAST(expr));
}

std::shared_ptr<CompoundStmtAST> Parser::parse_compound_stmt()
{
	std::vector<std::shared_ptr<FragmentAST>> stmts;

	while (!match(TokenName::tk_right_brace))
	{
		if (auto declaration = parse_declaration())
			stmts.push_back(declaration);
		else if (auto stmt = parse_stmt())
			stmts.push_back(stmt);
		else
			assert_not_reached();
	}

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

	std::shared_ptr<ExprAST> init;
	if (!match(TokenName::tk_semicolon))
		init = parse_expr();

	std::shared_ptr<ExprAST> cond;
	if (!match(TokenName::tk_semicolon))
		cond = parse_expr();

	std::shared_ptr<ExprAST> inc;
	if (!match(TokenName::tk_right_paren))
		inc = parse_expr();

	auto stmt = parse_stmt();
	return std::make_shared<ForStmtAST>(ForStmtAST(init, cond, inc, stmt));
}

std::shared_ptr<JumpStmtAST> Parser::parse_goto_stmt()
{
	auto identifier = std::dynamic_pointer_cast<TokenIdentifier>(advance());
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
	if (!match(TokenName::tk_semicolon))
		expr = parse_expr();

	match(TokenName::tk_semicolon, true);
	return std::make_shared<ReturnStmtAST>(ReturnStmtAST(expr));
}

std::shared_ptr<ExprAST> Parser::parse_expr()
{
	auto expr = parse_assignment_expr();

	if (!match(TokenName::tk_semicolon, true, false) && match(TokenName::tk_comma))
	{
		auto right_expr = parse_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, right_expr, BinaryOperator::comma));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_assignment_expr()
{
	auto expr = parse_tenary_expr();

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_name) {
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
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
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
		auto expr1 = parse_expr();
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
		auto expr1 = parse_logical_and_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::or_));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_logical_and_expr()
{
	auto expr = parse_bitwise_or_expr();

	if (match(TokenName::tk_ampersand_ampersand))
	{
		auto expr1 = parse_bitwise_or_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::and_));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_bitwise_or_expr()
{
	auto expr = parse_bitwise_xor_expr();

	if (match(TokenName::tk_vertical))
	{
		auto expr1 = parse_bitwise_xor_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::bitwise_or));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_bitwise_xor_expr()
{
	auto expr = parse_bitwise_and_expr();

	if (match(TokenName::tk_caret))
	{
		auto expr1 = parse_bitwise_and_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::bitwise_xor));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_bitwise_and_expr()
{
	auto expr = parse_equality_expr();

	if (match(TokenName::tk_ampersand))
	{
		auto expr1 = parse_equality_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::bitwise_and));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_equality_expr()
{
	auto expr = parse_relational_expr();

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_tk) {
			return nxt_tk == TokenName::tk_equal_equal || nxt_tk == TokenName::tk_bang_equal;
		}))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_relational_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_relational_expr()
{
	auto expr = parse_shift_expr();

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_tk) {
			return nxt_tk == TokenName::tk_less
				   || nxt_tk == TokenName::tk_less_equal
				   || nxt_tk == TokenName::tk_greater
				   || nxt_tk == TokenName::tk_greater_equal;
		}))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_shift_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_shift_expr()
{
	auto expr = parse_additive_expr();

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_tk) {
			return nxt_tk == TokenName::tk_much_less
				   || nxt_tk == TokenName::tk_much_greater;
		}))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_additive_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_additive_expr()
{
	auto expr = parse_multiplice_expr();

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_tk) {
			return nxt_tk == TokenName::tk_plus
				   || nxt_tk == TokenName::tk_minus;
		}))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_multiplice_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_multiplice_expr()
{
	auto expr = parse_cast_expr();

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_tk) {
			return nxt_tk == TokenName::tk_ampersand
				   || nxt_tk == TokenName::tk_slash
				   || nxt_tk == TokenName::tk_percent;
		}))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_cast_expr();
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, binop_token[token_symbol->name]));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_cast_expr()
{
	std::shared_ptr<ExprAST> expr;

	if (match(TokenName::tk_left_paren))
	{
		std::shared_ptr<TypeAST> type = parse_declaration_specifiers(false);
		match(TokenName::tk_right_paren);

		auto expr1 = parse_unary_expr();
		expr = std::make_shared<TypeCastExprAST>(TypeCastExprAST(type, expr1));
	}
	else
		expr = parse_unary_expr();

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_unary_expr()
{
	std::shared_ptr<ExprAST> expr;

	auto token = tokens->at(runner);
	if (match([](TokenName nxt_tk) {
			return nxt_tk == TokenName::tk_plus_plus || nxt_tk == TokenName::tk_minus_minus;
		}))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_unary_expr();
		auto op = token_symbol->name == TokenName::tk_plus_plus ? UnaryOperator::prefix_increment : UnaryOperator::prefix_decrement;
		expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr1, op));
	}
	else if (match([](TokenName nxt_tk) {
				 return nxt_tk == TokenName::tk_ampersand
						|| nxt_tk == TokenName::tk_asterisk
						|| nxt_tk == TokenName::tk_plus
						|| nxt_tk == TokenName::tk_minus
						|| nxt_tk == TokenName::tk_tilde
						|| nxt_tk == TokenName::tk_bang;
			 }))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
		auto expr1 = parse_cast_expr();
		expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr1, unaryop_token[token_symbol->name]));
	}
	else if (match(TokenName::tk_sizeof))
	{
		if (match(TokenName::tk_left_paren))
		{
			auto type = parse_declaration_specifiers(false);
		}
		else
		{
			auto expr1 = parse_unary_expr();
			expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr1, UnaryOperator::sizeof_));
		}
	}
	else
		expr = parse_postfix_expr();

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_postfix_expr()
{
	auto expr = parse_primary_expr();

	if (match(TokenName::tk_left_bracket))
	{
		auto expr1 = parse_expr();
		match(TokenName::tk_right_bracket, true);
		expr = std::make_shared<BinaryExprAST>(BinaryExprAST(expr, expr1, BinaryOperator::array_subscript));
	}
	else if (match(TokenName::tk_left_paren))
	{
		std::vector<std::shared_ptr<ExprAST>> arguments;

		while (!match(TokenName::tk_right_paren))
		{
			auto arg = parse_assignment_expr();
			arguments.push_back(arg);

			if (!match(TokenName::tk_comma))
				break;
		}

		expr = std::make_shared<FunctionCallExprAST>(FunctionCallExprAST(expr, arguments));
	}
	else if (match([](TokenName nxt_tk) {
				 return nxt_tk == TokenName::tk_dot || nxt_tk == TokenName::tk_arrow;
			 }))
	{
		auto token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(advance());
		assert(token_identifier);
		expr = std::make_shared<MemberAccessExprAST>(MemberAccessExprAST(expr, token_identifier));
	}
	else if (match([](TokenName nxt_tk) {
				 return nxt_tk == TokenName::tk_plus_plus || nxt_tk == TokenName::tk_minus_minus;
			 }))
	{
		auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(advance());
		assert(token_symbol);
		auto op = token_symbol->name == TokenName::tk_plus_plus ? UnaryOperator::postfix_increment : UnaryOperator::postfix_decrement;
		expr = std::make_shared<UnaryExprAST>(UnaryExprAST(expr, op));
	}

	return expr;
}

std::shared_ptr<ExprAST> Parser::parse_primary_expr()
{
	std::shared_ptr<ExprAST> expr;

	auto token = tokens->at(runner);
	if (match(TokenName::tk_left_paren))
	{
		expr = parse_expr();
		match(TokenName::tk_right_paren, true);
	}
	else if (match(TokenType::tk_identifier))
	{
		auto token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);
		assert(token_identifier);
		expr = std::make_shared<IdentifierExprAST>(IdentifierExprAST(token_identifier));
	}
	else if (match(TokenType::tk_literal))
	{
		auto token_literal = std::dynamic_pointer_cast<TokenLiteral>(token);
		expr = std::make_shared<LiteralExprAST>(LiteralExprAST(token_constant));
	}
	else
		assert_not_reached();

	return expr;
}

bool Parser::match(TokenName name, bool strict = false, bool advance = true)
{
	return match([&name](TokenName nxt_name) {
		return nxt_name == name;
	},
				 strict,
				 advance);
}

bool Parser::match(std::function<bool(TokenName)> comparator, bool strict = false, bool advance = true)
{
	auto token = std::dynamic_pointer_cast<TokenSymbol>(tokens->at(runner));
	assert(token);

	if (comparator(token->name))
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
	auto token = std::dynamic_pointer_cast<TokenIdentifier>(tokens->at(runner));
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
	auto token = tokens->at(runner);
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

std::nullptr_t Parser::parse_not_match()
{
	runner = current;
	return nullptr;
}