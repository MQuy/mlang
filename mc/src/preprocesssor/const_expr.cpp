#include "const_expr.h"

/*
conditional_expression = logical_OR_expression
                        | logical_OR_expression, "?", conditional_expression, ":", conditional_expression;

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

multiplicative_expression = unary_expression
                          | multiplicative_expression, "*", cast_expression
                          | multiplicative_expression, "/", cast_expression
                          | multiplicative_expression, "%", cast_expression;

unary_expression = primary_expression
                  | unary_operator, unary_expression;

unary_operator = "+" | "-" | "~" | "!";

primary_expression = integer_constant
                    | "(", tenary_expression, ")";
*/
bool PreprocessorConstExpr::eval()
{
	// validate all tokens are in literal form except align and sizeof
	return eval_tenary();
}

int PreprocessorConstExpr::eval_tenary()
{
	int expr = eval_logical_or();

	if (match(TokenName::tk_question_mark))
	{
		int expr1 = eval_tenary();
		match(TokenName::tk_colon);
		int expr2 = eval_tenary();

		return expr ? expr1 : expr2;
	}

	return expr;
}

int PreprocessorConstExpr::eval_logical_or()
{
	int expr = eval_logical_and();

	if (match(TokenName::tk_vertical_vertical))
	{
		int expr1 = eval_logical_or();
		return expr || expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_logical_and()
{
	int expr = eval_bitwise_or();

	if (match(TokenName::tk_ampersand_ampersand))
	{
		int expr1 = eval_logical_and();
		return expr && expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_bitwise_or()
{
	int expr = eval_bitwise_xor();

	if (match(TokenName::tk_vertical))
	{
		int expr1 = eval_bitwise_or();
		return expr | expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_bitwise_xor()
{
	int expr = eval_bitwise_and();

	if (match(TokenName::tk_caret))
	{
		int expr1 = eval_bitwise_xor();
		return expr ^ expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_bitwise_and()
{
	int expr = eval_equality();

	if (match(TokenName::tk_ampersand))
	{
		int expr1 = eval_bitwise_and();
		return expr && expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_equality()
{
	int expr = eval_relation();

	if (match(TokenName::tk_equal_equal))
	{
		int expr1 = eval_equality();
		return expr == expr1;
	}
	else if (match(TokenName::tk_bang_equal))
	{
		int expr1 = eval_equality();
		return expr != expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_relation()
{
	int expr = eval_shift();

	if (match([](TokenName name) {
			return name == TokenName::tk_less
				   || name == TokenName::tk_greater
				   || name == TokenName::tk_less_equal
				   || name == TokenName::tk_greater_equal;
		}),
		false,
		false)
	{
		int expr1 = eval_relation();

		if (match(TokenName::tk_less))
			return expr < expr1;
		else if (match(TokenName::tk_greater))
			return expr > expr1;
		else if (match(TokenName::tk_less_equal))
			return expr <= expr1;
		else if (match(TokenName::tk_greater_equal))
			return expr >= expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_shift()
{
	int expr = eval_addition();

	if (match(TokenName::tk_much_less))
	{
		int expr1 = eval_shift();
		return expr << expr1;
	}
	else if (match(TokenName::tk_much_greater))
	{
		int expr1 = eval_shift();
		return expr >> expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_addition()
{
	int expr = eval_multiplication();

	if (match(TokenName::tk_plus))
	{
		int expr1 = eval_addition();
		return expr + expr1;
	}
	else if (match(TokenName::tk_minus))
	{
		int expr1 = eval_addition();
		return expr - expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_multiplication()
{
	int expr = eval_unary();

	if (match(TokenName::tk_asterisk))
	{
		int expr1 = eval_multiplication();
		return expr * expr1;
	}
	else if (match(TokenName::tk_slash))
	{
		int expr1 = eval_multiplication();
		return expr / expr1;
	}

	return expr;
}

int PreprocessorConstExpr::eval_unary()
{
	if (match(TokenName::tk_plus))
		return +eval_unary();
	else if (match(TokenName::tk_minus))
		return -eval_unary();
	else if (match(TokenName::tk_tilde))
		return ~eval_unary();
	else if (match(TokenName::tk_bang))
		return !eval_unary();

	return eval_primary();
}

int PreprocessorConstExpr::eval_primary()
{
	if (match(TokenName::tk_left_paren))
	{
		int expr = eval_tenary();
		match(TokenName::tk_right_paren, true);

		return expr;
	}

	match(TokenType::tk_literal, true, false);
	auto token = tokens.at(current);
	if (auto literal_integer = std::dynamic_pointer_cast<TokenLiteral<int>>(token))
		return literal_integer->value;
	else if (auto literal_char = std::dynamic_pointer_cast<TokenLiteral<unsigned char>>(token))
		return literal_char->value;
	else
		throw std::runtime_error("only support integer and char constants");
}

bool PreprocessorConstExpr::match(TokenName name, bool strict, bool advance)
{
	return match([&name](TokenName tk_name) {
		return tk_name == name;
	},
				 strict,
				 advance);
}

bool PreprocessorConstExpr::match(std::function<bool(TokenName)> comparator, bool strict, bool advance)
{
	auto token = tokens.at(current);
	if (token->type != TokenType::tk_symbol)
		if (strict)
			throw std::runtime_error("Expect token symbol");
		else
			return false;

	auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
	if (comparator(token_symbol->name))
	{
		if (advance)
			current++;
		return true;
	}
	else if (strict)
		throw std::runtime_error("token symbol is not matched");
	else
		return false;
}

bool PreprocessorConstExpr::match(TokenType type, bool strict, bool advance)
{
	auto token = tokens.at(current);
	if (token->type == type)
	{
		if (advance)
			current++;
		return true;
	}
	if (strict)
		throw std::runtime_error("Expect token type");
	else
		return false;
}
