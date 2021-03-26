#ifndef PREPROCESSOR_CONST_EXPR_H
#define PREPROCESSOR_CONST_EXPR_H 1

#include "scan/token.h"

class PreprocessorConstExpr
{
public:
	PreprocessorConstExpr(std::vector<std::shared_ptr<Token>> &&tokens)
		: tokens(tokens)
		, current(0)
	{
	}

	bool eval();

private:
	int eval_tenary();
	int eval_logical_or();
	int eval_logical_and();
	int eval_bitwise_or();
	int eval_bitwise_xor();
	int eval_bitwise_and();
	int eval_equality();
	int eval_relation();
	int eval_shift();
	int eval_addition();
	int eval_multiplication();
	int eval_unary();
	int eval_primary();

	bool match(TokenName name, bool strict = false, bool advance = true);
	bool match(std::function<bool(TokenName)> comparator, bool strict = false, bool advance = true);
	bool match(std::string name, bool strict = false, bool advance = true);
	bool match(TokenType type, bool strict = false, bool advance = true);

	int current;
	std::vector<std::shared_ptr<Token>> tokens;
};

#endif
