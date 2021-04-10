#ifndef IR_CONST_EVAL_H
#define IR_CONST_EVAL_H 1

#include <memory>

#include "ast/parser.h"

class ConstExprEval
{
public:
	ConstExprEval(Program program, std::shared_ptr<ExprAST> expr)
		: program(program)
		, expr(expr)
	{
	}

	int eval();

private:
	Program program;
	std::shared_ptr<ExprAST> expr;
};

#endif
