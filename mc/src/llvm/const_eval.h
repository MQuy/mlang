#ifndef LLVM_CONST_EVAL_H
#define LLVM_CONST_EVAL_H 1

#include <memory>

#include "ast/parser.h"
#include "semantic/translation_unit.h"

class ConstExprEval
{
public:
	ConstExprEval(TranslationUnit translation_unit, std::shared_ptr<ExprAST> expr)
		: translation_unit(translation_unit)
		, expr(expr)
	{
	}

	int eval();

private:
	TranslationUnit translation_unit;
	std::shared_ptr<ExprAST> expr;
};

#endif
