#ifndef AST_EXPR_H
#define AST_EXPR_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

#include "scan/token.h"
#include "type.h"

class ExprAST : public ASTNode
{
public:
	ExprAST() {}
	ExprAST(std::shared_ptr<TypeAST> type)
		: type(type)
	{
	}

	std::shared_ptr<TypeAST> type;
};

template <class T>
class LiteralExprAST : public ExprAST
{
public:
	LiteralExprAST(std::shared_ptr<TokenLiteral<T>> value)
		: value(value)
	{
	}

	std::shared_ptr<TokenLiteral<T>> value;
};

class IdentifierExprAST : public ExprAST  // function and variable
{
public:
	IdentifierExprAST(std::shared_ptr<TokenIdentifier> name)
		: name(name)
	{
	}

	std::shared_ptr<TokenIdentifier> name;
};

class BinaryExprAST : public ExprAST
{
public:
	BinaryExprAST(std::shared_ptr<ExprAST> left, std::shared_ptr<ExprAST> right, BinaryOperator op)
		: left(left)
		, right(right)
		, op(op)
	{
	}

	std::shared_ptr<ExprAST> left;
	std::shared_ptr<ExprAST> right;
	BinaryOperator op;
};

class UnaryExprAST : public ExprAST
{
public:
	UnaryExprAST(std::shared_ptr<ExprAST> expr, UnaryOperator op)
		: expr(expr)
		, op(op)
	{
	}

	std::shared_ptr<ExprAST> expr;
	UnaryOperator op;
};

class TenaryExprAST : public ExprAST
{
public:
	TenaryExprAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<ExprAST> expr1, std::shared_ptr<ExprAST> expr2)
		: cond(cond)
		, expr1(expr1)
		, expr2(expr2)
	{
	}

	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<ExprAST> expr1;
	std::shared_ptr<ExprAST> expr2;
};

class MemberAccessExprAST : public ExprAST
{
public:
	MemberAccessExprAST(std::shared_ptr<ExprAST> object, std::shared_ptr<TokenIdentifier> member)
		: object(object)
		, member(member)
	{
	}

	std::shared_ptr<ExprAST> object;
	std::shared_ptr<TokenIdentifier> member;
};

class FunctionCallExprAST : public ExprAST
{
public:
	FunctionCallExprAST(std::shared_ptr<ExprAST> callee, std::vector<std::shared_ptr<ExprAST>> arguments)
		: callee(callee)
		, arguments(arguments)
	{
	}

	std::shared_ptr<ExprAST> callee;
	std::vector<std::shared_ptr<ExprAST>> arguments;
};

class TypeCastExprAST : public ExprAST
{
public:
	TypeCastExprAST(std::shared_ptr<TypeAST> type, std::shared_ptr<ExprAST> expr)
		: ExprAST(type)
		, expr(expr)
	{
	}

	std::shared_ptr<ExprAST> expr;
};

class InitializerExprAST : public ExprAST
{
public:
	InitializerExprAST(std::vector<std::shared_ptr<ExprAST>> exprs)
		: exprs(exprs)
	{
	}

	std::vector<std::shared_ptr<ExprAST>> exprs;
};

#endif
