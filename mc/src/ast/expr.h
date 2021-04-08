#ifndef AST_EXPR_H
#define AST_EXPR_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

#include "scan/token.h"
#include "type.h"

template <class T>
class LiteralExprAST;
class IdentifierExprAST;
class BinaryExprAST;
class UnaryExprAST;
class TenaryExprAST;
class MemberAccessExprAST;
class FunctionCallExprAST;
class TypeCastExprAST;
class InitializerExprAST;

class ExprVisitor
{
public:
	virtual void *visit_literal_expr(LiteralExprAST<int> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned int> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned long long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<float> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<double> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long double> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned char> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<std::string> *expr) = 0;
	virtual void *visit_identifier_expr(IdentifierExprAST *expr) = 0;
	virtual void *visit_binary_expr(BinaryExprAST *expr) = 0;
	virtual void *visit_unary_expr(UnaryExprAST *expr) = 0;
	virtual void *visit_tenary_expr(TenaryExprAST *expr) = 0;
	virtual void *visit_member_access_expr(MemberAccessExprAST *expr) = 0;
	virtual void *visit_function_call_expr(FunctionCallExprAST *expr) = 0;
	virtual void *visit_typecast_expr(TypeCastExprAST *expr) = 0;
	virtual void *visit_initializer_expr(InitializerExprAST *expr) = 0;
};

class ExprAST : public ASTNode
{
public:
	ExprAST(ASTNodeType node_type)
		: ASTNode(node_type)
	{
	}
	ExprAST(ASTNodeType node_type, std::shared_ptr<TypeAST> type)
		: ASTNode(node_type)
		, type(type)
	{
	}
	virtual void *accept(ExprVisitor *visitor) = 0;

	std::shared_ptr<TypeAST> type;
};

template <class T>
class LiteralExprAST : public ExprAST
{
public:
	LiteralExprAST(std::shared_ptr<TokenLiteral<T>> value)
		: ExprAST(ASTNodeType::expr_literal)
		, value(value)
	{
	}
	void *accept(ExprVisitor *visitor)
	{
		return visitor->visit_literal_expr(this);
	}

	std::shared_ptr<TokenLiteral<T>> value;
};

class IdentifierExprAST : public ExprAST  // function and variable
{
public:
	IdentifierExprAST(std::shared_ptr<TokenIdentifier> name)
		: ExprAST(ASTNodeType::expr_identifier)
		, name(name)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_identifier_expr(this); }

	std::shared_ptr<TokenIdentifier> name;
};

class BinaryExprAST : public ExprAST
{
public:
	BinaryExprAST(std::shared_ptr<ExprAST> left, std::shared_ptr<ExprAST> right, BinaryOperator op)
		: ExprAST(ASTNodeType::expr_binary)
		, left(left)
		, right(right)
		, op(op)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_binary_expr(this); }

	std::shared_ptr<ExprAST> left;
	std::shared_ptr<ExprAST> right;
	BinaryOperator op;
};

class UnaryExprAST : public ExprAST
{
public:
	UnaryExprAST(std::shared_ptr<ExprAST> expr, UnaryOperator op)
		: ExprAST(ASTNodeType::expr_unary)
		, expr(expr)
		, op(op)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_unary_expr(this); }

	std::shared_ptr<ExprAST> expr;
	UnaryOperator op;
};

class TenaryExprAST : public ExprAST
{
public:
	TenaryExprAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<ExprAST> expr1, std::shared_ptr<ExprAST> expr2)
		: ExprAST(ASTNodeType::expr_tenary)
		, cond(cond)
		, expr1(expr1)
		, expr2(expr2)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_tenary_expr(this); }

	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<ExprAST> expr1;
	std::shared_ptr<ExprAST> expr2;
};

enum class MemberAccessType
{
	dot,
	arrow,
};
class MemberAccessExprAST : public ExprAST
{
public:
	MemberAccessExprAST(std::shared_ptr<ExprAST> object, std::shared_ptr<TokenIdentifier> member, MemberAccessType access_type)
		: ExprAST(ASTNodeType::expr_member_access)
		, object(object)
		, member(member)
		, access_type(access_type)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_member_access_expr(this); }

	std::shared_ptr<ExprAST> object;
	std::shared_ptr<TokenIdentifier> member;
	MemberAccessType access_type;
};

class FunctionCallExprAST : public ExprAST
{
public:
	FunctionCallExprAST(std::shared_ptr<ExprAST> callee, std::vector<std::shared_ptr<ExprAST>> arguments)
		: ExprAST(ASTNodeType::expr_function_call)
		, callee(callee)
		, arguments(arguments)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_function_call_expr(this); }

	std::shared_ptr<ExprAST> callee;
	std::vector<std::shared_ptr<ExprAST>> arguments;
};

class TypeCastExprAST : public ExprAST
{
public:
	TypeCastExprAST(std::shared_ptr<TypeAST> type, std::shared_ptr<ExprAST> expr)
		: ExprAST(ASTNodeType::expr_typecast, type)
		, expr(expr)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_typecast_expr(this); }

	std::shared_ptr<ExprAST> expr;
};

class InitializerExprAST : public ExprAST
{
public:
	InitializerExprAST(std::vector<std::shared_ptr<ExprAST>> exprs)
		: ExprAST(ASTNodeType::expr_initializer)
		, exprs(exprs)
	{
	}
	void *accept(ExprVisitor *visitor) { return visitor->visit_initializer_expr(this); }

	std::vector<std::shared_ptr<ExprAST>> exprs;
};

#endif
