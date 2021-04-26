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
	ExprAST(ASTNodeType node_type)
		: ASTNode(node_type)
	{
	}
	ExprAST(ASTNodeType node_type, std::shared_ptr<TypeAST> type)
		: ASTNode(node_type)
		, type(type)
	{
	}
	virtual void *accept(NodeVisitor *visitor) = 0;

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
	void *accept(NodeVisitor *visitor)
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
	void *accept(NodeVisitor *visitor) { return visitor->visit_identifier_expr(this); }

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
	void *accept(NodeVisitor *visitor) { return visitor->visit_binary_expr(this); }

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
	void *accept(NodeVisitor *visitor) { return visitor->visit_unary_expr(this); }

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
	void *accept(NodeVisitor *visitor) { return visitor->visit_tenary_expr(this); }

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
	void *accept(NodeVisitor *visitor) { return visitor->visit_member_access_expr(this); }

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
	void *accept(NodeVisitor *visitor) { return visitor->visit_function_call_expr(this); }

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
	void *accept(NodeVisitor *visitor) { return visitor->visit_typecast_expr(this); }

	std::shared_ptr<ExprAST> expr;
};

class SizeOfExprAST : public ExprAST
{
public:
	SizeOfExprAST(std::shared_ptr<TypeAST> size_of_type, std::shared_ptr<ExprAST> expr)
		: ExprAST(ASTNodeType::expr_sizeof, nullptr)
		, expr(expr)
		, size_of_type(size_of_type)
	{
	}
	void *accept(NodeVisitor *visitor) { return visitor->visit_sizeof_expr(this); }

	std::shared_ptr<ExprAST> expr;
	std::shared_ptr<TypeAST> size_of_type;
};

class AlignOfExprAST : public ExprAST
{
public:
	AlignOfExprAST(std::shared_ptr<TypeAST> align_of_type)
		: ExprAST(ASTNodeType::expr_alignof, nullptr)
		, align_of_type(align_of_type)
	{
	}
	void *accept(NodeVisitor *visitor) { return visitor->visit_alignof_expr(this); }

	std::shared_ptr<TypeAST> align_of_type;
};

class InitializerExprAST : public ExprAST
{
public:
	InitializerExprAST(std::vector<std::shared_ptr<ExprAST>> exprs)
		: ExprAST(ASTNodeType::expr_initializer)
		, exprs(exprs)
	{
	}
	void *accept(NodeVisitor *visitor) { return visitor->visit_initializer_expr(this); }

	std::vector<std::shared_ptr<ExprAST>> exprs;
};

#endif
