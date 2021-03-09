#ifndef AST_EXPR_H
#define AST_EXPR_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

#include "token.h"
#include "type.h"

class ExprAST : public ASTNode
{
protected:
	std::optional<TypeAST> type;
};

template <class T>
class LiteralExprAST : ExprAST
{
private:
	TokenLiteral<T> token;
};

class IdentifierExprAST : ExprAST  // function and variable
{
private:
	TokenIdentifier name;
};

class BinaryExprAST : ExprAST
{
private:
	ExprAST left;
	ExprAST right;
	BinaryOperator op;
};

class UnaryExprAST : ExprAST
{
private:
	ExprAST expr;
	UnaryOperator op;
};

class TenaryExprAST : ExprAST
{
private:
	ExprAST condition;
	ExprAST expr1;
	ExprAST expr2;
};

class MemberAccessExprAST : ExprAST
{
private:
	IdentifierExprAST identifier;
	TokenIdentifier member;
};

class FunctionCallExprAST : ExprAST
{
private:
	IdentifierExprAST callee;
	std::vector<ExprAST> arguments;
};

class TypeCastExprAST : ExprAST
{
private:
	ExprAST expr;
};

#endif
