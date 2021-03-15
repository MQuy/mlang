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
protected:
	std::optional<TypeAST> type;
};

template <class T>
class LiteralExprAST : public ExprAST
{
private:
	TokenLiteral<T> token;
};

class IdentifierExprAST : public ExprAST  // function and variable
{
private:
	TokenIdentifier name;
};

class BinaryExprAST : public ExprAST
{
private:
	ExprAST left;
	ExprAST right;
	BinaryOperator op;
};

class UnaryExprAST : public ExprAST
{
private:
	ExprAST expr;
	UnaryOperator op;
};

class TenaryExprAST : public ExprAST
{
private:
	ExprAST condition;
	ExprAST expr1;
	ExprAST expr2;
};

class MemberAccessExprAST : public ExprAST
{
private:
	IdentifierExprAST identifier;
	TokenIdentifier member;
};

class FunctionCallExprAST : public ExprAST
{
private:
	IdentifierExprAST callee;
	std::vector<ExprAST> arguments;
};

class TypeCastExprAST : public ExprAST
{
private:
	ExprAST expr;
};

#endif
