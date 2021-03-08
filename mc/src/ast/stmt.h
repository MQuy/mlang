#ifndef AST_STMT_H
#define AST_STMT_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

#include "expr.h"
#include "token.h"
#include "type.h"

class FragmentAST : public ASTNode
{
};

class StmtAST : public FragmentAST
{
};

class LabelStmtAST : StmtAST
{
private:
	TokenIdentifier name;
	StmtAST stmt;
};

class CaseStmtAST : StmtAST
{
private:
	ExprAST constant;
	StmtAST stmt;
};

class DefaultStmtAST : StmtAST
{
private:
	StmtAST stmt;
};

class ExprStmtAST : StmtAST
{
private:
	ExprAST expr;
};

class CompoundStmtAST : StmtAST
{
protected:
	std::vector<FragmentAST> stmts;	 // have to statement or declaration
};

class IfStmtAST : StmtAST
{
private:
	ExprAST condition;
	StmtAST if_body;
	StmtAST else_body;
};

class ForStmtAST : StmtAST	// use for while?
{
private:
	StmtAST initializer;
	ExprAST condition;
	StmtAST increment;
	StmtAST body;
};

class DoWhileStmtAST : StmtAST
{
private:
	enum DoWhileKind
	{
		__identifier(do),
		__identifier(while),
	} kind;
	ExprAST condition;
	StmtAST body;
};

class JumpStmtAST : StmtAST
{
private:
	TokenIdentifier name;
};

class ContinueStmtAST : StmtAST
{
};

class BreakStmtAST : StmtAST
{
};

class ReturnStmtAST : StmtAST
{
private:
	std::vector<ExprAST> expr;
};

class DclAST : FragmentAST
{
protected:
	TypeAST type;
};

class FunctionDefStmtAST : DclAST
{
private:
	std::optional<StorageSpecifier> storage;
	TokenIdentifier name;
	std::vector<std::tuple<std::string, TypeAST>> declarators;
	CompoundStmtAST body;
};

class ProtoDclAST : DclAST
{
private:
	std::optional<StorageSpecifier> storage;
	TokenIdentifier name;
	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> declarators;  // null declarator's type means declaration's type
	std::pair<std::optional<std::string>, TypeAST> parameters;
};

class BasicDclAST : DclAST
{
private:
	std::optional<StorageSpecifier> storage;
	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> declarators;  // null declarator's type means declaration's type
};

class AggregateDclAST : DclAST
{
private:
	AggregateKind kind;
	std::optional<StorageSpecifier> storage;
	std::optional<TokenIdentifier> name;
	std::vector<std::pair<std::optional<std::string>, TypeAST>> members;
	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> declarators;
};

class EnumDclAST : DclAST
{
private:
	std::optional<StorageSpecifier> storage;
	std::optional<TokenIdentifier> name;
	std::vector<std::pair<std::string, int>> members;
	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> declarators;  // null declarator's type means declaration's type
};

class TypedefDclAST : DclAST
{
private:
	TokenIdentifier name;
};

class Program
{
private:
	std::vector<DclAST> declarations;
};

#endif
