#ifndef AST_STMT_H
#define AST_STMT_H 1

#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "expr.h"
#include "scan/token.h"
#include "type.h"

class FragmentAST : public ASTNode
{
};

class StmtAST : public FragmentAST
{
};

class LabelStmtAST : public StmtAST
{
private:
	TokenIdentifier name;
	std::shared_ptr<StmtAST> stmt;
};

class CaseStmtAST : public StmtAST
{
private:
	std::shared_ptr<ExprAST> constant;
	std::shared_ptr<StmtAST> stmt;
};

class DefaultStmtAST : public StmtAST
{
private:
	std::shared_ptr<StmtAST> stmt;
};

class ExprStmtAST : public StmtAST
{
private:
	std::shared_ptr<ExprAST> expr;
};

class CompoundStmtAST : public StmtAST
{
protected:
	std::vector<FragmentAST> stmts;	 // have to statement or declaration
};

class IfStmtAST : public StmtAST
{
private:
	std::shared_ptr<ExprAST> condition;
	std::shared_ptr<StmtAST> if_body;
	std::shared_ptr<StmtAST> else_body;
};

class ForStmtAST : public StmtAST  // use for while?
{
private:
	std::shared_ptr<StmtAST> initializer;
	std::shared_ptr<ExprAST> condition;
	std::shared_ptr<StmtAST> increment;
	std::shared_ptr<StmtAST> body;
};

class DoWhileStmtAST : public StmtAST
{
private:
	enum DoWhileKind
	{
		do_,
		while_,
	} kind;
	std::shared_ptr<ExprAST> condition;
	std::shared_ptr<StmtAST> body;
};

class JumpStmtAST : public StmtAST
{
private:
	TokenIdentifier name;
};

class ContinueStmtAST : public StmtAST
{
};

class BreakStmtAST : public StmtAST
{
};

class ReturnStmtAST : public StmtAST
{
private:
	std::vector<std::shared_ptr<ExprAST>> expr;
};

class DclAST : public FragmentAST
{
public:
	DclAST(std::shared_ptr<TypeAST> type)
		: type(type)
	{
	}

protected:
	std::shared_ptr<TypeAST> type;
};

class FunctionDefStmtAST : public DclAST
{
public:
	FunctionDefStmtAST(std::shared_ptr<FunctionTypeAST> type, std::shared_ptr< TokenIdentifier> name, std::shared_ptr<CompoundStmtAST> body)
		: DclAST(type)
		, name(name)
		, body(body)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
	std::shared_ptr<CompoundStmtAST> body;
};

class ProtoDclAST : public DclAST
{
private:
	std::shared_ptr<TokenIdentifier> name;
	std::vector<std::tuple<std::string, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;  // null declarator's type means declaration's type
	std::pair<std::shared_ptr<std::string>, std::shared_ptr<TypeAST>> parameters;
};

class BasicDclAST : public DclAST
{
public:
	BasicDclAST(std::shared_ptr<TypeAST> type, std::shared_ptr<StorageSpecifier> storage = nullptr, std::vector<std::tuple<std::string, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators = std::vector<std::tuple<std::string, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>>())
		: DclAST(type)
	{
	}

private:
	std::vector<std::tuple<std::string, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;  // null declarator's type means declaration's type
};

class AggregateDclAST : public DclAST
{
private:
	AggregateKind kind;
	std::shared_ptr<TokenIdentifier> name;
	std::vector<std::pair<std::shared_ptr<std::string>, std::shared_ptr<TypeAST>>> members;
	std::vector<std::tuple<std::string, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;
};

class EnumDclAST : public DclAST
{
private:
	std::shared_ptr<TokenIdentifier> name;
	std::vector<std::pair<std::string, int>> members;
	std::vector<std::tuple<std::string, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;  // null declarator's type means declaration's type
};

class Program
{
public:
	void add_declaration_stmt(std::shared_ptr<DclAST> dcl_stmt);

private:
	std::vector<std::shared_ptr<DclAST>> declarations;
};

#endif
