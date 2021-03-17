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
public:
	LabelStmtAST(std::shared_ptr<TokenIdentifier> name, std::shared_ptr<StmtAST> stmt)
		: name(name)
		, stmt(stmt)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
	std::shared_ptr<StmtAST> stmt;
};

class CaseStmtAST : public StmtAST
{
public:
	CaseStmtAST(std::shared_ptr<ExprAST> constant, std::shared_ptr<StmtAST> stmt)
		: constant(constant)
		, stmt(stmt)
	{
	}

private:
	std::shared_ptr<ExprAST> constant;
	std::shared_ptr<StmtAST> stmt;
};

class DefaultStmtAST : public StmtAST
{
public:
	DefaultStmtAST(std::shared_ptr<StmtAST> stmt)
		: stmt(stmt)
	{
	}

private:
	std::shared_ptr<StmtAST> stmt;
};

class ExprStmtAST : public StmtAST
{
public:
	ExprStmtAST(std::shared_ptr<ExprAST> expr)
		: expr(expr)
	{
	}

private:
	std::shared_ptr<ExprAST> expr;
};

class CompoundStmtAST : public StmtAST
{
public:
	CompoundStmtAST(std::vector<std::shared_ptr<FragmentAST>> stmts)
		: stmts(stmts)
	{
	}

protected:
	std::vector<std::shared_ptr<FragmentAST>> stmts;  // have to statement or declaration
};

class IfStmtAST : public StmtAST
{
public:
	IfStmtAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<StmtAST> if_stmt, std::shared_ptr<StmtAST> else_stmt)
		: cond(cond)
		, if_stmt(if_stmt)
		, else_stmt(else_stmt)
	{
	}

private:
	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<StmtAST> if_stmt;
	std::shared_ptr<StmtAST> else_stmt;
};

class SwitchStmtAST : public StmtAST
{
public:
	SwitchStmtAST(std::shared_ptr<ExprAST> expr, std::shared_ptr<StmtAST> stmt)
		: expr(expr)
		, stmt(stmt)
	{
	}

private:
	std::shared_ptr<ExprAST> expr;
	std::shared_ptr<StmtAST> stmt;
};

class ForStmtAST : public StmtAST
{
public:
	ForStmtAST(std::shared_ptr<ExprAST> init, std::shared_ptr<ExprAST> cond, std::shared_ptr<ExprAST> inc, std::shared_ptr<StmtAST> stmt)
		: init(init)
		, cond(cond)
		, inc(inc)
		, stmt(stmt)
	{
	}

private:
	std::shared_ptr<ExprAST> init;
	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<ExprAST> inc;
	std::shared_ptr<StmtAST> stmt;
};

class WhileStmtAST : public StmtAST
{
public:
	WhileStmtAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<StmtAST> stmt)
		: cond(cond)
		, stmt(stmt)
	{
	}

private:
	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<StmtAST> stmt;
};

class DoWhileStmtAST : public StmtAST
{
public:
	DoWhileStmtAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<StmtAST> stmt)
		: cond(cond)
		, stmt(stmt)
	{
	}

private:
	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<StmtAST> stmt;
};

class JumpStmtAST : public StmtAST
{
public:
	JumpStmtAST(std::shared_ptr<TokenIdentifier> name)
		: name(name)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
};

class ContinueStmtAST : public StmtAST
{
};

class BreakStmtAST : public StmtAST
{
};

class ReturnStmtAST : public StmtAST
{
public:
	ReturnStmtAST(std::shared_ptr<ExprAST> expr)
		: expr(expr)
	{
	}

private:
	std::shared_ptr<ExprAST> expr;
};

class ExternAST : public FragmentAST
{
public:
	ExternAST(std::shared_ptr<TypeAST> type)
		: type(type)
	{
	}

protected:
	std::shared_ptr<TypeAST> type;
};

class FunctionDefinitionAST : public ExternAST
{
public:
	FunctionDefinitionAST(std::shared_ptr<FunctionTypeAST> type, std::shared_ptr<TokenIdentifier> name, std::shared_ptr<CompoundStmtAST> body)
		: ExternAST(type)
		, name(name)
		, body(body)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
	std::shared_ptr<CompoundStmtAST> body;
};

class DeclarationAST : public ExternAST
{
public:
	DeclarationAST(std::shared_ptr<TypeAST> type, std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators = std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>>())
		: ExternAST(type)
	{
	}

private:
	std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;
};

class Program
{
public:
	void add_declaration_stmt(std::shared_ptr<ExternAST> dcl_stmt);

private:
	std::vector<std::shared_ptr<ExternAST>> declarations;
};

#endif
