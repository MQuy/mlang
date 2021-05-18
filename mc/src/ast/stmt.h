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
public:
	FragmentAST(ASTNodeType node_type)
		: ASTNode(node_type)
	{
	}
	virtual void *accept(NodeVisitor *visitor, void *data = nullptr) = 0;
};

class StmtAST : public FragmentAST
{
public:
	StmtAST(ASTNodeType node_type)
		: FragmentAST(node_type)
	{
	}
};

class LabelStmtAST : public StmtAST
{
public:
	LabelStmtAST(std::shared_ptr<TokenIdentifier> name, std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_label)
		, name(name)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_label_stmt(this, data); };

	std::shared_ptr<TokenIdentifier> name;
	std::shared_ptr<StmtAST> stmt;
};

class CaseStmtAST : public StmtAST
{
public:
	CaseStmtAST(std::shared_ptr<ExprAST> constant, std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_case)
		, constant(constant)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_case_stmt(this, data); };

	std::shared_ptr<ExprAST> constant;
	std::shared_ptr<StmtAST> stmt;
};

class DefaultStmtAST : public StmtAST
{
public:
	DefaultStmtAST(std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_default_)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_default_stmt(this, data); };

	std::shared_ptr<StmtAST> stmt;
};

class ExprStmtAST : public StmtAST
{
public:
	ExprStmtAST(std::shared_ptr<ExprAST> expr)
		: StmtAST(ASTNodeType::stmt_expr)
		, expr(expr)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_expr_stmt(this, data); };

	std::shared_ptr<ExprAST> expr;
};

class CompoundStmtAST : public StmtAST
{
public:
	CompoundStmtAST(std::vector<std::shared_ptr<FragmentAST>> stmts)
		: StmtAST(ASTNodeType::stmt_compound)
		, stmts(stmts)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_compound_stmt(this, data); };

	std::vector<std::shared_ptr<FragmentAST>> stmts;  // have to statement or declaration
};

class IfStmtAST : public StmtAST
{
public:
	IfStmtAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<StmtAST> if_stmt, std::shared_ptr<StmtAST> else_stmt)
		: StmtAST(ASTNodeType::stmt_if)
		, cond(cond)
		, if_stmt(if_stmt)
		, else_stmt(else_stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_if_stmt(this, data); };

	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<StmtAST> if_stmt;
	std::shared_ptr<StmtAST> else_stmt;
};

class SwitchStmtAST : public StmtAST
{
public:
	SwitchStmtAST(std::shared_ptr<ExprAST> expr, std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_switch)
		, expr(expr)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_switch_stmt(this, data); };

	std::shared_ptr<ExprAST> expr;
	std::shared_ptr<StmtAST> stmt;
};

class ForStmtAST : public StmtAST
{
public:
	ForStmtAST(std::shared_ptr<ASTNode> init, std::shared_ptr<ExprAST> cond, std::shared_ptr<ExprAST> inc, std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_for)
		, init(init)
		, cond(cond)
		, inc(inc)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_for_stmt(this, data); };

	std::shared_ptr<ASTNode> init;
	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<ExprAST> inc;
	std::shared_ptr<StmtAST> stmt;
};

class WhileStmtAST : public StmtAST
{
public:
	WhileStmtAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_while)
		, cond(cond)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_while_stmt(this, data); };

	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<StmtAST> stmt;
};

class DoWhileStmtAST : public StmtAST
{
public:
	DoWhileStmtAST(std::shared_ptr<ExprAST> cond, std::shared_ptr<StmtAST> stmt)
		: StmtAST(ASTNodeType::stmt_dowhile)
		, cond(cond)
		, stmt(stmt)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_dowhile_stmt(this, data); };

	std::shared_ptr<ExprAST> cond;
	std::shared_ptr<StmtAST> stmt;
};

class JumpStmtAST : public StmtAST
{
public:
	JumpStmtAST(std::shared_ptr<TokenIdentifier> name)
		: StmtAST(ASTNodeType::stmt_jump)
		, name(name)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_jump_stmt(this, data); };

	std::shared_ptr<TokenIdentifier> name;
};

class ContinueStmtAST : public StmtAST
{
public:
	ContinueStmtAST()
		: StmtAST(ASTNodeType::stmt_continue)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_continue_stmt(this, data); };
};

class BreakStmtAST : public StmtAST
{
public:
	BreakStmtAST()
		: StmtAST(ASTNodeType::stmt_break)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_break_stmt(this, data); };
};

class ReturnStmtAST : public StmtAST
{
public:
	ReturnStmtAST(std::shared_ptr<ExprAST> expr)
		: StmtAST(ASTNodeType::stmt_return)
		, expr(expr)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_return_stmt(this, data); };

	std::shared_ptr<ExprAST> expr;
};

class ExternAST : public FragmentAST
{
public:
	ExternAST(ASTNodeType node_type, std::shared_ptr<TypeAST> type)
		: FragmentAST(node_type)
		, type(type)
	{
	}

	std::shared_ptr<TypeAST> type;
};

class FunctionDefinitionAST : public ExternAST
{
public:
	FunctionDefinitionAST(std::shared_ptr<FunctionTypeAST> type, std::shared_ptr<TokenIdentifier> name, std::shared_ptr<CompoundStmtAST> body)
		: ExternAST(ASTNodeType::extern_function, type)
		, name(name)
		, body(body)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_function_definition(this, data); };

	std::shared_ptr<TokenIdentifier> name;
	std::shared_ptr<CompoundStmtAST> body;
};

class DeclarationAST : public ExternAST
{
public:
	DeclarationAST(std::shared_ptr<TypeAST> type, std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators = std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>>())
		: ExternAST(ASTNodeType::extern_declaration, type)
		, declarators(declarators)
	{
	}
	void *accept(NodeVisitor *visitor, void *data = nullptr) { return visitor->visit_declaration(this, data); };

	std::vector<std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>>> declarators;
};

#endif
