#ifndef LLVM_CONST_EVAL_H
#define LLVM_CONST_EVAL_H 1

#include <memory>

#include "ast/parser.h"
#include "semantic/translation_unit.h"

class IR;

class ConstExprEval : NodeVisitor
{
public:
	ConstExprEval(IR *ir, std::shared_ptr<ExprAST> expr)
		: ir(ir)
		, expr(expr)
	{
	}

	int eval();

	void *visit_literal_expr(LiteralExprAST<int> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<long> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<long long> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<unsigned int> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<unsigned long> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<unsigned long long> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<float> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<double> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<long double> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<unsigned char> *expr, void *data = nullptr);
	void *visit_literal_expr(LiteralExprAST<std::string> *expr, void *data = nullptr);
	void *visit_identifier_expr(IdentifierExprAST *expr, void *data = nullptr);
	void *visit_binary_expr(BinaryExprAST *expr, void *data = nullptr);
	void *visit_unary_expr(UnaryExprAST *expr, void *data = nullptr);
	void *visit_tenary_expr(TenaryExprAST *expr, void *data = nullptr);
	void *visit_member_access_expr(MemberAccessExprAST *expr, void *data = nullptr);
	void *visit_function_call_expr(FunctionCallExprAST *expr, void *data = nullptr);
	void *visit_typecast_expr(TypeCastExprAST *expr, void *data = nullptr);
	void *visit_sizeof_expr(SizeOfExprAST *expr, void *data = nullptr);
	void *visit_alignof_expr(AlignOfExprAST *expr, void *data = nullptr);
	void *visit_initializer_expr(InitializerExprAST *expr, void *data = nullptr);

	void *visit_label_stmt(LabelStmtAST *stmt, void *data = nullptr);
	void *visit_case_stmt(CaseStmtAST *stmt, void *data = nullptr);
	void *visit_default_stmt(DefaultStmtAST *stmt, void *data = nullptr);
	void *visit_expr_stmt(ExprStmtAST *stmt, void *data = nullptr);
	void *visit_compound_stmt(CompoundStmtAST *stmt, void *data = nullptr);
	void *visit_if_stmt(IfStmtAST *stmt, void *data = nullptr);
	void *visit_switch_stmt(SwitchStmtAST *stmt, void *data = nullptr);
	void *visit_for_stmt(ForStmtAST *stmt, void *data = nullptr);
	void *visit_while_stmt(WhileStmtAST *stmt, void *data = nullptr);
	void *visit_dowhile_stmt(DoWhileStmtAST *stmt, void *data = nullptr);
	void *visit_jump_stmt(JumpStmtAST *stmt, void *data = nullptr);
	void *visit_continue_stmt(ContinueStmtAST *stmt, void *data = nullptr);
	void *visit_break_stmt(BreakStmtAST *stmt, void *data = nullptr);
	void *visit_return_stmt(ReturnStmtAST *stmt, void *data = nullptr);

	void *visit_function_definition(FunctionDefinitionAST *stmt, void *data = nullptr);
	void *visit_declaration(DeclarationAST *stmt, void *data = nullptr);

private:
	TranslationUnit translation_unit;
	IR *ir;
	std::shared_ptr<ExprAST> expr;
};

#endif
