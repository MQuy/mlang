#ifndef SEMANTIC_TYPE_INFERENCE_H
#define SEMANTIC_TYPE_INFERENCE_H 1

#include "ast/expr.h"
#include "ast/stmt.h"
#include "type_environment.h"
#include "translation_unit.h"

class SemanticTypeInference : NodeVisitor
{
public:
	SemanticTypeInference(std::vector<std::shared_ptr<ExternAST>> declarations)
		: translation_unit(TranslationUnit(declarations))
		, environment(new TypeEnvironment(nullptr))
	{
	}

	TranslationUnit analyze();

	void *visit_literal_expr(LiteralExprAST<int> *expr);
	void *visit_literal_expr(LiteralExprAST<long> *expr);
	void *visit_literal_expr(LiteralExprAST<long long> *expr);
	void *visit_literal_expr(LiteralExprAST<unsigned int> *expr);
	void *visit_literal_expr(LiteralExprAST<unsigned long> *expr);
	void *visit_literal_expr(LiteralExprAST<unsigned long long> *expr);
	void *visit_literal_expr(LiteralExprAST<float> *expr);
	void *visit_literal_expr(LiteralExprAST<double> *expr);
	void *visit_literal_expr(LiteralExprAST<long double> *expr);
	void *visit_literal_expr(LiteralExprAST<unsigned char> *expr);
	void *visit_literal_expr(LiteralExprAST<std::string> *expr);
	void *visit_identifier_expr(IdentifierExprAST *expr);
	void *visit_binary_expr(BinaryExprAST *expr);
	void *visit_unary_expr(UnaryExprAST *expr);
	void *visit_tenary_expr(TenaryExprAST *expr);
	void *visit_member_access_expr(MemberAccessExprAST *expr);
	void *visit_function_call_expr(FunctionCallExprAST *expr);
	void *visit_typecast_expr(TypeCastExprAST *expr);
	void *visit_initializer_expr(InitializerExprAST *expr);

	void *visit_label_stmt(LabelStmtAST *stmt);
	void *visit_case_stmt(CaseStmtAST *stmt);
	void *visit_default_stmt(DefaultStmtAST *stmt);
	void *visit_expr_stmt(ExprStmtAST *stmt);
	void *visit_compound_stmt(CompoundStmtAST *stmt);
	void *visit_if_stmt(IfStmtAST *stmt);
	void *visit_switch_stmt(SwitchStmtAST *stmt);
	void *visit_for_stmt(ForStmtAST *stmt);
	void *visit_while_stmt(WhileStmtAST *stmt);
	void *visit_dowhile_stmt(DoWhileStmtAST *stmt);
	void *visit_jump_stmt(JumpStmtAST *stmt);
	void *visit_continue_stmt(ContinueStmtAST *stmt);
	void *visit_break_stmt(BreakStmtAST *stmt);
	void *visit_return_stmt(ReturnStmtAST *stmt);

	void *visit_function_definition(FunctionDefinitionAST *stmt);
	void *visit_declaration(DeclarationAST *stmt);

	void enter_scope();
	void leave_scope();
	bool add_type_declaration(std::shared_ptr<TypeAST> type);
	void define_type(std::string name, std::shared_ptr<TypeAST> type);
	void resolve_type(std::shared_ptr<TypeAST> type);

private:
	TranslationUnit translation_unit;
	TypeEnvironment *environment;
};

#endif
