#include "type_inference.h"

TranslationUnit SemanticTypeInference::analyze()
{
	for (auto decl : translation_unit.declarations)
		decl->accept(this);
	return translation_unit;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<int>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::int_).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::long_).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long long>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::long_long).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned int>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::unsigned_int).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned long>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::unsigned_long).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned long long>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::unsigned_long_long).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<float>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::float_).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<double>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::double_).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long double>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::long_double).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned char>* expr)
{
	return translation_unit.get_type(BuiltinTypeName::unsigned_char).get();
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<std::string>* expr)
{
	return translation_unit.get_type("const char *").get();
}

void* SemanticTypeInference::visit_identifier_expr(IdentifierExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_binary_expr(BinaryExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_unary_expr(UnaryExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_tenary_expr(TenaryExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_member_access_expr(MemberAccessExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_function_call_expr(FunctionCallExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_typecast_expr(TypeCastExprAST* expr)
{
	return expr->type.get();
}

void* SemanticTypeInference::visit_initializer_expr(InitializerExprAST* expr)
{
	throw std::runtime_error("not implemented");
}

void* SemanticTypeInference::visit_label_stmt(LabelStmtAST* stmt)
{
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_case_stmt(CaseStmtAST* stmt)
{
	stmt->constant->accept(this);
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_default_stmt(DefaultStmtAST* stmt)
{
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_expr_stmt(ExprStmtAST* stmt)
{
	stmt->expr->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_compound_stmt(CompoundStmtAST* stmt)
{
	for (auto s : stmt->stmts)
		s->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_if_stmt(IfStmtAST* stmt)
{
	stmt->cond->accept(this);
	stmt->if_stmt->accept(this);
	stmt->else_stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_switch_stmt(SwitchStmtAST* stmt)
{
	stmt->expr->accept(this);
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_for_stmt(ForStmtAST* stmt)
{
	stmt->init->accept(this);
	stmt->cond->accept(this);
	stmt->stmt->accept(this);
	stmt->inc->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_while_stmt(WhileStmtAST* stmt)
{
	stmt->cond->accept(this);
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_dowhile_stmt(DoWhileStmtAST* stmt)
{
	stmt->stmt->accept(this);
	stmt->cond->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_jump_stmt(JumpStmtAST* stmt)
{
	return nullptr;
}

void* SemanticTypeInference::visit_continue_stmt(ContinueStmtAST* stmt)
{
	return nullptr;
}

void* SemanticTypeInference::visit_break_stmt(BreakStmtAST* stmt)
{
	return nullptr;
}

void* SemanticTypeInference::visit_return_stmt(ReturnStmtAST* stmt)
{
	stmt->expr->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_function_definition(FunctionDefinitionAST* stmt)
{
	stmt->body->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_declaration(DeclarationAST* stmt)
{
	for (auto [name, type, expr] : stmt->declarators)
		expr->accept(this);
	return nullptr;
}
