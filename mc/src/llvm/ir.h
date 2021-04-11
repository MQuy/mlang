#ifndef LLVM_IR_H
#define LLVM_IR_H 1

#include <memory>

#include "ast/parser.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

class IR : ExprVisitor, StmtVisitor
{
public:
	IR(TranslationUnit translation_unit)
		: translation_unit(translation_unit)
		, in_func_scope(false)
	{
		context = std::make_unique<llvm::LLVMContext>();
		module = std::make_unique<llvm::Module>("mc", *context);
		builder = std::make_unique<llvm::IRBuilder<>>(*context);
	}

	std::string generate();

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

	llvm::Constant *cast_constant(llvm::Constant *source, llvm::Type *type);
	llvm::Type *get_type(std::shared_ptr<TypeAST> type);
	llvm::AllocaInst *create_entry_block_alloca(llvm::Function *func, llvm::Type *type, llvm::StringRef name);
	llvm::GlobalValue::LinkageTypes get_linkage_type(StorageSpecifier storage);

private:
	TranslationUnit translation_unit;
	std::unique_ptr<llvm::LLVMContext> context;
	std::unique_ptr<llvm::Module> module;
	std::unique_ptr<llvm::IRBuilder<>> builder;
	bool in_func_scope;
};

#endif
