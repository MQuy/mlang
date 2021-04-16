#ifndef LLVM_IR_H
#define LLVM_IR_H 1

#include <memory>

#include "ast/parser.h"
#include "environment.h"
#include "flowable_stmt.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "translation_unit.h"

#define LLVM_RETURN_NAME "__ret"

class IR : NodeVisitor
{
public:
	IR(TranslationUnit translation_unit)
		: translation_unit(translation_unit)
		, environment(new Environment(nullptr))
		, in_func_scope(false)
	{
		context = std::make_unique<llvm::LLVMContext>();
		module = std::make_unique<llvm::Module>("mc", *context);
		builder = std::make_unique<llvm::IRBuilder<>>(*context);
		func_pass_manager = std::make_unique<llvm::legacy::FunctionPassManager>(module.get());
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

	void init_pass_maanger();
	llvm::Constant *cast_constant(llvm::Constant *source, llvm::Type *type);
	llvm::Value *create_bool_branch(llvm::Value *source, std::string name);
	llvm::Type *get_type(std::shared_ptr<TypeAST> type);
	llvm::AllocaInst *create_entry_block_alloca(llvm::Function *func, llvm::Type *type, llvm::StringRef name = "");
	llvm::GlobalValue::LinkageTypes get_linkage_type(StorageSpecifier storage);
	void complete_block(llvm::Function *func, std::shared_ptr<ASTNode> node, llvm::BasicBlock *nextbb);
	void branch_block(llvm::Function *func, std::shared_ptr<ASTNode> node, llvm::BasicBlock *truebb, llvm::BasicBlock *falsebb);
	void activate_block(llvm::Function *func, llvm::BasicBlock *endbb);
	std::shared_ptr<FlowableStmt> find_flowable_stmt(FlowableStmtType type);
	void unwind_flowable_stmt(std::shared_ptr<FlowableStmt> block, bool self_included = true);
	void enter_scope();
	void leave_scope();

private:
	TranslationUnit translation_unit;
	Environment *environment;
	std::unique_ptr<llvm::LLVMContext> context;
	std::unique_ptr<llvm::Module> module;
	std::unique_ptr<llvm::IRBuilder<>> builder;
	std::unique_ptr<llvm::legacy::FunctionPassManager> func_pass_manager;
	std::vector<std::shared_ptr<FlowableStmt>> flowable_stmts;
	bool in_func_scope;
};

#endif
