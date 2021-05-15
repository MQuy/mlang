#ifndef LLVM_IR_H
#define LLVM_IR_H 1

#include <memory>

#include "ast/parser.h"
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
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include "semantic/translation_unit.h"
#include "stmt_branch.h"
#include "value_environment.h"

#define LLVM_RETURN_NAME "__ret"

class IR : NodeVisitor
{
public:
	IR(TranslationUnit translation_unit)
		: translation_unit(translation_unit)
		, environment(new ValueEnvironment(nullptr))
		, in_func_scope(nullptr)
	{
		context = std::make_unique<llvm::LLVMContext>();
		module = std::make_unique<llvm::Module>("mc", *context);
		builder = std::make_unique<llvm::IRBuilder<>>(*context);
		func_pass_manager = std::make_unique<llvm::legacy::FunctionPassManager>(module.get());
	}

	std::string generate(std::string output_path);

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
	void *visit_sizeof_expr(SizeOfExprAST *expr);
	void *visit_alignof_expr(AlignOfExprAST *expr);
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

	void emit_object_file(std::string path);
	void init_pass_maanger();
	llvm::Value *load_value(llvm::Value *source, std::shared_ptr<ExprAST> expr);
	llvm::Value *cast_value(llvm::Value *source, std::shared_ptr<TypeAST> src_type_ast, std::shared_ptr<TypeAST> dest_type_ast);
	llvm::Value *convert_to_bool(llvm::Value *source, std::string name);
	llvm::Type *get_type(std::shared_ptr<TypeAST> type_ast);
	llvm::AllocaInst *create_alloca(llvm::Function *func, llvm::Type *type, llvm::StringRef name = "");
	llvm::GlobalValue::LinkageTypes get_linkage_type(StorageSpecifier storage);
	std::vector<int> build_indices(std::shared_ptr<AggregateTypeAST> type_ast, std::string member_name);
	std::vector<llvm::Value *> get_indices(std::shared_ptr<AggregateTypeAST> type_ast, std::string member_name);
	void complete_block(llvm::Function *func, std::shared_ptr<ASTNode> node, llvm::BasicBlock *nextbb);
	void branch_block(llvm::Function *func, std::shared_ptr<ExprAST> expr, llvm::BasicBlock *truebb, llvm::BasicBlock *falsebb);
	void activate_block(llvm::Function *func, llvm::BasicBlock *endbb);
	std::shared_ptr<StmtBranch> find_stmt_branch(StmtBranchType type);
	void unwind_stmt_branch(std::shared_ptr<StmtBranch> block, bool self_included = true);
	void enter_scope();
	void leave_scope();
	void calculate_array_type_size(std::shared_ptr<TypeAST> type_ast, std::shared_ptr<ExprAST> expr);
	llvm::Value *execute_binop(BinaryOperator op, std::shared_ptr<TypeAST> type_ast, llvm::Value *left, llvm::Value *right);
	llvm::Function *create_function_prototype(std::string name, std::shared_ptr<TypeAST> type_ast);
	llvm::Value *get_or_insert_global_string(std::string content);
	unsigned get_sizeof_type(std::shared_ptr<TypeAST> type_ast);
	unsigned get_alignof_type(std::shared_ptr<TypeAST> type_ast);
	void store_inst(llvm::Value *dest, std::shared_ptr<TypeAST> dest_type, llvm::Value *src, std::shared_ptr<TypeAST> src_type);
	BinaryOperator convert_assignment_to_arithmetic_binop(BinaryOperator binop);
	std::string get_aggregate_name(std::shared_ptr<TypeAST> type_ast);
	llvm::Value *build_aggregate_accesses(llvm::Value *object, std::shared_ptr<AggregateTypeAST> type_ast, std::vector<int> indices);
	std::shared_ptr<TypeAST> get_largest_aggregate_member(std::shared_ptr<AggregateTypeAST> type_ast);
	llvm::Constant *get_null_value(llvm::Type *type);

private:
	TranslationUnit translation_unit;
	ValueEnvironment *environment;
	std::unique_ptr<llvm::LLVMContext> context;
	std::unique_ptr<llvm::Module> module;
	std::unique_ptr<llvm::IRBuilder<>> builder;
	std::unique_ptr<llvm::legacy::FunctionPassManager> func_pass_manager;
	std::vector<std::shared_ptr<StmtBranch>> stmts_branch;
	std::unordered_map<std::string, llvm::Value *> global_strings;	// use for string
	FunctionDefinitionAST *in_func_scope;
};

#endif
