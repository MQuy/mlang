#include "ir.h"

std::string IR::generate()
{
	for (auto declaration : program.declarations)
		declaration->accept(this);

	std::string str;
	llvm::raw_string_ostream ros(str);
	module->print(ros, nullptr);
	return ros.str();
}

void* IR::visit_literal_expr(LiteralExprAST<int>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, expr->value->value, true));
}

void* IR::visit_literal_expr(LiteralExprAST<long>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_LONG, expr->value->value, true));
}

void* IR::visit_literal_expr(LiteralExprAST<long long>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_LONG_LONG, expr->value->value, true));
}

void* IR::visit_literal_expr(LiteralExprAST<unsigned int>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<unsigned long>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_LONG, expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<unsigned long long>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_LONG_LONG, expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<float>* expr)
{
	return llvm::ConstantFP::get(*context, llvm::APFloat(expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<double>* expr)
{
	return llvm::ConstantFP::get(*context, llvm::APFloat(expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<long double>* expr)
{
	throw std::runtime_error("llvm does not support long double");
}

void* IR::visit_literal_expr(LiteralExprAST<unsigned char>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_CHAR, expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<std::string>* expr)
{
	return builder->CreateGlobalString(llvm::StringRef(expr->value->value));
}

void* IR::visit_identifier_expr(IdentifierExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_binary_expr(BinaryExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_unary_expr(UnaryExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_tenary_expr(TenaryExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_member_access_expr(MemberAccessExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_function_call_expr(FunctionCallExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_typecast_expr(TypeCastExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_initializer_expr(InitializerExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_label_stmt(LabelStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_case_stmt(CaseStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_default_stmt(DefaultStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_expr_stmt(ExprStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_compound_stmt(CompoundStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_if_stmt(IfStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_switch_stmt(SwitchStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_for_stmt(ForStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_while_stmt(WhileStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_dowhile_stmt(DoWhileStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_jump_stmt(JumpStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_continue_stmt(ContinueStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_break_stmt(BreakStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_return_stmt(ReturnStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_function_definition(FunctionDefinitionAST* stmt)
{
	in_func_scope = true;

	throw std::runtime_error("not implemented yet");

	in_func_scope = false;
}

void* IR::visit_declaration(DeclarationAST* stmt)
{
	for (auto [token, type, expr] : stmt->declarators)
	{
		if (!in_func_scope)
		{
			if (type->kind == TypeKind::builtin)
			{
				auto gvar = new llvm::GlobalVariable(*module, builder->getInt32Ty(), false, llvm::GlobalValue::LinkageTypes::ExternalLinkage, nullptr, token->name);
				if (expr)
				{
					auto value = (llvm::ConstantInt *)(expr->accept(this));
					gvar->setInitializer(value);
				}
			}
		}
	}
	return nullptr;
}
