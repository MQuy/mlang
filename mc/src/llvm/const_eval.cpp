#include "const_eval.h"

#include "ir.h"

int ConstExprEval::eval()
{
	return (int)expr->accept(this);
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<int>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<long>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<long long>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned int>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned long>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned long long>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<float>* expr)
{
	return (void*)(int)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<double>* expr)
{
	return (void*)(int)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<long double>* expr)
{
	return (void*)(int)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned char>* expr)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<std::string>* expr)
{
	assert_not_reached();
}

void* ConstExprEval::visit_identifier_expr(IdentifierExprAST* expr)
{
	assert_not_reached();
}

void* ConstExprEval::visit_binary_expr(BinaryExprAST* expr)
{
	auto left = (int)expr->left->accept(this);
	auto right = (int)expr->right->accept(this);
	int result = 0;

	switch (expr->op)
	{
	case BinaryOperator::addition:
		result = left + right;
		break;

	case BinaryOperator::subtraction:
		result = left - right;
		break;

	case BinaryOperator::multiplication:
		result = left * right;
		break;

	case BinaryOperator::division:
		result = left / right;
		break;

	case BinaryOperator::remainder:
		result = left % right;
		break;

	case BinaryOperator::bitwise_and:
		result = left & right;
		break;

	case BinaryOperator::bitwise_or:
		result = left | right;
		break;

	case BinaryOperator::bitwise_xor:
		result = left ^ right;
		break;

	case BinaryOperator::shift_left:
		result = left << right;
		break;

	case BinaryOperator::shift_right:
		result = left >> right;
		break;

	case BinaryOperator::and_:
		result = left && right;
		break;

	case BinaryOperator::or_:
		result = left || right;
		break;

	case BinaryOperator::equal:
		result = left == right;
		break;

	case BinaryOperator::not_equal:
		result = left != right;
		break;

	case BinaryOperator::less:
		result = left < right;
		break;

	case BinaryOperator::greater_than:
		result = left > right;
		break;

	case BinaryOperator::less_or_equal:
		result = left <= right;
		break;

	case BinaryOperator::greater_or_equal:
		result = left >= right;
		break;

	default:
		assert_not_reached();
	}

	return (void*)result;
}

void* ConstExprEval::visit_unary_expr(UnaryExprAST* expr)
{
	auto expr1 = (int)expr->expr->accept(this);
	int result = 0;

	switch (expr->op)
	{
	case UnaryOperator::plus:
		result = +expr1;
		break;

	case UnaryOperator::minus:
		result = -expr1;
		break;

	case UnaryOperator::complement:
		result = ~expr1;
		break;

	case UnaryOperator::not_:
		result = !expr1;
		break;

	default:
		assert_not_reached();
	}

	return (void*)result;
}

void* ConstExprEval::visit_tenary_expr(TenaryExprAST* expr)
{
	return expr->cond->accept(this)
			   ? expr->expr1->accept(this)
			   : expr->expr2->accept(this);
}

void* ConstExprEval::visit_member_access_expr(MemberAccessExprAST* expr)
{
	assert_not_reached();
}

void* ConstExprEval::visit_function_call_expr(FunctionCallExprAST* expr)
{
	assert_not_reached();
}

void* ConstExprEval::visit_typecast_expr(TypeCastExprAST* expr)
{
	assert(translation_unit.is_integer_type(expr->type));
	return expr->expr->accept(this);
}

void* ConstExprEval::visit_sizeof_expr(SizeOfExprAST* expr)
{
	std::shared_ptr<TypeAST> type = expr->expr ? expr->expr->type : expr->size_of_type;
	return (void*)ir->get_sizeof_type(type);
}

void* ConstExprEval::visit_alignof_expr(AlignOfExprAST* expr)
{
	return (void*)ir->get_alignof_type(expr->align_of_type);
}

void* ConstExprEval::visit_initializer_expr(InitializerExprAST* expr)
{
	return expr->exprs.front()->accept(this);
}

void* ConstExprEval::visit_label_stmt(LabelStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_case_stmt(CaseStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_default_stmt(DefaultStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_expr_stmt(ExprStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_compound_stmt(CompoundStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_if_stmt(IfStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_switch_stmt(SwitchStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_for_stmt(ForStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_while_stmt(WhileStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_dowhile_stmt(DoWhileStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_jump_stmt(JumpStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_continue_stmt(ContinueStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_break_stmt(BreakStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_return_stmt(ReturnStmtAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_function_definition(FunctionDefinitionAST* stmt)
{
	assert_not_reached();
}

void* ConstExprEval::visit_declaration(DeclarationAST* stmt)
{
	assert_not_reached();
}
