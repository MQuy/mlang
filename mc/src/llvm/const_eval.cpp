#include "const_eval.h"

#include "ir.h"

int ConstExprEval::eval()
{
	return (int)expr->accept(this);
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<int>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<long>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<long long>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned int>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned long>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned long long>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<float>* expr, void* data)
{
	return (void*)(int)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<double>* expr, void* data)
{
	return (void*)(int)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<long double>* expr, void* data)
{
	return (void*)(int)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<unsigned char>* expr, void* data)
{
	return (void*)expr->value->value;
}

void* ConstExprEval::visit_literal_expr(LiteralExprAST<std::string>* expr, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_identifier_expr(IdentifierExprAST* expr, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_binary_expr(BinaryExprAST* expr, void* data)
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

void* ConstExprEval::visit_unary_expr(UnaryExprAST* expr, void* data)
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

void* ConstExprEval::visit_tenary_expr(TenaryExprAST* expr, void* data)
{
	return expr->cond->accept(this)
			   ? expr->expr1->accept(this)
			   : expr->expr2->accept(this);
}

void* ConstExprEval::visit_member_access_expr(MemberAccessExprAST* expr, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_function_call_expr(FunctionCallExprAST* expr, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_typecast_expr(TypeCastExprAST* expr, void* data)
{
	assert(translation_unit.is_integer_type(expr->type));
	return expr->expr->accept(this);
}

void* ConstExprEval::visit_sizeof_expr(SizeOfExprAST* expr, void* data)
{
	std::shared_ptr<TypeAST> type = expr->expr ? expr->expr->type : expr->size_of_type;
	return (void*)ir->get_sizeof_type(type);
}

void* ConstExprEval::visit_alignof_expr(AlignOfExprAST* expr, void* data)
{
	return (void*)ir->get_alignof_type(expr->align_of_type);
}

void* ConstExprEval::visit_initializer_expr(InitializerExprAST* expr, void* data)
{
	return expr->exprs.front()->accept(this);
}

void* ConstExprEval::visit_label_stmt(LabelStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_case_stmt(CaseStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_default_stmt(DefaultStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_expr_stmt(ExprStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_compound_stmt(CompoundStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_if_stmt(IfStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_switch_stmt(SwitchStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_for_stmt(ForStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_while_stmt(WhileStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_dowhile_stmt(DoWhileStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_jump_stmt(JumpStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_continue_stmt(ContinueStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_break_stmt(BreakStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_return_stmt(ReturnStmtAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_function_definition(FunctionDefinitionAST* stmt, void* data)
{
	assert_not_reached();
}

void* ConstExprEval::visit_declaration(DeclarationAST* stmt, void* data)
{
	assert_not_reached();
}
