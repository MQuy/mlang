#include "type_inference.h"

void SemanticTypeInference::enter_scope()
{
	environment = new TypeEnvironment(environment);
}

void SemanticTypeInference::leave_scope()
{
	environment = environment->get_enclosing();
}

TranslationUnit SemanticTypeInference::analyze()
{
	for (auto decl : translation_unit.declarations)
		decl->accept(this);
	return translation_unit;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<int>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::int_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::long_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long long>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::long_long);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned int>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_int);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned long>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_long);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned long long>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_long_long);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<float>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::float_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<double>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::double_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long double>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::long_double);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned char>* expr)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_char);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<std::string>* expr)
{
	expr->type = translation_unit.get_type("const char *");
	return nullptr;
}

void* SemanticTypeInference::visit_identifier_expr(IdentifierExprAST* expr)
{
	expr->type = environment->get_identifier_type(expr->name);
	return nullptr;
}

void* SemanticTypeInference::visit_binary_expr(BinaryExprAST* expr)
{
	expr->left->accept(this);
	expr->right->accept(this);

	auto expr1_type = expr->left->type;
	auto expr2_type = expr->right->type;
	std::shared_ptr<TypeAST> expr_type = nullptr;

	switch (expr->op)
	{
	case BinaryOperator::assignment:
		expr_type = expr1_type;
		break;

	case BinaryOperator::addition_assigment:
		assert((translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type)));
		expr_type = expr1_type;
		break;

	case BinaryOperator::subtraction_assignment:
		assert((translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type)
				   && translation_unit.is_pointer_type(expr2_type)
				   && translation_unit.is_compatible_types(expr1_type, expr2_type)));
		expr_type = expr1_type;
		break;

	case BinaryOperator::multiplication_assigment:
	case BinaryOperator::division_assignment:
	case BinaryOperator::remainder_assignment:
	case BinaryOperator::bitwise_and_assigment:
	case BinaryOperator::bitwise_or_assigment:
	case BinaryOperator::bitwise_xor_assigment:
	case BinaryOperator::shift_left_assignment:
	case BinaryOperator::shift_right_assignment:
		assert(translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type));
		expr_type = expr1_type;
		break;

	case BinaryOperator::addition:
		if (translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
		else if (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type))
			expr_type = expr1_type;
		else if (translation_unit.is_pointer_type(expr2_type) && translation_unit.is_integer_type(expr1_type))
			expr_type = expr2_type;
		else
			throw std::runtime_error("addition only supports both arithmetic types or pointer/integer");
		break;

	case BinaryOperator::subtraction:
		if (translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
		else if (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type))
			expr_type = expr1_type;
		else if (translation_unit.is_pointer_type(expr2_type) && translation_unit.is_integer_type(expr1_type))
			expr_type = expr2_type;
		else if (translation_unit.is_pointer_type(expr1_type)
				 && translation_unit.is_pointer_type(expr2_type)
				 && translation_unit.is_compatible_types(expr1_type, expr2_type))
			expr_type = translation_unit.get_type("long");
		else
			throw std::runtime_error("subtraction only supports both arithmetic types, both pointers or pointer/integer");
		break;

	case BinaryOperator::multiplication:
	case BinaryOperator::division:
		assert(translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type));
		expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
		break;

	case BinaryOperator::remainder:
		assert(translation_unit.is_integer_type(expr1_type) && translation_unit.is_integer_type(expr2_type));
		expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
		break;

	case BinaryOperator::bitwise_and:
	case BinaryOperator::bitwise_or:
	case BinaryOperator::bitwise_xor:
		assert(translation_unit.is_integer_type(expr1_type) && translation_unit.is_integer_type(expr2_type));
		expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
		break;

	case BinaryOperator::shift_left:
	case BinaryOperator::shift_right:
	{
		assert(translation_unit.is_integer_type(expr1_type) && translation_unit.is_integer_type(expr2_type));
		auto t1 = std::static_pointer_cast<BuiltinTypeAST>(translation_unit.promote_integer(expr1_type));
		auto t2 = std::static_pointer_cast<BuiltinTypeAST>(translation_unit.promote_integer(expr2_type));

		if (translation_unit.is_unsigned_integer_type(t1))
			expr_type = t1;
		else if (translation_unit.is_unsigned_integer_type(t2))
			expr_type = t2;
		else
			expr_type = type_nbits[t1->name] >= type_nbits[t2->name] ? t1 : t2;
		break;
	}

	case BinaryOperator::and_:
	case BinaryOperator::or_:
		assert(translation_unit.is_scalar_type(expr1_type) && translation_unit.is_scalar_type(expr2_type));
		expr_type = translation_unit.get_type("_Bool");
		break;

	case BinaryOperator::equal:
	case BinaryOperator::not_equal:
		assert((translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_pointer_type(expr2_type)));
		expr_type = translation_unit.get_type("_Bool");
		break;

	case BinaryOperator::greater_than:
	case BinaryOperator::greater_or_equal:
	case BinaryOperator::less:
	case BinaryOperator::less_or_equal:
		assert((translation_unit.is_real_type(expr1_type) && translation_unit.is_real_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_pointer_type(expr2_type)));
		expr_type = translation_unit.get_type("_Bool");
		break;

	case BinaryOperator::array_subscript:
	{
		std::shared_ptr<PointerTypeAST> ptype = nullptr;
		std::shared_ptr<BuiltinTypeAST> btype = nullptr;

		if ((translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type)))
		{
			ptype = std::static_pointer_cast<PointerTypeAST>(expr1_type);
			btype = std::static_pointer_cast<BuiltinTypeAST>(expr2_type);
		}
		else if (translation_unit.is_pointer_type(expr2_type) && translation_unit.is_integer_type(expr1_type))
		{
			ptype = std::static_pointer_cast<PointerTypeAST>(expr2_type);
			btype = std::static_pointer_cast<BuiltinTypeAST>(expr1_type);
		}

		assert(ptype && btype);
		expr_type = ptype->underlay;
		break;
	}

	case BinaryOperator::member_access:
	{
		assert(translation_unit.is_aggregate_type(expr1_type));
		auto atype = std::static_pointer_cast<AggregateTypeAST>(expr1_type);
		auto identifier = std::static_pointer_cast<IdentifierExprAST>(expr->right);

		for (auto [mname, mtype] : atype->members)
			if (mname->name == identifier->name->name)
			{
				expr1_type = mtype;
				break;
			}
		break;
	}

	case BinaryOperator::comma:
		expr_type = expr2_type;
		break;

	default:
		assert_not_reached();
	}

	if (!expr_type)
		throw std::runtime_error("cannot interfere type from binary expression");

	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_unary_expr(UnaryExprAST* expr)
{
	if (expr->expr)
		expr->expr->accept(this);

	std::shared_ptr<TypeAST> expr_type = nullptr;
	switch (expr->op)
	{
	case UnaryOperator::postfix_increment:
	case UnaryOperator::postfix_decrement:
	case UnaryOperator::prefix_increment:
	case UnaryOperator::prefix_decrement:
		if (translation_unit.is_arithmetic_type(expr->expr->type))
			expr_type = translation_unit.promote_integer(expr->expr->type);
		else if (translation_unit.is_pointer_type(expr->expr->type))
			expr_type = expr->expr->type;
		else
			throw std::runtime_error("only integer, real float or pointer type is supported in increment/decrement");
		break;

	case UnaryOperator::plus:
	case UnaryOperator::minus:
		assert(translation_unit.is_arithmetic_type(expr->expr->type));
		expr_type = translation_unit.promote_integer(expr->expr->type);
		break;

	case UnaryOperator::complement:
	case UnaryOperator::not_:
		assert(translation_unit.is_integer_type(expr->expr->type));
		expr_type = translation_unit.promote_integer(expr->expr->type);
		break;

	case UnaryOperator::dereference:
	{
		assert(translation_unit.is_pointer_type(expr->expr->type));
		auto ptype = std::static_pointer_cast<PointerTypeAST>(expr->expr->type);
		expr_type = ptype->underlay;
		break;
	}

	case UnaryOperator::address_of:
		expr_type = std::make_shared<PointerTypeAST>(expr->expr->type);
		break;

	default:
		assert_not_reached();
	}

	if (!expr_type)
		throw std::runtime_error("cannot interfere type from unary expression");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_tenary_expr(TenaryExprAST* expr)
{
	expr->cond->accept(this);
	expr->expr1->accept(this);
	expr->expr2->accept(this);

	auto expr1_type = expr->expr1->type;
	auto expr2_type = expr->expr2->type;
	std::shared_ptr<TypeAST> expr_type = nullptr;
	if (translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
		expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
	else if (expr1_type->kind == expr2_type->kind)
	{
		if (translation_unit.is_aggregate_type(expr1_type)
			&& std::static_pointer_cast<AggregateTypeAST>(expr1_type)->name->name
				   == std::static_pointer_cast<AggregateTypeAST>(expr2_type)->name->name)
			expr_type = expr1_type;
		else if (translation_unit.is_void_type(expr1_type) && translation_unit.is_void_type(expr2_type))
			expr_type = translation_unit.get_type("void");
		else if (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_null_pointer(expr2_type, expr->expr2))
			expr_type = expr1_type;
		else if (translation_unit.is_pointer_type(expr2_type) && translation_unit.is_null_pointer(expr1_type, expr->expr2))
			expr_type = expr2_type;
		else if (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_pointer_type(expr2_type) && translation_unit.is_compatible_types(expr1_type, expr2_type))
			expr_type = translation_unit.composite_type(expr1_type, expr2_type);
		else if ((translation_unit.is_pointer_type(expr1_type) && translation_unit.is_void_pointer(expr2_type))
				 || (translation_unit.is_pointer_type(expr2_type) && translation_unit.is_void_pointer(expr1_type)))
			expr_type = translation_unit.composite_type(expr1_type, expr2_type);
	}

	if (!expr_type)
		throw std::runtime_error("invalid type conversion");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_member_access_expr(MemberAccessExprAST* expr)
{
	expr->object->accept(this);

	std::shared_ptr<AggregateTypeAST> object_type = nullptr;
	if (translation_unit.is_aggregate_type(expr->object->type))
	{
		assert(expr->access_type == MemberAccessType::dot);
		object_type = std::static_pointer_cast<AggregateTypeAST>(expr->object->type);
	}
	else if (translation_unit.is_pointer_type(expr->object->type))
	{
		assert(expr->access_type == MemberAccessType::arrow);
		auto ptype = std::static_pointer_cast<PointerTypeAST>(expr->object->type);
		assert(translation_unit.is_aggregate_type(ptype->underlay));
		object_type = std::static_pointer_cast<AggregateTypeAST>(ptype->underlay);
	}
	assert(object_type);

	std::shared_ptr<TypeAST> expr_type = nullptr;
	for (auto [mname, mtype] : object_type->members)
	{
		if (expr->member->name == mname->name)
		{
			expr_type = mtype;
			break;
		}
	}

	if (!expr_type)
		throw std::runtime_error(expr->member->name + " doesn't exist");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_function_call_expr(FunctionCallExprAST* expr)
{
	expr->callee->accept(this);
	for (auto arg : expr->arguments)
		arg->accept(this);

	assert(translation_unit.is_function_type(expr->callee->type));
	std::shared_ptr<TypeAST> expr_type = translation_unit.get_function_return_type(expr->callee->type);
	if (!expr_type)
		throw std::runtime_error("function call type is not valid");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_typecast_expr(TypeCastExprAST* expr)
{
	resolve_type(expr->type);
	expr->expr->accept(this);

	if (expr->expr->node_type == ASTNodeType::expr_initializer)
		expr->expr->type = expr->type;

	return nullptr;
}

void* SemanticTypeInference::visit_sizeof_expr(SizeOfExprAST* expr)
{
	// at parsing phase, we cannot differentiate between identifier or alias type
	// if size of type exists but not alias -> it is a identifier
	if (expr->size_of_type && !translation_unit.get_type(expr->size_of_type))
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(expr->size_of_type);
		expr->expr = std::make_shared<IdentifierExprAST>(atype->name);
		expr->size_of_type = nullptr;
	}

	if (expr->expr)
		expr->expr->accept(this);

	expr->type = translation_unit.get_type("unsigned int");
	return nullptr;
}

void* SemanticTypeInference::visit_alignof_expr(AlignOfExprAST* expr)
{
	expr->type = translation_unit.get_type("unsigned int");
	return nullptr;
}

void* SemanticTypeInference::visit_initializer_expr(InitializerExprAST* expr)
{
	for (auto e : expr->exprs)
		e->accept(this);
	return nullptr;
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
	enter_scope();

	for (auto s : stmt->stmts)
		s->accept(this);

	leave_scope();
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
	auto ftype = std::static_pointer_cast<FunctionTypeAST>(stmt->type);
	add_type_declaration(ftype->returning);
	resolve_type(ftype);  // NOTE: MQ 2021-04-21 we don't support type definition in function parameters
	environment->define_variable(stmt->name, ftype);

	for (auto [pname, ptype] : ftype->parameters)
		environment->define_variable(pname, ptype);

	enter_scope();

	stmt->body->accept(this);

	leave_scope();
	return nullptr;
}

void* SemanticTypeInference::visit_declaration(DeclarationAST* stmt)
{
	auto type_defined = add_type_declaration(stmt->type);
	resolve_type(stmt->type);

	for (auto [name, type, expr] : stmt->declarators)
	{
		if (translation_unit.get_storage_specifier(type) == StorageSpecifier::typedef_)
		{
			if (!type_defined)
			{
				auto utype = translation_unit.unbox_type(type);
				type_defined = add_type_declaration(utype);
			}
			define_type(name->name, type);
		}
		else
		{
			environment->define_variable(name, type);
			if (expr)
			{
				expr->accept(this);
				// cannot inference initializer's type since its type have to depend on declaration
				if (expr->node_type == ASTNodeType::expr_initializer)
					fill_initializer_type(std::static_pointer_cast<InitializerExprAST>(expr), type);
			}
		}
		resolve_type(type);
	}

	return nullptr;
}

// only for struct, union and enum
bool SemanticTypeInference::add_type_declaration(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		if (atype->members.size() > 0)
		{
			define_type(atype->name->name, type);
			for (auto [mname, mtype] : atype->members)
				add_type_declaration(mtype);
			return true;
		}
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto atype = std::static_pointer_cast<EnumTypeAST>(type);
		if (atype->members.size() > 0)
		{
			define_type(atype->name->name, type);
			for (auto [mname, _] : atype->members)
				environment->define_variable(mname, translation_unit.get_type("int"));
			return true;
		}
	}
	return false;
}

void SemanticTypeInference::define_type(std::string name, std::shared_ptr<TypeAST> type)
{
	auto new_name = environment->contain_type_name(name) ? environment->generate_type_name(name) : name;
	std::shared_ptr<TokenIdentifier> token_identifier;

	if (translation_unit.get_storage_specifier(type) == StorageSpecifier::typedef_)
		translation_unit.add_type(new_name, type);
	else if (type->kind == TypeKind::aggregate)
	{
		token_identifier = std::static_pointer_cast<AggregateTypeAST>(type)->name;
		token_identifier->name = new_name;
		translation_unit.add_type(type);
	}
	else if (type->kind == TypeKind::enum_)
	{
		token_identifier = std::static_pointer_cast<EnumTypeAST>(type)->name;
		token_identifier->name = new_name;
		translation_unit.add_type(type);
	}
	else
		assert_not_reached();

	if (name == new_name)
		environment->define_type(name, name);
	else
	{
		environment->define_type(name, new_name);
		environment->define_type(new_name, new_name);
	}
}

void SemanticTypeInference::resolve_type(std::shared_ptr<TypeAST> type)
{
	// don't need to resolve alias type since it is resolved at at line of declaration
	if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		atype->name->name = environment->get_type_name(atype->name->name);

		for (auto [member_name, member_type] : atype->members)
			resolve_type(member_type);
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto atype = std::static_pointer_cast<EnumTypeAST>(type);
		atype->name->name = environment->get_type_name(atype->name->name);
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		resolve_type(atype->underlay);
	}
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		resolve_type(ptype->underlay);
	}
	else if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
		resolve_type(ftype->returning);
		for (auto [pname, ptype] : ftype->parameters)
			resolve_type(ptype);
	}
}

void SemanticTypeInference::fill_initializer_type(std::shared_ptr<InitializerExprAST> expr, std::shared_ptr<TypeAST> type)
{
	assert(expr->node_type == ASTNodeType::expr_initializer);
	expr->type = type;

	if (translation_unit.is_aggregate_type(type))
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		for (auto idx = 0; idx < expr->exprs.size(); ++idx)
		{
			auto iexpr = expr->exprs[idx];
			auto [_, mtype] = atype->members[idx];
			if (!iexpr->type)
				fill_initializer_type(std::static_pointer_cast<InitializerExprAST>(iexpr), mtype);
		}
	}
}
