#include "type_inference.h"

void SemanticTypeInference::enter_scope()
{
	environment = new TypeEnvironment(environment);
}

void SemanticTypeInference::leave_scope()
{
	environment = environment->get_enclosing();
}

std::shared_ptr<TypeAST> SemanticTypeInference::get_member_type(const std::shared_ptr<AggregateTypeAST>& type, std::string member_name)
{
	for (auto [mname, mtype] : type->members)
	{
		if (mname && mname->name == member_name)
			return mtype;
		else if (!mname && mtype->kind == TypeKind::aggregate)
		{
			auto rtype = get_member_type(std::static_pointer_cast<AggregateTypeAST>(mtype), member_name);
			if (rtype)
				return rtype;
		}
	}
	return nullptr;
}

TranslationUnit SemanticTypeInference::analyze()
{
	for (auto decl : translation_unit.declarations)
		decl->accept(this);
	return translation_unit;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<int>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::int_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::long_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long long>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::long_long);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned int>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_int);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned long>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_long);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned long long>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_long_long);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<float>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::float_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<double>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::double_);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<long double>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::long_double);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<unsigned char>* expr, void* data)
{
	expr->type = translation_unit.get_type(BuiltinTypeName::unsigned_char);
	return nullptr;
}

void* SemanticTypeInference::visit_literal_expr(LiteralExprAST<std::string>* expr, void* data)
{
	expr->type = translation_unit.get_type("const char *");
	return nullptr;
}

void* SemanticTypeInference::visit_identifier_expr(IdentifierExprAST* expr, void* data)
{
	expr->type = environment->get_declarator_type(expr->name);
	expr->name->name = environment->get_declarator_name(expr->name->name);
	return nullptr;
}

void* SemanticTypeInference::visit_binary_expr(BinaryExprAST* expr, void* data)
{
	expr->left->accept(this);
	expr->right->accept(this);

	auto expr1_type = translation_unit.get_type(expr->left->type);
	auto expr2_type = translation_unit.get_type(expr->right->type);
	std::shared_ptr<TypeAST> expr_type = nullptr;

	switch (expr->op)
	{
	case BinaryOperator::assignment:
		expr_type = expr1_type;
		break;

	case BinaryOperator::addition_assigment:
		assert((translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			   || (translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_pointer_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type)));
		expr_type = expr1_type;
		break;

	case BinaryOperator::subtraction_assignment:
		assert((translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			   || (translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_pointer_type(expr2_type))
			   || (translation_unit.is_pointer_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
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
	case BinaryOperator::subtraction:
		if (translation_unit.is_arithmetic_type(expr1_type) && translation_unit.is_arithmetic_type(expr2_type))
			expr_type = translation_unit.convert_arithmetic_type(expr1_type, expr2_type);
		else if (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type))
			expr_type = translation_unit.convert_array_to_pointer(expr1_type);
		else if (translation_unit.is_array_or_pointer_type(expr2_type) && translation_unit.is_integer_type(expr1_type))
			expr_type = translation_unit.convert_array_to_pointer(expr2_type);
		else if (expr->op == BinaryOperator::subtraction
				 && (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_array_or_pointer_type(expr2_type))
				 && translation_unit.is_compatible_types(expr1_type, expr2_type))
			expr_type = translation_unit.get_type("long");
		else if (expr->op == BinaryOperator::addition)
			throw std::runtime_error("addition only supports both arithmetic types or pointer/integer");
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
		expr_type = std::static_pointer_cast<BuiltinTypeAST>(translation_unit.promote_integer(expr1_type));
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
			   || (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_array_or_pointer_type(expr2_type)));
		expr_type = translation_unit.get_type("_Bool");
		break;

	case BinaryOperator::greater_than:
	case BinaryOperator::greater_or_equal:
	case BinaryOperator::less:
	case BinaryOperator::less_or_equal:
		assert((translation_unit.is_real_type(expr1_type) && translation_unit.is_real_type(expr2_type))
			   || (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_array_or_pointer_type(expr2_type)));
		expr_type = translation_unit.get_type("_Bool");
		break;

	case BinaryOperator::array_subscript:
	{
		std::shared_ptr<TypeAST> object_type = nullptr;
		if ((translation_unit.is_pointer_type(expr1_type) && translation_unit.is_integer_type(expr2_type)))
		{
			object_type = expr1_type;
			auto ptype = std::static_pointer_cast<PointerTypeAST>(expr1_type);
			expr_type = ptype->underlay;
		}
		else if (translation_unit.is_pointer_type(expr2_type) && translation_unit.is_integer_type(expr1_type))
		{
			object_type = expr2_type;
			auto ptype = std::static_pointer_cast<PointerTypeAST>(expr2_type);
			expr_type = ptype->underlay;
		}
		else if (translation_unit.is_array_type(expr1_type) && translation_unit.is_integer_type(expr2_type))
		{
			object_type = expr1_type;
			auto atype = std::static_pointer_cast<ArrayTypeAST>(expr1_type);
			expr_type = atype->underlay;
		}
		else if (translation_unit.is_array_type(expr2_type) && translation_unit.is_integer_type(expr1_type))
		{
			object_type = expr2_type;
			auto atype = std::static_pointer_cast<ArrayTypeAST>(expr2_type);
			expr_type = atype->underlay;
		}

		if (translation_unit.is_volatile_type(object_type))
			expr_type = translation_unit.duplicate_type_with_qualifier(expr_type, TypeQualifier::volatile_, DuplicateTypeAction::add);

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

void* SemanticTypeInference::visit_unary_expr(UnaryExprAST* expr, void* data)
{
	if (expr->expr)
		expr->expr->accept(this);

	std::shared_ptr<TypeAST> expr_type = nullptr;
	auto expr1_type = translation_unit.get_type(expr->expr->type);
	switch (expr->op)
	{
	case UnaryOperator::postfix_increment:
	case UnaryOperator::postfix_decrement:
	case UnaryOperator::prefix_increment:
	case UnaryOperator::prefix_decrement:
		if (translation_unit.is_arithmetic_type(expr1_type))
			expr_type = translation_unit.promote_integer(expr1_type);
		else if (translation_unit.is_pointer_type(expr1_type))
			expr_type = expr1_type;
		else
			throw std::runtime_error("only integer, real float or pointer type is supported in increment/decrement");
		break;

	case UnaryOperator::plus:
	case UnaryOperator::minus:
		assert(translation_unit.is_arithmetic_type(expr1_type));
		expr_type = translation_unit.promote_integer(expr1_type);
		break;

	case UnaryOperator::complement:
	case UnaryOperator::not_:
		assert(translation_unit.is_integer_type(expr1_type));
		expr_type = translation_unit.promote_integer(expr1_type);
		break;

	case UnaryOperator::dereference:
	{
		std::shared_ptr<TypeAST> element_type = nullptr;
		if (translation_unit.is_pointer_type(expr1_type))
			element_type = std::static_pointer_cast<PointerTypeAST>(expr1_type)->underlay;
		else
		{
			assert(translation_unit.is_array_type(expr1_type));
			element_type = std::static_pointer_cast<ArrayTypeAST>(expr1_type)->underlay;
		}

		expr_type = element_type;
		break;
	}

	case UnaryOperator::address_of:
		expr_type = std::make_shared<PointerTypeAST>(expr1_type);
		break;

	default:
		assert_not_reached();
	}

	if (!expr_type)
		throw std::runtime_error("cannot interfere type from unary expression");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_tenary_expr(TenaryExprAST* expr, void* data)
{
	expr->cond->accept(this);
	expr->expr1->accept(this);
	expr->expr2->accept(this);

	auto expr1_type = translation_unit.get_type(expr->expr1->type);
	auto expr2_type = translation_unit.get_type(expr->expr2->type);
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
		else if (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_null_pointer(expr2_type, expr->expr2))
			expr_type = translation_unit.convert_array_to_pointer(expr1_type);
		else if (translation_unit.is_array_or_pointer_type(expr2_type) && translation_unit.is_null_pointer(expr1_type, expr->expr1))
			expr_type = translation_unit.convert_array_to_pointer(expr2_type);
		else if (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_array_or_pointer_type(expr2_type) && translation_unit.is_compatible_types(expr1_type, expr2_type))
			expr_type = translation_unit.composite_type(expr1_type, expr2_type);
		else if ((translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_void_pointer(expr2_type))
				 || (translation_unit.is_array_or_pointer_type(expr2_type) && translation_unit.is_void_pointer(expr1_type)))
			expr_type = translation_unit.composite_type(expr1_type, expr2_type);
	}
	else
	{
		if (translation_unit.is_array_or_pointer_type(expr1_type) && translation_unit.is_null_pointer(expr2_type, expr->expr2))
			expr_type = translation_unit.convert_array_to_pointer(expr1_type);
		else if (translation_unit.is_array_or_pointer_type(expr2_type) && translation_unit.is_null_pointer(expr1_type, expr->expr1))
			expr_type = translation_unit.convert_array_to_pointer(expr2_type);
	}

	if (!expr_type)
		throw std::runtime_error("invalid type conversion");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_member_access_expr(MemberAccessExprAST* expr, void* data)
{
	expr->object->accept(this);

	std::shared_ptr<AggregateTypeAST> object_type = nullptr;
	if (translation_unit.is_aggregate_type(expr->object->type))
	{
		assert(expr->access_type == MemberAccessType::dot);
		object_type = std::static_pointer_cast<AggregateTypeAST>(translation_unit.get_type(expr->object->type));
	}
	else if (translation_unit.is_pointer_type(expr->object->type))
	{
		assert(expr->access_type == MemberAccessType::arrow);
		auto ptype = std::static_pointer_cast<PointerTypeAST>(translation_unit.get_type(expr->object->type));
		auto underlay_type = translation_unit.get_type(ptype->underlay);
		assert(underlay_type->kind == TypeKind::aggregate);
		object_type = std::static_pointer_cast<AggregateTypeAST>(underlay_type);
	}
	assert(object_type);

	std::shared_ptr<TypeAST> expr_type = get_member_type(object_type, expr->member->name);
	if (!expr_type)
		throw std::runtime_error(expr->member->name + " doesn't exist");
	// check directly from declarator's type since original type might not contain qualifiers
	// ```c
	//  struct foo { int a; }
	//  volatile struct foo x;
	// ```
	else if (translation_unit.is_volatile_type(expr->object->type))
		expr_type = translation_unit.duplicate_type_with_qualifier(expr_type, TypeQualifier::volatile_, DuplicateTypeAction::add);

	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_function_call_expr(FunctionCallExprAST* expr, void* data)
{
	expr->callee->accept(this);
	for (auto arg : expr->arguments)
		arg->accept(this);

	std::shared_ptr<TypeAST> expr_type = nullptr;
	if (translation_unit.is_function_type(expr->callee->type))
		expr_type = translation_unit.get_function_return_type(expr->callee->type);
	else if (translation_unit.is_pointer_type(expr->callee->type))
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(translation_unit.get_type(expr->callee->type));
		assert(translation_unit.is_function_type(ptype->underlay));
		expr_type = ptype->underlay;
	}

	if (!expr_type)
		throw std::runtime_error("function call type is not valid");
	expr->type = expr_type;
	return nullptr;
}

void* SemanticTypeInference::visit_typecast_expr(TypeCastExprAST* expr, void* data)
{
	resolve_type(expr->type);
	expr->expr->accept(this);

	if (expr->expr->node_type == ASTNodeType::expr_initializer)
		expr->expr->type = expr->type;

	if (translation_unit.is_volatile_type(expr->type) && !translation_unit.is_volatile_type(expr->expr->type))
		expr->type = translation_unit.duplicate_type_with_qualifier(expr->type, TypeQualifier::volatile_, DuplicateTypeAction::remove);

	return nullptr;
}

void* SemanticTypeInference::visit_sizeof_expr(SizeOfExprAST* expr, void* data)
{
	if (expr->expr)
		expr->expr->accept(this);

	expr->type = translation_unit.get_type("unsigned int");
	return nullptr;
}

void* SemanticTypeInference::visit_alignof_expr(AlignOfExprAST* expr, void* data)
{
	expr->type = translation_unit.get_type("unsigned int");
	return nullptr;
}

void* SemanticTypeInference::visit_initializer_expr(InitializerExprAST* expr, void* data)
{
	for (auto e : expr->exprs)
		e->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_label_stmt(LabelStmtAST* stmt, void* data)
{
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_case_stmt(CaseStmtAST* stmt, void* data)
{
	stmt->constant->accept(this);
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_default_stmt(DefaultStmtAST* stmt, void* data)
{
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_expr_stmt(ExprStmtAST* stmt, void* data)
{
	stmt->expr->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_compound_stmt(CompoundStmtAST* stmt, void* data)
{
	enter_scope();

	for (auto s : stmt->stmts)
		s->accept(this);

	leave_scope();
	return nullptr;
}

void* SemanticTypeInference::visit_if_stmt(IfStmtAST* stmt, void* data)
{
	stmt->cond->accept(this);
	stmt->if_stmt->accept(this);
	if (stmt->else_stmt)
		stmt->else_stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_switch_stmt(SwitchStmtAST* stmt, void* data)
{
	stmt->expr->accept(this);
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_for_stmt(ForStmtAST* stmt, void* data)
{
	if (stmt->init)
		stmt->init->accept(this);
	if (stmt->cond)
		stmt->cond->accept(this);

	stmt->stmt->accept(this);

	if (stmt->inc)
		stmt->inc->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_while_stmt(WhileStmtAST* stmt, void* data)
{
	stmt->cond->accept(this);
	stmt->stmt->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_dowhile_stmt(DoWhileStmtAST* stmt, void* data)
{
	stmt->stmt->accept(this);
	stmt->cond->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_jump_stmt(JumpStmtAST* stmt, void* data)
{
	return nullptr;
}

void* SemanticTypeInference::visit_continue_stmt(ContinueStmtAST* stmt, void* data)
{
	return nullptr;
}

void* SemanticTypeInference::visit_break_stmt(BreakStmtAST* stmt, void* data)
{
	return nullptr;
}

void* SemanticTypeInference::visit_return_stmt(ReturnStmtAST* stmt, void* data)
{
	if (stmt->expr)
		stmt->expr->accept(this);
	return nullptr;
}

void* SemanticTypeInference::visit_function_definition(FunctionDefinitionAST* stmt, void* data)
{
	auto ftype = std::static_pointer_cast<FunctionTypeAST>(stmt->type);
	add_type_declaration(ftype->returning, false);
	resolve_type(ftype);  // NOTE: MQ 2021-04-21 we don't support type definition in function parameters

	auto override = false;
	if (auto declarator_type = environment->get_declarator_type(stmt->name))
		override = translation_unit.is_function_type(declarator_type);
	environment->define_declarator_type(stmt->name, ftype, override);
	environment->define_declarator_name(stmt->name->name, stmt->name->name, override);

	enter_scope();
	in_func_scope = stmt;

	for (auto [pname, ptype] : ftype->parameters)
	{
		environment->define_declarator_type(pname, ptype);
		environment->define_declarator_name(pname->name, pname->name);
	}

	stmt->body->accept(this);

	in_func_scope = nullptr;
	leave_scope();
	return nullptr;
}

void* SemanticTypeInference::visit_declaration(DeclarationAST* stmt, void* data)
{
	auto type_defined = add_type_declaration(stmt->type, stmt->declarators.size() == 0);
	resolve_type(stmt->type);

	for (auto idx = 0; idx < stmt->declarators.size(); ++idx)
	{
		auto [name, type, expr] = stmt->declarators[idx];

		if (translation_unit.get_storage_specifier(type) == StorageSpecifier::typedef_)
		{
			if (!type_defined)
			{
				auto utype = translation_unit.unbox_type(type);
				type_defined = add_type_declaration(utype, false);
			}
			define_type(name->name, type, false, true);
		}
		else
		{
			auto declarator_type = environment->get_declarator_type(name, true);
			if (declarator_type)
			{
				if (in_func_scope)
					throw std::runtime_error("redefinition of " + name->name);
				else
				{
					if (!translation_unit.is_compatible_types(type, declarator_type))
						throw std::runtime_error("redefinition of " + name->name + " with a different type");
					type = translation_unit.composite_type(type, declarator_type);
					std::get<1>(stmt->declarators[idx]) = type;
					environment->define_declarator_type(name, type, true);
				}
			}
			else
			{
				environment->define_declarator_type(name, type);

				auto declarator_name = name->name;
				name->name = environment->generate_declarator_name(name, translation_unit.get_storage_specifier(type), in_func_scope);
				environment->define_declarator_name(declarator_name, name->name);
			}

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
bool SemanticTypeInference::add_type_declaration(std::shared_ptr<TypeAST>& type, bool is_forward)
{
	if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		if (atype->members.size() > 0)
		{
			if (atype->anonymous)
			{
				auto name = environment->contain_type_name(AGGREGATE_ANONYMOUS)
								? environment->generate_type_name(AGGREGATE_ANONYMOUS)
								: AGGREGATE_ANONYMOUS;
				atype->name = std::make_shared<TokenIdentifier>(name);
			}

			bool override = false;
			if (environment->contain_type_name(atype->name->name, true))
			{
				auto type = environment->get_type_type(atype->name);
				override = std::static_pointer_cast<AggregateTypeAST>(type)->members.size() == 0;
			}

			define_type(atype->name->name, type, override);
			for (auto [_, mtype] : atype->members)
				add_type_declaration(mtype, false);
			return true;
		}
		else if (is_forward)
			define_type(atype->name->name, type);
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto etype = std::static_pointer_cast<EnumTypeAST>(type);
		if (etype->members.size() > 0)
		{
			if (etype->anonymous)
			{
				auto name = environment->contain_type_name(AGGREGATE_ANONYMOUS)
								? environment->generate_type_name(AGGREGATE_ANONYMOUS)
								: AGGREGATE_ANONYMOUS;
				etype->name = std::make_shared<TokenIdentifier>(name);
			}

			bool override = false;
			if (environment->contain_type_name(etype->name->name, true))
			{
				auto type = environment->get_type_type(etype->name);
				override = std::static_pointer_cast<AggregateTypeAST>(type)->members.size() == 0;
			}

			define_type(etype->name->name, type, override);
			for (auto [mname, _] : etype->members)
			{
				environment->define_declarator_type(mname, translation_unit.get_type("int"));
				environment->define_declarator_name(mname->name, mname->name);
			}
			return true;
		}
		else if (is_forward)
			define_type(etype->name->name, type);
	}
	return false;
}

void SemanticTypeInference::define_type(std::string name, std::shared_ptr<TypeAST>& type, bool override, bool is_typedef)
{
	auto new_name = environment->contain_type_name(name) && !override ? environment->generate_type_name(name) : name;
	std::shared_ptr<TokenIdentifier> token_identifier;

	if (is_typedef)
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
	{
		environment->define_type_name(name, name);
		environment->define_type_type(name, type);
	}
	else
	{
		environment->define_type_name(name, new_name);
		environment->define_type_type(name, type);
		environment->define_type_name(new_name, new_name);
		environment->define_type_type(new_name, type);
	}
}

void SemanticTypeInference::resolve_type(std::shared_ptr<TypeAST> type)
{
	// don't need to resolve alias type since it is resolved at line of declaration
	if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		atype->name->name = environment->get_type_name(atype->name->name);

		for (auto [_, member_type] : atype->members)
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

void SemanticTypeInference::fill_initializer_type(std::shared_ptr<InitializerExprAST> expr, const std::shared_ptr<TypeAST>& type)
{
	assert(expr->node_type == ASTNodeType::expr_initializer);
	expr->type = type;

	if (translation_unit.is_aggregate_type(type))
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(translation_unit.get_type(type));
		for (auto idx = 0; idx < expr->exprs.size(); ++idx)
		{
			auto iexpr = expr->exprs[idx];
			auto [_, mtype] = atype->members[idx];
			if (!iexpr->type)
				fill_initializer_type(std::static_pointer_cast<InitializerExprAST>(iexpr), mtype);
		}
	}
	else if (translation_unit.is_array_type(type))
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(translation_unit.get_type(type));
		for (auto idx = 0; idx < expr->exprs.size(); ++idx)
		{
			auto iexpr = expr->exprs[idx];
			if (!iexpr->type)
				fill_initializer_type(std::static_pointer_cast<InitializerExprAST>(iexpr), atype->underlay);
		}
	}
}
