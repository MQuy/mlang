#include "type_inference.h"

void SemanticTypeInference::enter_scope()
{
	name_resolver = new TypeEnvironment(name_resolver);
}

void SemanticTypeInference::leave_scope()
{
	name_resolver = name_resolver->get_enclosing();
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
	resolve_type(expr->type);
	return nullptr;
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
	enter_scope();

	auto ftype = std::static_pointer_cast<FunctionTypeAST>(stmt->type);
	add_type_declaration(ftype->returning);
	resolve_type(ftype);  // NOTE: MQ 2021-04-21 we don't support type definition in function parameters

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
		else if (expr)
			expr->accept(this);
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
			return true;
		}
	}
	return false;
}

void SemanticTypeInference::define_type(std::string name, std::shared_ptr<TypeAST> type)
{
	auto new_name = name_resolver->contain_type_name(name) ? name_resolver->generate_type_name(name) : name;
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
		name_resolver->define(name, name);
	else
	{
		name_resolver->define(name, new_name);
		name_resolver->define(new_name, new_name);
	}
}

void SemanticTypeInference::resolve_type(std::shared_ptr<TypeAST> type)
{
	// don't need to resolve alias type since it is resolved at at line of declaration
	if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		atype->name->name = name_resolver->get_type_name(atype->name->name);

		for (auto [member_name, member_type] : atype->members)
			resolve_type(member_type);
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		atype->name->name = name_resolver->get_type_name(atype->name->name);
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

/*
ir.cpp#visit_declaration
- initialize type: create struct, union or enum if needed
- later, transversing declaration's type, we simply get from context
*/
