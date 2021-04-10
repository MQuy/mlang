#include "ir.h"

#include "const_eval.h"

std::unordered_map<BuiltinTypeName, unsigned> scalar_type_nbits;

void init_types()
{
	scalar_type_nbits[BuiltinTypeName::char_] = NBITS_CHAR;
	scalar_type_nbits[BuiltinTypeName::unsigned_char] = NBITS_CHAR;
	scalar_type_nbits[BuiltinTypeName::signed_char] = NBITS_CHAR;
	scalar_type_nbits[BuiltinTypeName::short_] = NBITS_SHORT;
	scalar_type_nbits[BuiltinTypeName::unsigned_short] = NBITS_SHORT;
	scalar_type_nbits[BuiltinTypeName::int_] = NBITS_INT;
	scalar_type_nbits[BuiltinTypeName::unsigned_int] = NBITS_INT;
	scalar_type_nbits[BuiltinTypeName::long_] = NBITS_LONG;
	scalar_type_nbits[BuiltinTypeName::unsigned_long] = NBITS_LONG;
	scalar_type_nbits[BuiltinTypeName::long_long] = NBITS_LONG_LONG;
	scalar_type_nbits[BuiltinTypeName::unsigned_long_long] = NBITS_LONG_LONG;
}

/*
- type
  | == builtin -> get llvm type
  | == pointer
    - ntype = get_type(pointer->type)
    - return pointer of ntype
  | == array
    - evaluate array's expr
    - ntype = get_type(array->type)
    - return array of ntype
  | == alias -> return get_type(alias->type)
  | == aggregate(struct)
    - for each member -> get_type(member) and add it to array
    - return struct type
  | == aggregate(union)
    - for each member -> get_type(member)
    - return struct type with only one largest member size
  | == enum -> return int type;
  | == function
    - for each parameter -> get_type(parameter) and add it
    - rtype = get_type(function's returning)
    - return function type
*/
llvm::Type* IR::get_type(std::shared_ptr<TypeAST> type_ast, llvm::Type* base)
{
	llvm::Type* type = nullptr;
	if (type_ast->kind == TypeKind::builtin)
	{
		auto btype_ast = std::static_pointer_cast<BuiltinTypeAST>(type_ast);
		if (btype_ast->name == BuiltinTypeName::_Bool)
			type = builder->getInt1Ty();
		if (btype_ast->name == BuiltinTypeName::char_ || btype_ast->name == BuiltinTypeName::unsigned_char
			|| btype_ast->name == BuiltinTypeName::signed_char)
			type = builder->getInt8Ty();
		// at this stage, there is not be any incomplete type allowed
		// -> no standalone void and instead, void pointer which is treated at int8 pointer
		else if (btype_ast->name == BuiltinTypeName::void_)
			type = builder->getInt8Ty();
		else if (btype_ast->name == BuiltinTypeName::short_ || btype_ast->name == BuiltinTypeName::unsigned_short)
			type = builder->getInt16Ty();
		else if (btype_ast->name == BuiltinTypeName::int_ || btype_ast->name == BuiltinTypeName::unsigned_int
				 || btype_ast->name == BuiltinTypeName::long_ || btype_ast->name == BuiltinTypeName::unsigned_long)
			type = builder->getInt32Ty();
		else if (btype_ast->name == BuiltinTypeName::long_long || btype_ast->name == BuiltinTypeName::unsigned_long_long)
			type = builder->getInt64Ty();
		else if (btype_ast->name == BuiltinTypeName::float_)
			type = builder->getFloatTy();
		else if (btype_ast->name == BuiltinTypeName::double_)
			type = builder->getDoubleTy();
		else if (btype_ast->name == BuiltinTypeName::long_double)
			type = llvm::Type::getFP128Ty(*context);
	}
	else if (type_ast->kind == TypeKind::pointer)
	{
		auto ptype_ast = std::static_pointer_cast<PointerTypeAST>(type_ast);
		auto underlay_type = get_type(ptype_ast->underlay);
		type = llvm::PointerType::get(underlay_type, 0);
	}
	else if (type_ast->kind == TypeKind::array)
	{
		auto atype_ast = std::static_pointer_cast<ArrayTypeAST>(type_ast);
		auto element = get_type(atype_ast->underlay);
		auto number_of_elements = ConstExprEval(translation_unit, atype_ast->expr).eval();
		type = llvm::ArrayType::get(element, number_of_elements);
	}
	else if (type_ast->kind == TypeKind::alias)
	{
		auto atype_ast = std::static_pointer_cast<AliasTypeAST>(type_ast);
		auto stype_ast = translation_unit.types[atype_ast->name->name];
		type = get_type(stype_ast);
	}
	else if (type_ast->kind == TypeKind::aggregate)
	{
		auto atype_ast = std::static_pointer_cast<AggregateTypeAST>(type_ast);
		std::vector<llvm::Type*> members;
		for (auto [aname, atype_ast] : atype_ast->members)
		{
			auto atype = get_type(atype_ast);
			members.push_back(atype);
		}

		if (atype_ast->aggregate_kind == AggregateKind::union_)
		{
			llvm::Type* mtype;
			auto msize = 0;
			for (auto member : members)
			{
				auto nsize = module->getDataLayout().getTypeAllocSize(member);
				if (nsize > msize)
				{
					msize = nsize;
					mtype = member;
				}
			}
			type = llvm::StructType::create(*context, {mtype}, atype_ast->name->name);
		}
		else
			type = llvm::StructType::create(*context, members, atype_ast->name->name);
	}
	else if (type_ast->kind == TypeKind::function)
	{
		auto ftype_ast = std::static_pointer_cast<FunctionTypeAST>(type_ast);
		auto return_type = get_type(ftype_ast->returning);
		std::vector<llvm::Type*> parameters;
		for (auto [pname, ptype_ast] : ftype_ast->parameters)
		{
			auto ptype = get_type(ptype_ast);
			parameters.push_back(ptype);
		}
		type = llvm::FunctionType::get(return_type, parameters, false);
	}
	else if (type_ast->kind == TypeKind::enum_)
		type = builder->getInt32Ty();

	return type;
}

llvm::Constant* IR::cast_constant(llvm::Constant* source, llvm::Type* type, BuiltinTypeName type_name)
{
	auto source_tid = source->getType()->getTypeID();

	llvm::Instruction::CastOps inst;
	if (source_tid == llvm::Type::TypeID::IntegerTyID)
	{
		auto src_type = (llvm::IntegerType*)source->getType();
		auto value = ((llvm::ConstantInt*)source)->getValue();
		auto nbits = src_type->getBitWidth();

		auto is_signed = value.isSignBitSet();
		if (type_name == BuiltinTypeName::float_ || type_name == BuiltinTypeName::double_)
			inst = is_signed ? llvm::Instruction::SIToFP : llvm::Instruction::UIToFP;
		else if (nbits > scalar_type_nbits[type_name])
			inst = llvm::Instruction::Trunc;
		else
			inst = llvm::Instruction::ZExt;
	}
	else if (source_tid == llvm::Type::TypeID::FloatTyID)
	{
		if (type_name == BuiltinTypeName::float_)
			return source;
		else if (type_name == BuiltinTypeName::double_)
			inst = llvm::Instruction::FPExt;
		else if (type_name == BuiltinTypeName::int_)
			inst = llvm::Instruction::FPToSI;
		else if (type_name == BuiltinTypeName::unsigned_int)
			inst = llvm::Instruction::FPToUI;
	}
	else if (source_tid == llvm::Type::TypeID::DoubleTyID)
	{
		if (type_name == BuiltinTypeName::double_)
			return source;
		if (type_name == BuiltinTypeName::float_)
			inst = llvm::Instruction::FPTrunc;
		else if (type_name == BuiltinTypeName::int_)
			inst = llvm::Instruction::FPToSI;
		else if (type_name == BuiltinTypeName::unsigned_int)
			inst = llvm::Instruction::FPToUI;
	}
	else
		assert_not_reached();

	return (llvm::Constant*)builder->CreateCast(inst, source, type);
}

std::string IR::generate()
{
	for (auto declaration : translation_unit.declarations)
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
				auto builtin_type = std::static_pointer_cast<BuiltinTypeAST>(type);
				bool is_constant = std::any_of(builtin_type->qualifiers.begin(), builtin_type->qualifiers.end(), [](TypeQualifier qualifier) {
					return qualifier == TypeQualifier::const_;
				});
				llvm::GlobalValue::LinkageTypes linkage = builtin_type->storage == StorageSpecifier::static_
															  ? llvm::GlobalValue::LinkageTypes::InternalLinkage
															  : llvm::GlobalValue::LinkageTypes::ExternalLinkage;
				llvm::Type* ty = get_type(builtin_type);
				llvm::Constant* value = nullptr;
				if (expr)
					value = cast_constant((llvm::Constant*)expr->accept(this), ty, builtin_type->name);
				new llvm::GlobalVariable(*module, ty, is_constant, linkage, value, token->name);
			}
		}
	}
	return nullptr;
}
