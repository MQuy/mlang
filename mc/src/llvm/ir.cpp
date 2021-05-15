#include "ir.h"

#include "const_eval.h"
#include "passes/unreachable_block_instruction_pass.h"

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
llvm::Type* IR::get_type(std::shared_ptr<TypeAST> type_ast)
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
		else if (btype_ast->name == BuiltinTypeName::void_)
			type = builder->getVoidTy();
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
		if (translation_unit.is_void_type(ptype_ast->underlay))
			return builder->getInt8PtrTy();
		else
		{
			auto underlay_type = get_type(ptype_ast->underlay);
			type = llvm::PointerType::get(underlay_type, 0);
		}
	}
	else if (type_ast->kind == TypeKind::array)
	{
		auto atype_ast = std::static_pointer_cast<ArrayTypeAST>(type_ast);
		auto element = get_type(atype_ast->underlay);
		auto number_of_elements = ConstExprEval(this, atype_ast->expr).eval();
		type = llvm::ArrayType::get(element, number_of_elements);
	}
	else if (type_ast->kind == TypeKind::alias)
	{
		auto stype_ast = translation_unit.get_type(type_ast);
		type = get_type(stype_ast);
	}
	else if (type_ast->kind == TypeKind::aggregate)
	{
		auto atype_ast = std::static_pointer_cast<AggregateTypeAST>(type_ast);
		auto type_name = get_aggregate_name(type_ast);
		if (auto ty = module->getTypeByName(type_name))
			type = ty;
		else
		{
			std::vector<llvm::Type*> members;
			for (auto [aname, atype_ast] : atype_ast->members)
			{
				auto atype = get_type(atype_ast);
				members.push_back(atype);
			}

			if (atype_ast->aggregate_kind == AggregateKind::union_)
			{
				llvm::Type* mtype = nullptr;
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
				type = llvm::StructType::create(*context, {mtype}, type_name);
			}
			else
				type = llvm::StructType::create(*context, members, type_name);
		}
	}
	else if (type_ast->kind == TypeKind::function)
	{
		auto ftype_ast = std::static_pointer_cast<FunctionTypeAST>(type_ast);
		std::vector<llvm::Type*> parameters;
		for (auto [pname, ptype_ast] : ftype_ast->parameters)
		{
			auto ptype = get_type(ptype_ast);

			if (translation_unit.is_aggregate_type(ptype_ast))
				ptype = llvm::PointerType::get(ptype, 0);

			parameters.push_back(ptype);
		}

		llvm::Type* return_type = nullptr;
		if (translation_unit.is_aggregate_type(ftype_ast->returning))
		{
			return_type = builder->getVoidTy();
			parameters.insert(parameters.begin(), 1, llvm::PointerType::get(get_type(ftype_ast->returning), 0));
		}
		else
			return_type = get_type(ftype_ast->returning);

		type = llvm::FunctionType::get(return_type, parameters, ftype_ast->is_variadic_args);
	}
	else if (type_ast->kind == TypeKind::enum_)
	{
		auto etype_ast = std::static_pointer_cast<EnumTypeAST>(type_ast);
		auto prev_constant = llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, -1, true));
		for (auto [mname, mexpr] : etype_ast->members)
		{
			llvm::ConstantInt* mvalue = nullptr;

			if (mexpr)
				mvalue = (llvm::ConstantInt*)mexpr->accept(this);
			else
				mvalue = llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, prev_constant->getValue().getSExtValue() + 1, true));

			prev_constant = mvalue;
			environment->define(mname, mvalue);
		}

		type = builder->getInt32Ty();
	}
	else
		assert_not_reached();

	return type;
}

llvm::Constant* IR::get_null_value(llvm::Type* type)
{
	switch (type->getTypeID())
	{
	case llvm::Type::IntegerTyID:
		return llvm::ConstantInt::get(type, 0);
	case llvm::Type::HalfTyID:
		return llvm::ConstantFP::get(type->getContext(),
									 llvm::APFloat::getZero(llvm::APFloat::IEEEhalf()));
	case llvm::Type::FloatTyID:
		return llvm::ConstantFP::get(type->getContext(),
									 llvm::APFloat::getZero(llvm::APFloat::IEEEsingle()));
	case llvm::Type::DoubleTyID:
		return llvm::ConstantFP::get(type->getContext(),
									 llvm::APFloat::getZero(llvm::APFloat::IEEEdouble()));
	case llvm::Type::FP128TyID:
		return llvm::ConstantFP::get(type->getContext(),
									 llvm::APFloat::getZero(llvm::APFloat::IEEEquad()));
	case llvm::Type::PointerTyID:
		return llvm::ConstantPointerNull::get((llvm::PointerType*)type);
	case llvm::Type::StructTyID:
	case llvm::Type::ArrayTyID:
		return llvm::ConstantAggregateZero::get(type);
	default:
		assert_not_reached();
	}
}

llvm::GlobalValue::LinkageTypes IR::get_linkage_type(StorageSpecifier storage)
{
	auto linkage = storage == StorageSpecifier::static_
					   ? llvm::GlobalValue::LinkageTypes::InternalLinkage
					   : llvm::GlobalValue::LinkageTypes::ExternalLinkage;
	return linkage;
}

std::vector<int> IR::build_indices(std::shared_ptr<AggregateTypeAST> type_ast, std::string member_name)
{
	for (auto idx = 0; idx < type_ast->members.size(); ++idx)
	{
		auto [mname, mtype] = type_ast->members[idx];
		if (mname && mname->name == member_name)
			return std::vector<int>(1, idx);
		else if (!mname && mtype->kind == TypeKind::aggregate)
		{
			auto indices = build_indices(std::static_pointer_cast<AggregateTypeAST>(mtype), member_name);
			if (indices.size() > 0)
			{
				indices.insert(std::begin(indices), idx);
				return indices;
			}
		}
	}
	return std::vector<int>();
}

std::vector<llvm::Value*> IR::get_indices(std::shared_ptr<AggregateTypeAST> type_ast, std::string member_name)
{
	std::vector<llvm::Value*> indices = {llvm::ConstantInt::get(builder->getInt32Ty(), 0)};
	auto idxs = build_indices(type_ast, member_name);
	for (auto idx : idxs)
		indices.push_back(llvm::ConstantInt::get(builder->getInt32Ty(), idx));
	return indices;
}

void IR::complete_block(llvm::Function* func, std::shared_ptr<ASTNode> node, llvm::BasicBlock* nextbb)
{
	node->accept(this);
	builder->CreateBr(nextbb);
}

void IR::branch_block(llvm::Function* func, std::shared_ptr<ExprAST> expr, llvm::BasicBlock* truebb, llvm::BasicBlock* falsebb)
{
	auto cond = (llvm::Value*)expr->accept(this);
	auto rvalue_cond = load_value(cond, expr);
	auto bool_cond = convert_to_bool(rvalue_cond, "cond");
	builder->CreateCondBr(bool_cond, truebb, falsebb);
}

void IR::activate_block(llvm::Function* func, llvm::BasicBlock* endbb)
{
	func->getBasicBlockList().push_back(endbb);
	builder->SetInsertPoint(endbb);
}

std::shared_ptr<StmtBranch> IR::find_stmt_branch(StmtBranchType type)
{
	for (auto block = stmts_branch.rbegin(); block != stmts_branch.rend(); ++block)
		if ((*block)->type == type)
			return *block;
	return nullptr;
}

void IR::unwind_stmt_branch(std::shared_ptr<StmtBranch> target, bool self_included)
{
	auto iterator = std::find(stmts_branch.begin(), stmts_branch.end(), target);

	if (iterator == stmts_branch.end())
		return;

	stmts_branch.erase(self_included ? iterator : iterator++, stmts_branch.end());
}

llvm::Value* IR::convert_to_bool(llvm::Value* source, std::string name)
{
	auto type = source->getType();
	if (type->isIntegerTy())
		return builder->CreateICmpNE(source, get_null_value(type), name);
	else if (type->isFloatTy())
		return builder->CreateFCmpONE(source, get_null_value(type), name);
	else if (type->isPointerTy())
		return builder->CreateIsNull(source);
	else
		throw std::runtime_error("Only support scalar types in branch condtion");
}

void IR::enter_scope()
{
	environment = new ValueEnvironment(environment);
}

void IR::leave_scope()
{
	environment = environment->get_enclosing();
}

void IR::calculate_array_type_size(std::shared_ptr<TypeAST> type_ast, std::shared_ptr<ExprAST> expr)
{
	assert(type_ast->kind == TypeKind::array);

	auto atype_ast = std::static_pointer_cast<ArrayTypeAST>(type_ast);

	if (expr->node_type == ASTNodeType::expr_sizeof)
		expr = std::static_pointer_cast<SizeOfExprAST>(expr)->expr;

	if (!atype_ast->expr && expr->node_type == ASTNodeType::expr_initializer)
	{
		auto initializer = std::static_pointer_cast<InitializerExprAST>(expr);
		auto size = initializer->exprs.size();
		auto literal = std::make_shared<TokenLiteral<int>>(size, std::to_string(size));
		atype_ast->expr = std::make_shared<LiteralExprAST<int>>(LiteralExprAST<int>(literal));
	}
}

llvm::Value* IR::execute_binop(BinaryOperator op, std::shared_ptr<TypeAST> type_ast, llvm::Value* left, llvm::Value* right)
{
	llvm::Value* result = nullptr;

	if (op == BinaryOperator::addition)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = builder->CreateAdd(left, right, "", false, translation_unit.is_signed_integer_type(type_ast));
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFAdd(left, right);
		else if (translation_unit.is_pointer_type(type_ast))
		{
			llvm::ArrayRef<llvm::Value*> indices = {(llvm::Constant*)right};
			result = builder->CreateInBoundsGEP(left, indices);
		}
	}
	else if (op == BinaryOperator::subtraction)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = builder->CreateSub(left, right, "", false, translation_unit.is_signed_integer_type(type_ast));
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFSub(left, right);
		else if (translation_unit.is_pointer_type(type_ast))
		{
			llvm::ConstantInt* constant = (llvm::ConstantInt*)right;
			auto value = constant->getValue().getSExtValue() * -1;
			llvm::ArrayRef<llvm::Value*> indices = {llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, value, true))};
			result = builder->CreateInBoundsGEP(left, indices);
		}
	}
	else if (op == BinaryOperator::multiplication)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = builder->CreateMul(left, right);
		else
			result = builder->CreateFMul(left, right);
	}
	else if (op == BinaryOperator::division)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = translation_unit.is_unsigned_integer_type(type_ast)
						 ? builder->CreateUDiv(left, right)
						 : builder->CreateSDiv(left, right);
		else
			result = builder->CreateFDiv(left, right);
	}
	else if (op == BinaryOperator::remainder)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = translation_unit.is_unsigned_integer_type(type_ast)
						 ? builder->CreateURem(left, right)
						 : builder->CreateSRem(left, right);
		else
			result = builder->CreateFRem(left, right);
	}
	else if (op == BinaryOperator::bitwise_and)
		result = builder->CreateAnd(left, right);
	else if (op == BinaryOperator::bitwise_or)
		result = builder->CreateOr(left, right);
	else if (op == BinaryOperator::bitwise_xor)
		result = builder->CreateXor(left, right);
	else if (op == BinaryOperator::shift_left)
		result = builder->CreateShl(left, right);
	else if (op == BinaryOperator::shift_right)
		result = translation_unit.is_signed_integer_type(type_ast)
					 ? builder->CreateAShr(left, right)
					 : builder->CreateLShr(left, right);
	else if (op == BinaryOperator::equal)
	{
		if (translation_unit.is_integer_type(type_ast) || translation_unit.is_pointer_type(type_ast))
			result = builder->CreateICmpEQ(left, right);
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFCmpUEQ(left, right);
	}
	else if (op == BinaryOperator::not_equal)
	{
		if (translation_unit.is_integer_type(type_ast) || translation_unit.is_pointer_type(type_ast))
			result = builder->CreateICmpNE(left, right);
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFCmpONE(left, right);
	}
	else if (op == BinaryOperator::less)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = translation_unit.is_signed_integer_type(type_ast)
						 ? builder->CreateICmpSLT(left, right)
						 : builder->CreateICmpULT(left, right);
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFCmpOLT(left, right);
	}
	else if (op == BinaryOperator::greater_than)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = translation_unit.is_signed_integer_type(type_ast)
						 ? builder->CreateICmpSGT(left, right)
						 : builder->CreateICmpUGT(left, right);
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFCmpOGT(left, right);
	}
	else if (op == BinaryOperator::less_or_equal)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = translation_unit.is_signed_integer_type(type_ast)
						 ? builder->CreateICmpSLE(left, right)
						 : builder->CreateICmpULE(left, right);
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFCmpOLE(left, right);
	}
	else if (op == BinaryOperator::greater_or_equal)
	{
		if (translation_unit.is_integer_type(type_ast))
			result = translation_unit.is_signed_integer_type(type_ast)
						 ? builder->CreateICmpSGE(left, right)
						 : builder->CreateICmpUGE(left, right);
		else if (translation_unit.is_real_float_type(type_ast))
			result = builder->CreateFCmpOGE(left, right);
	}

	assert(result);
	return result;
}

llvm::Function* IR::create_function_prototype(std::string name, std::shared_ptr<TypeAST> type)
{
	auto ftype_ast = std::static_pointer_cast<FunctionTypeAST>(type);
	llvm::FunctionType* ftype = (llvm::FunctionType*)get_type(ftype_ast);

	auto storage = translation_unit.get_storage_specifier(ftype_ast->returning);
	auto linkage = get_linkage_type(storage);
	auto func = llvm::Function::Create(ftype, linkage, name, &*module);
	environment->define(name, func);

	return func;
}

llvm::Value* IR::get_or_insert_global_string(std::string content)
{
	llvm::Value* value = global_strings[content];
	if (!value)
		global_strings[content] = value = builder->CreateGlobalStringPtr(content);

	return value;
}

unsigned IR::get_alignof_type(std::shared_ptr<TypeAST> type_ast)
{
	auto type = get_type(type_ast);
	return module->getDataLayout().getABITypeAlignment(type);
}

void IR::store_inst(llvm::Value* dest, std::shared_ptr<TypeAST> dest_type_ast, llvm::Value* src, std::shared_ptr<TypeAST> src_type_ast)
{
	auto src_type = get_type(src_type_ast);

	if (translation_unit.is_array_type(dest_type_ast) && translation_unit.is_pointer_type(src_type_ast))
	{
		auto dest_atype_ast = std::static_pointer_cast<ArrayTypeAST>(dest_type_ast);
		auto dest_ptype_ast = translation_unit.convert_array_to_pointer(dest_type_ast);
		auto dest_type = get_type(dest_ptype_ast);
		auto casted_dest = builder->CreateBitCast(dest, dest_type);
		auto size = ConstExprEval(this, dest_atype_ast->expr).eval();
		builder->CreateMemCpy(casted_dest,
							  llvm::MaybeAlign(get_alignof_type(dest_type_ast)),
							  src,
							  llvm::MaybeAlign(get_alignof_type(src_type_ast)),
							  size);
	}
	else if (translation_unit.is_aggregate_type(dest_type_ast)
			 && translation_unit.is_aggregate_type(src_type_ast)
			 && translation_unit.is_compatible_types(dest_type_ast, src_type_ast))
	{
		auto size = get_sizeof_type(dest_type_ast);
		auto type = llvm::PointerType::get(builder->getInt8Ty(), 0);
		auto casted_src = builder->CreateBitCast(src, type);
		auto casted_dest = builder->CreateBitCast(dest, type);
		builder->CreateMemCpy(casted_dest,
							  llvm::MaybeAlign(get_alignof_type(dest_type_ast)),
							  casted_src,
							  llvm::MaybeAlign(get_alignof_type(src_type_ast)),
							  size);
	}
	else
	{
		auto cvalue = cast_value(src, src_type_ast, dest_type_ast);
		builder->CreateStore(cvalue, dest);
	}
}

BinaryOperator IR::convert_assignment_to_arithmetic_binop(BinaryOperator binop)
{
	if (binop == BinaryOperator::addition_assigment)
		return BinaryOperator::addition;
	else if (binop == BinaryOperator::subtraction_assignment)
		return BinaryOperator::subtraction;
	else if (binop == BinaryOperator::multiplication_assigment)
		return BinaryOperator::multiplication;
	else if (binop == BinaryOperator::division_assignment)
		return BinaryOperator::division;
	else if (binop == BinaryOperator::remainder_assignment)
		return BinaryOperator::remainder;
	else if (binop == BinaryOperator::bitwise_and_assigment)
		return BinaryOperator::bitwise_and;
	else if (binop == BinaryOperator::bitwise_or_assigment)
		return BinaryOperator::bitwise_or;
	else if (binop == BinaryOperator::bitwise_xor_assigment)
		return BinaryOperator::bitwise_xor;
	else if (binop == BinaryOperator::shift_left_assignment)
		return BinaryOperator::shift_left;
	else if (binop == BinaryOperator::shift_right_assignment)
		return BinaryOperator::shift_right;

	assert_not_reached();
}

std::string IR::get_aggregate_name(std::shared_ptr<TypeAST> type)
{
	assert(type->kind == TypeKind::aggregate);
	auto atype_ast = std::static_pointer_cast<AggregateTypeAST>(type);
	auto prefix = atype_ast->aggregate_kind == AggregateKind::struct_ ? "struct." : "union.";
	return prefix + atype_ast->name->name;
}

llvm::Value* IR::build_aggregate_accesses(llvm::Value* object, std::shared_ptr<AggregateTypeAST> type_ast, std::vector<int> indices)
{
	auto idx = indices.front();
	indices.erase(indices.begin());

	llvm::Value* object_member = nullptr;
	std::shared_ptr<TypeAST> member_type = nullptr;
	if (type_ast->aggregate_kind == AggregateKind::struct_)
	{
		member_type = std::get<1>(type_ast->members[idx]);
		object_member = builder->CreateGEP(object, llvm::ConstantInt::get(builder->getInt32Ty(), idx));
	}
	else
	{
		assert(type_ast->aggregate_kind == AggregateKind::union_);
		member_type = std::get<1>(type_ast->members[idx]);

		auto pmember_type = std::make_shared<PointerTypeAST>(member_type);
		auto ptype = std::make_shared<PointerTypeAST>(type_ast);
		object_member = cast_value(object, ptype, pmember_type);
	}

	if (indices.size() > 0)
		return build_aggregate_accesses(object_member, std::static_pointer_cast<AggregateTypeAST>(member_type), indices);
	else
		return object_member;
}

std::shared_ptr<TypeAST> IR::get_largest_aggregate_member(std::shared_ptr<AggregateTypeAST> type_ast)
{
	std::shared_ptr<TypeAST> largest_member_type = nullptr;
	for (auto idx = 0, msize = 0; idx < type_ast->members.size(); ++idx)
	{
		auto [_, member_type] = type_ast->members[idx];
		auto atype = get_type(member_type);
		auto nsize = module->getDataLayout().getTypeAllocSize(atype);

		if (nsize > msize)
		{
			msize = nsize;
			largest_member_type = member_type;
		}
	}
	return largest_member_type;
}

unsigned IR::get_sizeof_type(std::shared_ptr<TypeAST> type_ast)
{
	auto type = get_type(type_ast);
	return module->getDataLayout().getTypeAllocSize(type);
}

llvm::Value* IR::load_value(llvm::Value* source, std::shared_ptr<ExprAST> expr)
{
	auto type = source->getType();

	if (expr == nullptr)
		return builder->CreateLoad(source);
	// skip if current expression is address of and aggregate (struct, union)
	else if ((type->isIntegerTy() || type->isFloatTy() || type->isDoubleTy() || type->isFP128Ty())
			 || (expr->node_type == ASTNodeType::expr_unary && std::static_pointer_cast<UnaryExprAST>(expr)->op == UnaryOperator::address_of)
			 || expr->type->kind == TypeKind::aggregate)
		return source;
	else
	{
		assert(source->getValueID() == llvm::Value::ValueTy::GlobalVariableVal
			   || source->getValueID() == llvm::Value::ValueTy::InstructionVal + llvm::Instruction::Alloca
			   || source->getValueID() == llvm::Value::ValueTy::InstructionVal + llvm::Instruction::GetElementPtr
			   || source->getValueID() == llvm::Value::ValueTy::InstructionVal + llvm::Instruction::BitCast);
		return builder->CreateLoad(source);
	}
}

llvm::Value* IR::cast_value(llvm::Value* source, std::shared_ptr<TypeAST> src_type_ast, std::shared_ptr<TypeAST> dest_type_ast)
{
	llvm::Instruction::CastOps inst = llvm::Instruction::BitCast;

	if (translation_unit.is_same_types(src_type_ast, dest_type_ast))
		return source;
	else if (translation_unit.is_integer_type(src_type_ast))
	{
		if (translation_unit.is_real_float_type(dest_type_ast))
			inst = translation_unit.is_signed_integer_type(src_type_ast) ? llvm::Instruction::SIToFP : llvm::Instruction::UIToFP;
		else if (translation_unit.is_integer_type(dest_type_ast))
		{
			auto sta = std::static_pointer_cast<BuiltinTypeAST>(src_type_ast);
			auto dta = std::static_pointer_cast<BuiltinTypeAST>(dest_type_ast);
			if (type_nbits[sta->name] > type_nbits[dta->name])
				inst = llvm::Instruction::Trunc;
			else
				inst = translation_unit.is_signed_integer_type(src_type_ast) ? llvm::Instruction::SExt : llvm::Instruction::ZExt;
		}
		else if (translation_unit.is_pointer_type(dest_type_ast))
			inst = llvm::Instruction::IntToPtr;
		else
			assert_not_reached();
	}
	else if (translation_unit.is_float_type(src_type_ast))
	{
		if (translation_unit.is_float_type(dest_type_ast))
			return source;
		else if (translation_unit.is_double_type(dest_type_ast))
			inst = llvm::Instruction::FPExt;
		else if (translation_unit.is_signed_integer_type(dest_type_ast))
			inst = llvm::Instruction::FPToSI;
		else if (translation_unit.is_unsigned_integer_type(dest_type_ast))
			inst = llvm::Instruction::FPToUI;
		else
			assert_not_reached();
	}
	else if (translation_unit.is_double_type(src_type_ast))
	{
		if (translation_unit.is_double_type(dest_type_ast))
			return source;
		else if (translation_unit.is_float_type(dest_type_ast))
			inst = llvm::Instruction::FPTrunc;
		else if (translation_unit.is_signed_integer_type(dest_type_ast))
			inst = llvm::Instruction::FPToSI;
		else if (translation_unit.is_unsigned_integer_type(dest_type_ast))
			inst = llvm::Instruction::FPToUI;
		else
			assert_not_reached();
	}
	else if (translation_unit.is_long_double_type(src_type_ast))
	{
		if (translation_unit.is_long_double_type(dest_type_ast))
			return source;
		else if (translation_unit.is_float_type(dest_type_ast) || translation_unit.is_double_type(dest_type_ast))
			inst = llvm::Instruction::FPTrunc;
		else if (translation_unit.is_signed_integer_type(dest_type_ast))
			inst = llvm::Instruction::FPToSI;
		else if (translation_unit.is_unsigned_integer_type(dest_type_ast))
			inst = llvm::Instruction::FPToUI;
		else
			assert_not_reached();
	}
	else if (translation_unit.is_pointer_type(src_type_ast))
	{
		if (translation_unit.is_pointer_type(dest_type_ast))
			inst = llvm::Instruction::BitCast;
		else if (translation_unit.is_integer_type(dest_type_ast))
			inst = llvm::Instruction::PtrToInt;
		else
			assert_not_reached();
	}
	else if (translation_unit.is_array_type(src_type_ast))
	{
		llvm::Constant* zeroth = llvm::ConstantInt::get(builder->getInt32Ty(), 0);
		llvm::ArrayRef<llvm::Value*> indices = {zeroth, zeroth};
		auto value = builder->CreateGEP(source, indices);
		auto dest_type = get_type(dest_type_ast);

		if (translation_unit.is_integer_type(dest_type_ast))
			return builder->CreateCast(llvm::Instruction::PtrToInt, value, dest_type);
		else if (translation_unit.is_pointer_type(dest_type_ast))
			return value;
		else
			assert_not_reached();
	}
	else if (translation_unit.is_function_type(src_type_ast))
	{
		if (translation_unit.is_integer_type(dest_type_ast))
			inst = llvm::Instruction::PtrToInt;
		else if (translation_unit.is_pointer_type(dest_type_ast))
			inst = llvm::Instruction::BitCast;
		else
			assert_not_reached();
	}
	else
		assert_not_reached();

	auto dest_type = get_type(dest_type_ast);
	return builder->CreateCast(inst, source, dest_type);
}

llvm::AllocaInst* IR::create_alloca(llvm::Function* func, llvm::Type* type, llvm::StringRef name)
{
	llvm::IRBuilder<> tmp_block(&func->getEntryBlock(), func->getEntryBlock().begin());
	auto inst = tmp_block.CreateAlloca(type, nullptr, name);
	// temporary variable don't need to put into environment
	if (!name.empty())
		environment->define(name.str(), inst);
	return inst;
}

void IR::init_pass_maanger()
{
	func_pass_manager->add(new UnreachableBlockInstructionPass());
	// func_pass_manager->add(llvm::createPromoteMemoryToRegisterPass());
	// func_pass_manager->add(llvm::createInstructionCombiningPass());
	// func_pass_manager->add(llvm::createReassociatePass());
	// func_pass_manager->add(llvm::createGVNPass());
	func_pass_manager->add(llvm::createCFGSimplificationPass());
	func_pass_manager->doInitialization();
}

void IR::emit_object_file()
{
	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	auto target_triple = llvm::sys::getDefaultTargetTriple();
	module->setTargetTriple(target_triple);

	std::string error;
	auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

	if (!target)
		throw std::runtime_error(error);

	auto cpu = "generic";
	auto features = "";

	llvm::TargetOptions opt;
	auto rm = llvm::Optional<llvm::Reloc::Model>();
	auto target_machine = target->createTargetMachine(target_triple, cpu, features, opt, rm);

	module->setDataLayout(target_machine->createDataLayout());

	auto filename = "output.o";
	std::error_code ec;
	llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);

	if (ec)
		throw std::runtime_error("Could not open file: " + ec.message());

	llvm::legacy::PassManager pass;
	auto filetype = llvm::CGFT_ObjectFile;

	if (target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype))
		throw std::runtime_error("the target machine can't emit a file of this type");

	pass.run(*module);
	dest.flush();
}

std::string IR::generate()
{
	init_pass_maanger();
	for (auto declaration : translation_unit.declarations)
		declaration->accept(this);

	std::string str;
	llvm::raw_string_ostream ros(str);
	module->print(ros, nullptr, false, true);

	emit_object_file();

	return str;
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
	llvm::APFloat value(static_cast<double>(expr->value->value));
	bool ignored;
	const llvm::fltSemantics* flt_semantic = &llvm::APFloat::IEEEquad();
	value.convert(*flt_semantic, llvm::APFloat::rmTowardZero, &ignored);
	return llvm::ConstantFP::get(*context, value);
}

void* IR::visit_literal_expr(LiteralExprAST<unsigned char>* expr)
{
	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_CHAR, expr->value->value));
}

void* IR::visit_literal_expr(LiteralExprAST<std::string>* expr)
{
	return get_or_insert_global_string(expr->value->value);
}

void* IR::visit_identifier_expr(IdentifierExprAST* expr)
{
	llvm::Value* value = environment->lookup(expr->name);
	if (!value)
		throw std::runtime_error(expr->name->name + " doesn't exist");
	return value;
}

void* IR::visit_binary_expr(BinaryExprAST* expr)
{
	BinaryOperator binop = expr->op;
	llvm::Value* left = (llvm::Value*)expr->left->accept(this);
	llvm::Value* right = (llvm::Value*)expr->right->accept(this);
	llvm::Value* result = nullptr;

	switch (binop)
	{
	case BinaryOperator::assignment:
	{
		auto rvalue_right = load_value(right, expr->right);
		auto casted_rvalue_right = cast_value(rvalue_right, expr->right->type, expr->left->type);

		store_inst(left, expr->type, casted_rvalue_right, expr->right->type);
		result = casted_rvalue_right;
		break;
	}

	case BinaryOperator::addition_assigment:
	case BinaryOperator::subtraction_assignment:
	case BinaryOperator::addition:
	case BinaryOperator::subtraction:
	{
		auto rvalue_left = load_value(left, expr->left);
		auto casted_rvalue_left = cast_value(rvalue_left, expr->left->type, expr->type);
		auto rvalue_right = load_value(right, expr->right);
		auto casted_rvalue_right = translation_unit.is_pointer_type(expr->left->type) && translation_unit.is_integer_type(expr->right->type)
									   ? cast_value(rvalue_right, expr->right->type, translation_unit.get_type("int"))
									   : cast_value(rvalue_right, expr->right->type, expr->type);
		result = execute_binop(convert_assignment_to_arithmetic_binop(binop), expr->type, casted_rvalue_left, casted_rvalue_right);

		if (translation_unit.is_pointer_type(expr->left->type)
			&& translation_unit.is_pointer_type(expr->right->type)
			&& binop == BinaryOperator::subtraction)
		{
			auto ptype_ast = std::static_pointer_cast<PointerTypeAST>(expr->left->type);
			auto ptype = get_type(ptype_ast->underlay);
			auto size = module->getDataLayout().getTypeSizeInBits(ptype);
			result = builder->CreateExactSDiv(result, llvm::ConstantInt::get(builder->getInt32Ty(), llvm::APInt(NBITS_INT, size, true)));
		}

		if (binop == BinaryOperator::addition_assigment
			|| binop == BinaryOperator::subtraction_assignment)
			builder->CreateStore(result, left);
		break;
	}
	case BinaryOperator::multiplication_assigment:
	case BinaryOperator::division_assignment:
	case BinaryOperator::remainder_assignment:
	case BinaryOperator::bitwise_and_assigment:
	case BinaryOperator::bitwise_or_assigment:
	case BinaryOperator::bitwise_xor_assigment:
	case BinaryOperator::shift_left_assignment:
	case BinaryOperator::shift_right_assignment:
	case BinaryOperator::multiplication:
	case BinaryOperator::division:
	case BinaryOperator::remainder:
	case BinaryOperator::bitwise_and:
	case BinaryOperator::bitwise_or:
	case BinaryOperator::bitwise_xor:
	case BinaryOperator::shift_left:
	case BinaryOperator::shift_right:
	{
		auto rvalue_left = load_value(left, expr->left);
		auto casted_rvalue_left = cast_value(rvalue_left, expr->left->type, expr->type);
		auto rvalue_right = load_value(right, expr->right);
		auto casted_rvalue_right = cast_value(rvalue_right, expr->right->type, expr->type);
		result = execute_binop(convert_assignment_to_arithmetic_binop(binop), expr->type, casted_rvalue_left, casted_rvalue_right);

		if (binop == BinaryOperator::multiplication_assigment
			|| binop == BinaryOperator::division_assignment
			|| binop == BinaryOperator::remainder_assignment
			|| binop == BinaryOperator::bitwise_and_assigment
			|| binop == BinaryOperator::bitwise_or_assigment
			|| binop == BinaryOperator::bitwise_xor_assigment
			|| binop == BinaryOperator::shift_left_assignment
			|| binop == BinaryOperator::shift_right_assignment)
			builder->CreateStore(result, left);
		break;
	}

	case BinaryOperator::equal:
	case BinaryOperator::not_equal:
	case BinaryOperator::less:
	case BinaryOperator::greater_than:
	case BinaryOperator::less_or_equal:
	case BinaryOperator::greater_or_equal:
	{
		auto left_type = translation_unit.is_pointer_type(expr->left->type)
							 ? translation_unit.get_type("unsigned int")
							 : expr->left->type;
		auto right_type = translation_unit.is_pointer_type(expr->right->type)
							  ? translation_unit.get_type("unsigned int")
							  : expr->right->type;
		auto type_ast = translation_unit.convert_arithmetic_type(left_type, right_type);
		auto rvalue_left = load_value(left, expr->left);
		auto casted_rvalue_left = cast_value(rvalue_left, left_type, type_ast);
		auto rvalue_right = load_value(right, expr->right);
		auto casted_rvalue_right = cast_value(rvalue_right, right_type, type_ast);
		auto value = execute_binop(binop, expr->type, casted_rvalue_left, casted_rvalue_right);
		result = cast_value(value, type_ast, expr->type);
		break;
	}

	case BinaryOperator::comma:
	{
		auto rvalue_left = load_value(left, expr->left);
		auto rvalue_right = load_value(right, expr->right);
		result = cast_value(rvalue_right, expr->right->type, expr->type);
		break;
	}

	case BinaryOperator::and_:
	{
		auto func = builder->GetInsertBlock()->getParent();
		auto thenbb = llvm::BasicBlock::Create(*context, "and.then");
		auto elsebb = llvm::BasicBlock::Create(*context, "and.else");
		auto endbb = llvm::BasicBlock::Create(*context, "and.end");

		auto tmpvar = create_alloca(func, get_type(expr->type));
		branch_block(func, expr->left, thenbb, elsebb);

		activate_block(func, elsebb);
		store_inst(tmpvar, expr->type, llvm::ConstantInt::get(builder->getInt1Ty(), llvm::APInt(NBITS_BOOL, 0, false)), expr->type);
		builder->CreateBr(endbb);

		activate_block(func, thenbb);
		auto rvalue_right = load_value(right, expr->right);
		store_inst(tmpvar, expr->type, convert_to_bool(rvalue_right, ""), translation_unit.get_type("_Bool"));
		builder->CreateBr(endbb);

		activate_block(func, endbb);
		result = load_value(tmpvar, nullptr);
		break;
	}

	case BinaryOperator::or_:
	{
		auto func = builder->GetInsertBlock()->getParent();
		auto thenbb = llvm::BasicBlock::Create(*context, "or.then");
		auto elsebb = llvm::BasicBlock::Create(*context, "or.else");
		auto endbb = llvm::BasicBlock::Create(*context, "or.end");

		auto tmpvar = create_alloca(func, get_type(expr->type));
		branch_block(func, expr->left, thenbb, elsebb);

		activate_block(func, thenbb);
		store_inst(tmpvar, expr->type, llvm::ConstantInt::get(builder->getInt1Ty(), llvm::APInt(NBITS_BOOL, 1, false)), expr->type);
		builder->CreateBr(endbb);

		activate_block(func, elsebb);
		auto rvalue_right = load_value(right, expr->right);
		store_inst(tmpvar, expr->type, convert_to_bool(rvalue_right, ""), translation_unit.get_type("_Bool"));
		builder->CreateBr(endbb);

		activate_block(func, endbb);
		result = load_value(tmpvar, nullptr);
		break;
	}

	case BinaryOperator::array_subscript:
	{
		auto idx = ConstExprEval(this, expr->right).eval();
		std::vector<llvm::Value*> indices = {llvm::ConstantInt::get(builder->getInt32Ty(), 0), llvm::ConstantInt::get(builder->getInt32Ty(), idx)};
		result = builder->CreateGEP(left, indices);
		break;
	}
	}

	assert(result);
	return result;
}

void* IR::visit_unary_expr(UnaryExprAST* expr)
{
	UnaryOperator unaryop = expr->op;
	llvm::Value* expr1 = (llvm::Value*)expr->expr->accept(this);
	llvm::Value* result = nullptr;

	switch (unaryop)
	{
	case UnaryOperator::prefix_increment:
	case UnaryOperator::prefix_decrement:
	{
		auto rvalue_right = load_value(expr1, expr->expr);
		auto sign = unaryop == UnaryOperator::prefix_increment ? 1 : -1;
		auto one = translation_unit.is_real_float_type(expr->type)
					   ? (llvm::Constant*)llvm::ConstantFP::get(*context, llvm::APFloat(1.0 * sign))
					   : (llvm::Constant*)llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, 1 * sign, true));
		auto value = execute_binop(BinaryOperator::addition, expr->expr->type, rvalue_right, one);

		store_inst(expr1, expr->type, value, expr->type);
		result = value;
		break;
	}
	case UnaryOperator::postfix_increment:
	case UnaryOperator::postfix_decrement:
	{
		auto rvalue_right = load_value(expr1, expr->expr);
		auto sign = unaryop == UnaryOperator::postfix_increment ? 1 : -1;
		auto one = translation_unit.is_real_float_type(expr->type)
					   ? (llvm::Constant*)llvm::ConstantFP::get(*context, llvm::APFloat(1.0 * sign))
					   : (llvm::Constant*)llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, 1 * sign, true));
		auto value = execute_binop(BinaryOperator::addition, expr->expr->type, rvalue_right, one);

		store_inst(expr1, expr->type, value, expr->type);
		result = rvalue_right;
		break;
	}

	case UnaryOperator::plus:
		result = load_value(expr1, expr->expr);
		break;

	case UnaryOperator::minus:
	{
		auto rvalue_left = cast_value(llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, 0, true)),
									  translation_unit.get_type("int"),
									  expr->type);
		auto rvalue_right = load_value(expr1, expr->expr);
		auto casted_rvalue_right = cast_value(rvalue_right, expr->expr->type, expr->type);
		result = execute_binop(BinaryOperator::subtraction, expr->type, rvalue_left, casted_rvalue_right);
		break;
	}

	case UnaryOperator::complement:
	{
		auto rvalue_left = load_value(expr1, expr->expr);
		auto casted_rvalue_left = cast_value(rvalue_left, expr->expr->type, expr->type);
		auto rvalue_right = cast_value(llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, -1, true)),
									   translation_unit.get_type("int"),
									   expr->type);
		result = execute_binop(BinaryOperator::bitwise_xor, expr->type, casted_rvalue_left, rvalue_right);
		break;
	}

	case UnaryOperator::not_:
	{
		auto rvalue_left = load_value(expr1, expr->expr);
		auto casted_rvalue_left = cast_value(rvalue_left, expr->expr->type, expr->type);
		auto rvalue_right = cast_value(llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, 0, true)),
									   translation_unit.get_type("int"),
									   expr->type);
		result = execute_binop(BinaryOperator::equal, expr->type, casted_rvalue_left, rvalue_right);
		break;
	}

	case UnaryOperator::dereference:
		result = builder->CreateLoad(expr1);
		break;

	case UnaryOperator::address_of:
		result = expr1;
	}

	assert(result);
	return result;
}

void* IR::visit_tenary_expr(TenaryExprAST* expr)
{
	auto func = builder->GetInsertBlock()->getParent();
	auto thenbb = llvm::BasicBlock::Create(*context, "tenary.then");
	auto elsebb = llvm::BasicBlock::Create(*context, "tenary.else");
	auto endbb = llvm::BasicBlock::Create(*context, "tenary.end");

	auto tmpvar = create_alloca(func, get_type(expr->type));
	branch_block(func, expr->cond, thenbb, elsebb);

	activate_block(func, thenbb);
	auto then_value = (llvm::Value*)expr->expr1->accept(this);
	auto then_rvalue = load_value(then_value, expr->expr1);
	store_inst(tmpvar, expr->type, then_rvalue, expr->expr1->type);
	builder->CreateBr(endbb);

	activate_block(func, elsebb);
	auto else_value = (llvm::Value*)expr->expr2->accept(this);
	auto else_rvalue = load_value(else_value, expr->expr2);
	store_inst(tmpvar, expr->type, else_rvalue, expr->expr2->type);
	builder->CreateBr(endbb);

	activate_block(func, endbb);
	return builder->CreateLoad(tmpvar);
}

void* IR::visit_member_access_expr(MemberAccessExprAST* expr)
{
	auto expr_obj = (llvm::Value*)expr->object->accept(this);
	llvm::Value* object = nullptr;

	std::shared_ptr<AggregateTypeAST> object_type_ast;
	if (translation_unit.is_aggregate_type(expr->object->type))
	{
		object = expr_obj;
		object_type_ast = std::static_pointer_cast<AggregateTypeAST>(translation_unit.get_type(expr->object->type));
	}
	else if (translation_unit.is_pointer_type(expr->object->type))
	{
		auto ptype_ast = std::static_pointer_cast<PointerTypeAST>(expr->object->type);
		object_type_ast = std::static_pointer_cast<AggregateTypeAST>(translation_unit.get_type(ptype_ast->underlay));
		object = load_value(expr_obj, expr->object);
	}

	if (object_type_ast->aggregate_kind == AggregateKind::struct_)
	{
		auto indices = get_indices(object_type_ast, expr->member->name);
		return builder->CreateGEP(object, indices);
	}
	else
	{
		assert(object_type_ast->aggregate_kind == AggregateKind::union_);
		auto idxs = build_indices(object_type_ast, expr->member->name);
		return build_aggregate_accesses(object, object_type_ast, idxs);
	}
}

void* IR::visit_function_call_expr(FunctionCallExprAST* expr)
{
	auto func = builder->GetInsertBlock()->getParent();
	llvm::Function* callee = (llvm::Function*)expr->callee->accept(this);
	auto ftype_ast = std::static_pointer_cast<FunctionTypeAST>(expr->callee->type);

	if (!ftype_ast->is_variadic_args)
	{
		auto pad_arg = translation_unit.is_aggregate_type(ftype_ast->returning) ? 1 : 0;
		auto nargs = callee->arg_size() - pad_arg;
		if (expr->arguments.size() != nargs)
			throw std::runtime_error("arguments are mismatched");
	}

	std::vector<llvm::Value*> args;

	llvm::AllocaInst* fret = nullptr;
	if (translation_unit.is_aggregate_type(ftype_ast->returning))
	{
		auto return_type = get_type(ftype_ast->returning);
		fret = create_alloca(func, return_type);
		args.push_back(fret);
	}

	for (auto i = 0; i < expr->arguments.size(); ++i)
	{
		auto arg = expr->arguments[i];
		auto arg_value = (llvm::Value*)arg->accept(this);
		std::shared_ptr<TypeAST> ptype_ast = nullptr;

		if (i >= ftype_ast->parameters.size())
			ptype_ast = translation_unit.is_integer_type(arg->type)
							? translation_unit.promote_integer(arg->type)
							: arg->type;
		else
			ptype_ast = std::get<1>(ftype_ast->parameters[i]);

		if (translation_unit.is_aggregate_type(ptype_ast))
		{
			auto atype = get_type(ptype_ast);
			auto arg_avalue = create_alloca(func, atype);
			store_inst(arg_avalue, ptype_ast, arg_value, arg->type);
			args.push_back(arg_avalue);
		}
		else
		{
			auto arg_rvalue = load_value(arg_value, arg);
			auto casted_arg_value = cast_value(arg_rvalue, arg->type, ptype_ast);
			args.push_back(casted_arg_value);
		}
	}

	llvm::Value* rvalue = builder->CreateCall(callee, args);
	return fret ? fret : rvalue;
}

void* IR::visit_typecast_expr(TypeCastExprAST* expr)
{
	if (translation_unit.is_void_type(expr->type))
		return nullptr;

	auto value = (llvm::Value*)expr->expr->accept(this);
	return cast_value(value, expr->expr->type, expr->type);
}

void* IR::visit_sizeof_expr(SizeOfExprAST* expr)
{
	std::shared_ptr<TypeAST> type_ast = expr->expr ? expr->expr->type : expr->size_of_type;
	auto typesize = get_sizeof_type(type_ast);

	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, typesize, true));
}

void* IR::visit_alignof_expr(AlignOfExprAST* expr)
{
	auto alignsize = get_alignof_type(expr->align_of_type);

	return llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, alignsize, true));
}

void* IR::visit_initializer_expr(InitializerExprAST* expr)
{
	if (translation_unit.is_scalar_type(expr->type))
	{
		auto fexpr = expr->exprs.front();
		auto value = (llvm::Value*)fexpr->accept(this);
		return load_value(value, fexpr);
	}
	else if (translation_unit.is_array_type(expr->type))
	{
		auto atype = get_type(expr->type);
		std::vector<llvm::Constant*> values;
		for (auto e : expr->exprs)
			values.push_back((llvm::Constant*)e->accept(this));
		return llvm::ConstantArray::get((llvm::ArrayType*)atype, values);
	}
	else if (translation_unit.is_aggregate_type(expr->type))
	{
		auto stype_ast = std::static_pointer_cast<AggregateTypeAST>(translation_unit.get_type(expr->type));
		auto stype = get_type(stype_ast);
		llvm::Function* func = builder->GetInsertBlock()->getParent();
		llvm::Value* tmp = create_alloca(func, stype);

		if (stype_ast->aggregate_kind == AggregateKind::struct_)
		{
			for (auto idx = 0; idx < expr->exprs.size(); ++idx)
			{
				auto iexpr = expr->exprs[idx];
				auto value = (llvm::Value*)iexpr->accept(this);
				llvm::ArrayRef<llvm::Value*> indices = {
					llvm::ConstantInt::get(builder->getInt32Ty(), llvm::APInt(NBITS_INT, 0)),
					llvm::ConstantInt::get(builder->getInt32Ty(), llvm::APInt(NBITS_INT, idx)),
				};
				auto member = builder->CreateInBoundsGEP(tmp, indices);
				auto [_, member_type] = stype_ast->members[idx];
				store_inst(member, member_type, value, iexpr->type);
			}
		}
		else
		{
			assert(stype_ast->aggregate_kind == AggregateKind::union_);
			auto iexpr = expr->exprs.front();
			auto value = (llvm::Value*)iexpr->accept(this);
			std::shared_ptr<TypeAST> presented_type = get_largest_aggregate_member(stype_ast);

			tmp = cast_value(tmp, std::make_shared<PointerTypeAST>(stype_ast), std::make_shared<PointerTypeAST>(presented_type));
			store_inst(tmp, presented_type, value, iexpr->type);
		}
		return tmp;
	}
}

void* IR::visit_label_stmt(LabelStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	auto labelbb = llvm::BasicBlock::Create(*context, stmt->name->name);
	builder->CreateBr(labelbb);
	activate_block(func, labelbb);

	stmt->stmt->accept(this);
	return nullptr;
}

void* IR::visit_case_stmt(CaseStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	auto sstmt = std::static_pointer_cast<SwitchStmtBranch>(find_stmt_branch(StmtBranchType::switch_));
	assert(sstmt);

	auto value = (llvm::ConstantInt*)stmt->constant->accept(this);
	auto casebb = llvm::BasicBlock::Create(*context, "switch.case");
	builder->CreateBr(casebb);
	activate_block(func, casebb);
	sstmt->inst->addCase(value, casebb);

	stmt->stmt->accept(this);
	return nullptr;
}

void* IR::visit_default_stmt(DefaultStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	auto sstmt = std::static_pointer_cast<SwitchStmtBranch>(find_stmt_branch(StmtBranchType::switch_));
	assert(sstmt);

	auto defaultbb = llvm::BasicBlock::Create(*context, "switch.default");
	builder->CreateBr(defaultbb);
	activate_block(func, defaultbb);
	sstmt->inst->setDefaultDest(defaultbb);

	stmt->stmt->accept(this);
	return nullptr;
}

void* IR::visit_expr_stmt(ExprStmtAST* stmt)
{
	if (stmt->expr)
		return stmt->expr->accept(this);
	else
		return nullptr;
}

void* IR::visit_compound_stmt(CompoundStmtAST* stmt)
{
	enter_scope();

	for (auto st : stmt->stmts)
		st->accept(this);

	leave_scope();
	return nullptr;
}

void* IR::visit_if_stmt(IfStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	auto thenbb = llvm::BasicBlock::Create(*context, "if.then");
	auto elsebb = stmt->else_stmt ? llvm::BasicBlock::Create(*context, "if.else") : nullptr;
	auto endbb = llvm::BasicBlock::Create(*context, "if.end");

	branch_block(func, stmt->cond, thenbb, stmt->else_stmt ? elsebb : endbb);

	activate_block(func, thenbb);
	complete_block(func, stmt->if_stmt, endbb);

	if (stmt->else_stmt)
	{
		activate_block(func, elsebb);
		complete_block(func, stmt->else_stmt, endbb);
	}

	activate_block(func, endbb);
	return nullptr;
}

void* IR::visit_switch_stmt(SwitchStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	auto value = (llvm::Value*)stmt->expr->accept(this);
	auto endbb = llvm::BasicBlock::Create(*context, "switch.exit");
	auto switch_inst = builder->CreateSwitch(value, endbb, 2);

	auto sstmt = std::make_shared<SwitchStmtBranch>(SwitchStmtBranch(switch_inst, endbb));
	stmts_branch.push_back(sstmt);

	stmt->stmt->accept(this);

	unwind_stmt_branch(sstmt);
	builder->CreateBr(endbb);
	activate_block(func, endbb);
	return nullptr;
}

void* IR::visit_for_stmt(ForStmtAST* stmt)
{
	enter_scope();
	stmt->init->accept(this);

	auto func = builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* condbb = llvm::BasicBlock::Create(*context, "for.cond");
	llvm::BasicBlock* bodybb = llvm::BasicBlock::Create(*context, "for.body");
	llvm::BasicBlock* incrbb = llvm::BasicBlock::Create(*context, "for.incr");
	llvm::BasicBlock* endbb = llvm::BasicBlock::Create(*context, "for.end");

	auto fstmt = std::make_shared<LoopStmtBranch>(LoopStmtBranch(endbb, incrbb));
	stmts_branch.push_back(fstmt);
	builder->CreateBr(condbb);

	activate_block(func, condbb);
	branch_block(func, stmt->cond, bodybb, endbb);

	activate_block(func, bodybb);
	complete_block(func, stmt->stmt, incrbb);

	activate_block(func, incrbb);
	complete_block(func, stmt->inc, condbb);

	activate_block(func, endbb);

	unwind_stmt_branch(fstmt);
	leave_scope();
	return nullptr;
}

void* IR::visit_while_stmt(WhileStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* condbb = llvm::BasicBlock::Create(*context, "while.cond");
	llvm::BasicBlock* bodybb = llvm::BasicBlock::Create(*context, "while.body");
	llvm::BasicBlock* endbb = llvm::BasicBlock::Create(*context, "while.end");

	auto fstmt = std::make_shared<LoopStmtBranch>(LoopStmtBranch(endbb, condbb));
	stmts_branch.push_back(fstmt);
	builder->CreateBr(condbb);

	activate_block(func, condbb);
	branch_block(func, stmt->cond, bodybb, endbb);

	activate_block(func, bodybb);
	complete_block(func, stmt->stmt, condbb);

	activate_block(func, endbb);

	unwind_stmt_branch(fstmt);
	return nullptr;
}

void* IR::visit_dowhile_stmt(DoWhileStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* bodybb = llvm::BasicBlock::Create(*context, "dowhile.body");
	llvm::BasicBlock* condbb = llvm::BasicBlock::Create(*context, "dowhile.cond");
	llvm::BasicBlock* endbb = llvm::BasicBlock::Create(*context, "dowhile.pend");

	auto fstmt = std::make_shared<LoopStmtBranch>(LoopStmtBranch(endbb, condbb));
	stmts_branch.push_back(fstmt);
	builder->CreateBr(bodybb);

	activate_block(func, bodybb);
	complete_block(func, stmt->stmt, condbb);

	activate_block(func, condbb);
	branch_block(func, stmt->cond, bodybb, endbb);

	activate_block(func, endbb);

	unwind_stmt_branch(fstmt);
	return nullptr;
}

void* IR::visit_jump_stmt(JumpStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	for (auto& block : func->getBasicBlockList())
		if (block.getName() == stmt->name->name)
			builder->CreateBr(&block);
	return nullptr;
}

void* IR::visit_continue_stmt(ContinueStmtAST* stmt)
{
	auto fstmt = std::static_pointer_cast<LoopStmtBranch>(find_stmt_branch(StmtBranchType::loop));
	assert(fstmt);

	builder->CreateBr(fstmt->nextbb);
	return nullptr;
}

void* IR::visit_break_stmt(BreakStmtAST* stmt)
{
	auto fstmt = stmts_branch.back();
	assert(fstmt);

	builder->CreateBr(fstmt->endbb);
	return nullptr;
}

void* IR::visit_return_stmt(ReturnStmtAST* stmt)
{
	if (stmt->expr)
	{
		auto value = (llvm::Value*)stmt->expr->accept(this);
		auto rvalue = load_value(value, stmt->expr);
		auto ret = environment->lookup(LLVM_RETURN_NAME);
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(in_func_scope->type);
		store_inst(ret, ftype->returning, rvalue, stmt->expr->type);
	}
	auto rstmt = find_stmt_branch(StmtBranchType::function);
	builder->CreateBr(rstmt->endbb);

	return nullptr;
}

void* IR::visit_function_definition(FunctionDefinitionAST* stmt)
{
	llvm::Function* func = nullptr;
	auto ftype = std::static_pointer_cast<FunctionTypeAST>(stmt->type);
	if (!(func = module->getFunction(stmt->name->name)))
		func = create_function_prototype(stmt->name->name, stmt->type);

	enter_scope();
	stmts_branch.clear();
	in_func_scope = stmt;

	llvm::BasicBlock* entrybb = llvm::BasicBlock::Create(*context, "entry");
	activate_block(func, entrybb);

	llvm::BasicBlock* exitbb = llvm::BasicBlock::Create(*context, "exit");
	auto estmt = std::make_shared<StmtBranch>(StmtBranch(StmtBranchType::function, exitbb));
	stmts_branch.push_back(estmt);

	auto arg = func->args().begin();
	if (translation_unit.is_aggregate_type(ftype->returning))
	{
		arg->addAttr(llvm::Attribute::NoAlias);
		arg->addAttr(llvm::Attribute::StructRet);
		arg->setName(LLVM_RETURN_NAME);

		auto& larg = *arg;
		environment->define(LLVM_RETURN_NAME, &larg);

		arg = std::next(arg);
	}
	for (auto idx = 0; arg != func->args().end(); arg = std::next(arg))
	{
		auto [pname, ptype_ast] = ftype->parameters[idx++];
		arg->setName(pname->name);
		auto& larg = *arg;

		if (translation_unit.is_aggregate_type(ptype_ast))
		{
			environment->define(arg->getName().str(), &larg);
			arg->addAttr(llvm::Attribute::ByVal);
		}
		else
		{
			llvm::AllocaInst* alloca = create_alloca(func, arg->getType(), arg->getName());
			builder->CreateStore(&larg, alloca);
		}
	}

	if (!func->getReturnType()->isVoidTy())
		create_alloca(func, func->getReturnType(), LLVM_RETURN_NAME);

	stmt->body->accept(this);

	builder->CreateBr(exitbb);
	activate_block(func, exitbb);

	if (!func->getReturnType()->isVoidTy())
	{
		auto retval = builder->CreateLoad(environment->lookup(LLVM_RETURN_NAME));
		builder->CreateRet(retval);
	}
	else
		builder->CreateRetVoid();

	llvm::verifyFunction(*func);
	func_pass_manager->run(*func);

	in_func_scope = nullptr;
	stmts_branch.clear();
	leave_scope();
	return func;
}

void* IR::visit_declaration(DeclarationAST* stmt)
{
	get_type(stmt->type);

	for (auto [token, type_ast, expr] : stmt->declarators)
	{
		if (type_ast->kind == TypeKind::array)
			calculate_array_type_size(type_ast, expr);

		auto storage = translation_unit.get_storage_specifier(type_ast);
		auto type = get_type(type_ast);

		if (in_func_scope && storage != StorageSpecifier::static_)
		{
			llvm::Function* func = builder->GetInsertBlock()->getParent();
			auto alloca = create_alloca(func, type, token->name);

			if (expr)
			{
				auto value = (llvm::Value*)expr->accept(this);
				auto rvalue = load_value(value, expr);
				store_inst(alloca, type_ast, rvalue, expr->type);
			}
		}
		else
		{
			if (type->isFunctionTy())
				create_function_prototype(token->name, type_ast);
			else
			{
				auto qualifiers = translation_unit.get_type_qualifiers(type_ast);
				bool is_constant = std::any_of(qualifiers.begin(), qualifiers.end(), [](TypeQualifier qualifier)
											   {
												   return qualifier == TypeQualifier::const_;
											   });
				auto storage = translation_unit.get_storage_specifier(type_ast);
				auto linkage = get_linkage_type(storage);
				llvm::Constant* constant = nullptr;
				if (expr)
				{
					auto value = (llvm::Constant*)expr->accept(this);
					constant = (llvm::Constant*)cast_value(value, expr->type, type_ast);
				}

				auto declarator = new llvm::GlobalVariable(*module, type, is_constant, linkage, constant, token->name);
				environment->define(token, declarator);
			}
		}
	}
	return nullptr;
}
