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
		auto underlay_type = get_type(ptype_ast->underlay);
		type = llvm::PointerType::get(underlay_type, 0);
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
		if (auto ty = module->getTypeByName(atype_ast->name->name))
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
				type = llvm::StructType::create(*context, {mtype}, atype_ast->name->name);
			}
			else
				type = llvm::StructType::create(*context, members, atype_ast->name->name);
		}
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
	else
		assert_not_reached();

	return type;
}

llvm::GlobalValue::LinkageTypes IR::get_linkage_type(StorageSpecifier storage)
{
	auto linkage = storage == StorageSpecifier::static_
					   ? llvm::GlobalValue::LinkageTypes::InternalLinkage
					   : llvm::GlobalValue::LinkageTypes::ExternalLinkage;
	return linkage;
}

void IR::complete_block(llvm::Function* func, std::shared_ptr<ASTNode> node, llvm::BasicBlock* nextbb)
{
	node->accept(this);
	builder->CreateBr(nextbb);
}

void IR::branch_block(llvm::Function* func, std::shared_ptr<ASTNode> node, llvm::BasicBlock* truebb, llvm::BasicBlock* falsebb)
{
	auto cond = (llvm::Value*)node->accept(this);
	cond = create_bool_branch(cond, "loopcond");
	builder->CreateCondBr(cond, truebb, falsebb);
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

llvm::Value* IR::create_bool_branch(llvm::Value* source, std::string name)
{
	auto type = source->getType();
	if (type->isIntegerTy())
		return builder->CreateICmpNE(source, llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, 0)), name);
	else if (type->isFloatTy())
		return builder->CreateFCmpUNE(source, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), name);
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

llvm::Value* IR::execute_binop(BinaryOperator op, std::shared_ptr<TypeAST> type, llvm::Value* left, llvm::Value* right)
{
	llvm::Value* result = nullptr;

	// TODO: MQ 2021-04-25 Support pointer addition and substraction
	if (op == BinaryOperator::addition)
	{
		if (translation_unit.is_integer_type(type))
			result = builder->CreateAdd(left, right, "", false, translation_unit.is_signed_integer_type(type));
		else
			result = builder->CreateFAdd(left, right);
	}
	else if (op == BinaryOperator::subtraction)
	{
		if (translation_unit.is_integer_type(type))
			result = builder->CreateSub(left, right, "", false, translation_unit.is_signed_integer_type(type));
		else
			result = builder->CreateFSub(left, right);
	}
	else if (op == BinaryOperator::multiplication)
	{
		if (translation_unit.is_integer_type(type))
			result = builder->CreateMul(left, right);
		else
			result = builder->CreateFMul(left, right);
	}
	else if (op == BinaryOperator::division)
	{
		if (translation_unit.is_integer_type(type))
			result = translation_unit.is_unsigned_integer_type(type)
						 ? builder->CreateUDiv(left, right)
						 : builder->CreateSDiv(left, right);
		else
			result = builder->CreateFDiv(left, right);
	}
	else if (op == BinaryOperator::remainder)
	{
		if (translation_unit.is_integer_type(type))
			result = translation_unit.is_unsigned_integer_type(type)
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
		result = translation_unit.is_signed_integer_type(type)
					 ? builder->CreateAShr(left, right)
					 : builder->CreateLShr(left, right);
	else if (op == BinaryOperator::equal)
	{
		if (translation_unit.is_integer_type(type) || translation_unit.is_pointer_type(type))
			result = builder->CreateICmpEQ(left, right);
		else if (translation_unit.is_real_float_type(type))
			result = builder->CreateFCmpUEQ(left, right);
	}
	else if (op == BinaryOperator::not_equal)
	{
		if (translation_unit.is_integer_type(type) || translation_unit.is_pointer_type(type))
			result = builder->CreateICmpNE(left, right);
		else if (translation_unit.is_real_float_type(type))
			result = builder->CreateFCmpUNE(left, right);
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
	return llvm::Function::Create(ftype, linkage, name, &*module);
}

llvm::Value* IR::get_or_insert_global_string(std::string content)
{
	llvm::Value* value = global_strings[content];
	if (!value)
		global_strings[content] = value = builder->CreateGlobalStringPtr(content);

	return value;
}

unsigned IR::get_alignof_type(std::shared_ptr<TypeAST> type)
{
	auto ty = get_type(type);
	return module->getDataLayout().getABITypeAlignment(ty);
}

unsigned IR::get_sizeof_type(std::shared_ptr<TypeAST> type)
{
	auto ty = get_type(type);
	return module->getDataLayout().getTypeAllocSize(ty);
}

llvm::Value* IR::load_value(llvm::Value* source, std::shared_ptr<ExprAST> expr)
{
	// skip if current expression is address of
	if (expr->node_type == ASTNodeType::expr_unary
		&& std::static_pointer_cast<UnaryExprAST>(expr)->op == UnaryOperator::address_of)
		return source;
	else if (source->getValueID() == llvm::Value::ValueTy::GlobalVariableVal
			 || source->getValueID() == llvm::Value::ValueTy::InstructionVal + llvm::Instruction::Alloca)
		return builder->CreateLoad(source);
	else
		return source;
}

llvm::Value* IR::cast_value(llvm::Value* source, std::shared_ptr<TypeAST> src_type_ast, std::shared_ptr<TypeAST> dest_type_ast)
{
	llvm::Instruction::CastOps inst = llvm::Instruction::BitCast;

	if (translation_unit.is_integer_type(src_type_ast))
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
	}
	else if (translation_unit.is_pointer_type(src_type_ast))
	{
		if (translation_unit.is_pointer_type(dest_type_ast))
			return source;
		else if (translation_unit.is_integer_type(dest_type_ast))
			inst = llvm::Instruction::PtrToInt;
	}
	else if (translation_unit.is_array_type(src_type_ast))
	{
		if (translation_unit.is_array_type(dest_type_ast) && translation_unit.is_same_types(src_type_ast, dest_type_ast))
			return source;
		else
			assert_not_reached();
	}
	else
		assert_not_reached();

	auto dest_type = get_type(dest_type_ast);
	return builder->CreateCast(inst, source, dest_type);
}

llvm::AllocaInst* IR::create_entry_block_alloca(llvm::Function* func, llvm::Type* type, llvm::StringRef name)
{
	llvm::IRBuilder<> tmp_block(&func->getEntryBlock(), func->getEntryBlock().begin());
	return tmp_block.CreateAlloca(type, nullptr, name);
}

void IR::init_pass_maanger()
{
	func_pass_manager->add(new UnreachableBlockInstructionPass());
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
	module->print(ros, nullptr);

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
	// TODO: MQ 2021-04-11 Support generating long double constant
	return llvm::ConstantFP::get(*context, llvm::APFloat((double)expr->value->value));
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

		builder->CreateStore(casted_rvalue_right, left);
		result = casted_rvalue_right;
		break;
	}

	case BinaryOperator::addition_assigment:
	case BinaryOperator::subtraction_assignment:
	case BinaryOperator::multiplication_assigment:
	case BinaryOperator::division_assignment:
	case BinaryOperator::remainder_assignment:
	case BinaryOperator::bitwise_and_assigment:
	case BinaryOperator::bitwise_or_assigment:
	case BinaryOperator::bitwise_xor_assigment:
	case BinaryOperator::shift_left_assignment:
	case BinaryOperator::shift_right_assignment:
	{
		auto rvalue_right = load_value(right, expr->right);
		auto casted_rvalue_right = cast_value(rvalue_right, expr->right->type, expr->left->type);
		auto rvalue_left = load_value(left, expr->left);

		result = execute_binop(binop, expr->type, rvalue_left, casted_rvalue_right);
		builder->CreateStore(result, left);
		break;
	}

	case BinaryOperator::addition:
	case BinaryOperator::subtraction:
	case BinaryOperator::multiplication:
	case BinaryOperator::division:
	case BinaryOperator::remainder:
	case BinaryOperator::bitwise_and:
	case BinaryOperator::bitwise_or:
	case BinaryOperator::bitwise_xor:
	case BinaryOperator::shift_left:
	case BinaryOperator::shift_right:
	case BinaryOperator::equal:
	case BinaryOperator::not_equal:
	{
		auto rvalue_left = load_value(left, expr->left);
		auto casted_rvalue_left = cast_value(rvalue_left, expr->left->type, expr->type);
		auto rvalue_right = load_value(right, expr->right);
		auto casted_rvalue_right = cast_value(rvalue_right, expr->right->type, expr->type);
		result = execute_binop(binop, expr->type, casted_rvalue_left, casted_rvalue_right);
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
		result = load_value(expr1, expr->expr);
		break;

	case UnaryOperator::address_of:
		result = expr1;
	}

	assert(result);
	return result;
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
	llvm::Function* func = (llvm::Function*)expr->callee->accept(this);

	if (expr->arguments.size() != func->arg_size())
		throw std::runtime_error("arguments are mismatched");

	std::vector<llvm::Value*> args;
	for (auto arg : expr->arguments)
		args.push_back((llvm::Value*)arg->accept(this));

	return builder->CreateCall(func, args);
}

void* IR::visit_typecast_expr(TypeCastExprAST* expr)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_sizeof_expr(SizeOfExprAST* expr)
{
	std::shared_ptr<TypeAST> type = expr->expr ? expr->expr->type : expr->size_of_type;
	auto typesize = get_sizeof_type(type);

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
		return expr->exprs.front()->accept(this);
	else if (translation_unit.is_array_type(expr->type))
	{
		auto atype = get_type(expr->type);
		std::vector<llvm::Constant*> values;
		for (auto e : expr->exprs)
			values.push_back((llvm::Constant*)e->accept(this));
		return llvm::ConstantArray::get((llvm::ArrayType*)atype, values);
	}
	else if (translation_unit.is_struct_type(expr->type))
	{
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
		auto ret = environment->lookup(LLVM_RETURN_NAME);
		auto value = (llvm::Value*)stmt->expr->accept(this);

		builder->CreateStore(value, ret);
	}
	auto rstmt = find_stmt_branch(StmtBranchType::function);
	builder->CreateBr(rstmt->endbb);

	return nullptr;
}

void* IR::visit_function_definition(FunctionDefinitionAST* stmt)
{
	enter_scope();
	stmts_branch.clear();
	in_func_scope = true;

	llvm::Function* func = nullptr;
	if (!(func = module->getFunction(stmt->name->name)))
		func = create_function_prototype(stmt->name->name, stmt->type);

	unsigned idx = 0;
	for (auto& arg : func->args())
		arg.setName(std::get<0>(std::static_pointer_cast<FunctionTypeAST>(stmt->type)->parameters[idx++])->name);

	llvm::BasicBlock* entrybb = llvm::BasicBlock::Create(*context, "entry", func);
	builder->SetInsertPoint(entrybb);

	llvm::BasicBlock* exitbb = llvm::BasicBlock::Create(*context, "exit");
	auto estmt = std::make_shared<StmtBranch>(StmtBranch(StmtBranchType::function, exitbb));
	stmts_branch.push_back(estmt);

	if (!func->getReturnType()->isVoidTy())
	{
		llvm::AllocaInst* alloca = create_entry_block_alloca(func, func->getReturnType());
		environment->define(LLVM_RETURN_NAME, alloca);
	}
	for (auto& arg : func->args())
	{
		llvm::AllocaInst* alloca = create_entry_block_alloca(func, arg.getType(), arg.getName());
		builder->CreateStore(&arg, alloca);
		environment->define(arg.getName().str(), alloca);
	}

	stmt->body->accept(this);

	builder->CreateBr(exitbb);
	activate_block(func, exitbb);

	if (!func->getReturnType()->isVoidTy())
	{
		auto retval = builder->CreateLoad(environment->lookup(LLVM_RETURN_NAME));
		builder->CreateRet(retval);
	}

	llvm::verifyFunction(*func);
	func_pass_manager->run(*func);

	in_func_scope = false;
	stmts_branch.clear();
	leave_scope();
	return func;
}

void* IR::visit_declaration(DeclarationAST* stmt)
{
	for (auto [token, type, expr] : stmt->declarators)
	{
		if (in_func_scope)
		{
			llvm::Function* func = builder->GetInsertBlock()->getParent();
			auto ty = get_type(type);
			auto alloca = create_entry_block_alloca(func, ty, token->name);
			environment->define(token, alloca);

			if (expr)
			{
				auto value = (llvm::Value*)expr->accept(this);
				auto cvalue = cast_value(value, expr->type, type);
				builder->CreateStore(cvalue, alloca);
			}
		}
		else
		{
			llvm::Type* ty = get_type(type);
			llvm::Value* declarator;

			if (ty->isFunctionTy())
				declarator = create_function_prototype(token->name, type);
			else
			{
				auto qualifiers = translation_unit.get_type_qualifiers(type);
				bool is_constant = std::any_of(qualifiers.begin(), qualifiers.end(), [](TypeQualifier qualifier) {
					return qualifier == TypeQualifier::const_;
				});
				auto storage = translation_unit.get_storage_specifier(type);
				auto linkage = get_linkage_type(storage);
				llvm::Constant* constant = nullptr;
				if (expr)
				{
					auto value = (llvm::Constant*)expr->accept(this);
					constant = (llvm::Constant*)cast_value(value, expr->type, type);
				}

				declarator = new llvm::GlobalVariable(*module, ty, is_constant, linkage, constant, token->name);
			}
			environment->define(token, declarator);
		}
	}
	return nullptr;
}
