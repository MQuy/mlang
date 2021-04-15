#include "ir.h"

#include "const_eval.h"

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

std::shared_ptr<FlowableStmt> IR::find_flowable_stmt(FlowableStmtType type)
{
	for (auto block = flowable_stmts.rbegin(); block != flowable_stmts.rend(); ++block)
		if ((*block)->type == type)
			return *block;
	return nullptr;
}

void IR::unwind_flowable_stmt(std::shared_ptr<FlowableStmt> target, bool self_included)
{
	auto iterator = std::find(flowable_stmts.begin(), flowable_stmts.end(), target);

	if (iterator == flowable_stmts.end())
		return;

	flowable_stmts.erase(self_included ? iterator : iterator++, flowable_stmts.end());
}

llvm::Value* IR::create_bool_branch(llvm::Value* source, std::string name)
{
	auto type = source->getType();
	if (type->isIntegerTy())
		return builder->CreateICmpNE(source, llvm::ConstantInt::get(*context, llvm::APInt(NBITS_INT, 0)), name);
	else if (type->isFloatTy())
		return builder->CreateICmpNE(source, llvm::ConstantFP::get(*context, llvm::APFloat(0.0)), name);
	else if (type->isPointerTy())
		return builder->CreateIsNull(source);
	else
		throw std::runtime_error("Only support scalar types in branch condtion");
}

void IR::enter_scope()
{
	environment = new Environment(environment);
}

void IR::leave_scope()
{
	environment = environment->get_enclosing();
}

/*
constant casting from integer to float/double or vice versa
the sign of value doesn't matter since the action is performed at compile time
*/
llvm::Constant* IR::cast_constant(llvm::Constant* source, llvm::Type* dest_type)
{
	llvm::Instruction::CastOps inst = llvm::Instruction::BitCast;
	auto source_type = source->getType();

	if (source_type->isIntegerTy())
	{
		auto value = ((llvm::ConstantInt*)source)->getValue();
		auto layout = module->getDataLayout();

		if (dest_type->isFloatTy() || dest_type->isDoubleTy() || dest_type->isFP128Ty())
			inst = llvm::Instruction::SIToFP;
		else if (layout.getTypeAllocSize(source_type) > layout.getTypeAllocSize(dest_type))
			inst = llvm::Instruction::Trunc;
		else
			inst = llvm::Instruction::SExt;
	}
	else if (source_type->isFloatTy())
	{
		if (dest_type->isFloatTy())
			return source;
		else if (dest_type->isDoubleTy())
			inst = llvm::Instruction::FPExt;
		else if (dest_type->isIntegerTy())
			inst = llvm::Instruction::FPToSI;
	}
	else if (source_type->isDoubleTy())
	{
		if (dest_type->isDoubleTy())
			return source;
		if (dest_type->isFloatTy())
			inst = llvm::Instruction::FPTrunc;
		else if (dest_type->isIntegerTy())
			inst = llvm::Instruction::FPToSI;
	}
	else
		assert_not_reached();

	return (llvm::Constant*)builder->CreateCast(inst, source, dest_type);
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
	// TODO: MQ 2021-04-11 Support generating long double constant
	return llvm::ConstantFP::get(*context, llvm::APFloat((double)expr->value->value));
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
	llvm::Value* value = environment->lookup(expr->name->name);
	if (!value)
		throw std::runtime_error(expr->name->name + " doesn't exist");

	assert(value->getValueID() == llvm::Value::ValueTy::GlobalVariableVal || value->getValueID() == llvm::Value::ValueTy::InstructionVal + llvm::Instruction::Alloca);
	return builder->CreateLoad(value);
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
	throw std::runtime_error("not implemented yet");
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

	auto fstmt = std::make_shared<FlowableStmt>(FlowableStmt(FlowableStmtType::loop, endbb, incrbb));
	flowable_stmts.push_back(fstmt);
	builder->CreateBr(condbb);

	activate_block(func, condbb);
	branch_block(func, stmt->cond, bodybb, endbb);

	activate_block(func, bodybb);
	complete_block(func, stmt->stmt, incrbb);

	activate_block(func, incrbb);
	complete_block(func, stmt->inc, condbb);

	activate_block(func, endbb);

	unwind_flowable_stmt(fstmt);
	leave_scope();
	return nullptr;
}

void* IR::visit_while_stmt(WhileStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* condbb = llvm::BasicBlock::Create(*context, "while.cond");
	llvm::BasicBlock* bodybb = llvm::BasicBlock::Create(*context, "while.body");
	llvm::BasicBlock* endbb = llvm::BasicBlock::Create(*context, "while.end");

	auto fstmt = std::make_shared<FlowableStmt>(FlowableStmt(FlowableStmtType::loop, endbb, condbb));
	flowable_stmts.push_back(fstmt);
	builder->CreateBr(condbb);

	activate_block(func, condbb);
	branch_block(func, stmt->cond, bodybb, endbb);

	activate_block(func, bodybb);
	complete_block(func, stmt->stmt, condbb);

	activate_block(func, endbb);

	unwind_flowable_stmt(fstmt);
	return nullptr;
}

void* IR::visit_dowhile_stmt(DoWhileStmtAST* stmt)
{
	auto func = builder->GetInsertBlock()->getParent();
	llvm::BasicBlock* bodybb = llvm::BasicBlock::Create(*context, "dowhile.body");
	llvm::BasicBlock* condbb = llvm::BasicBlock::Create(*context, "dowhile.cond");
	llvm::BasicBlock* endbb = llvm::BasicBlock::Create(*context, "dowhile.pend");

	auto fstmt = std::make_shared<FlowableStmt>(FlowableStmt(FlowableStmtType::loop, endbb, condbb));
	flowable_stmts.push_back(fstmt);
	builder->CreateBr(bodybb);

	activate_block(func, bodybb);
	complete_block(func, stmt->stmt, condbb);

	activate_block(func, condbb);
	branch_block(func, stmt->cond, bodybb, endbb);

	activate_block(func, endbb);

	unwind_flowable_stmt(fstmt);
	return nullptr;
}

void* IR::visit_jump_stmt(JumpStmtAST* stmt)
{
	throw std::runtime_error("not implemented yet");
}

void* IR::visit_continue_stmt(ContinueStmtAST* stmt)
{
	auto fstmt = find_flowable_stmt(FlowableStmtType::loop);
	if (!fstmt)
		throw std::runtime_error("continue has to be inside loop stmt");

	builder->CreateBr(fstmt->nextbb);
	unwind_flowable_stmt(fstmt, false);
	return nullptr;
}

void* IR::visit_break_stmt(BreakStmtAST* stmt)
{
	auto fstmt = flowable_stmts.back();
	if (!fstmt)
		throw std::runtime_error("break has to be inside loop/switch stmt");

	builder->CreateBr(fstmt->endbb);
	unwind_flowable_stmt(fstmt);
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
	return nullptr;
}

llvm::AllocaInst* IR::create_entry_block_alloca(llvm::Function* func, llvm::Type* type, llvm::StringRef name)
{
	llvm::IRBuilder<> tmp_block(&func->getEntryBlock(), func->getEntryBlock().begin());
	return tmp_block.CreateAlloca(type, nullptr, name);
}

void* IR::visit_function_definition(FunctionDefinitionAST* stmt)
{
	enter_scope();
	in_func_scope = true;

	llvm::Function* func = nullptr;
	if (!(func = module->getFunction(stmt->name->name)))
	{
		auto ftype_ast = std::static_pointer_cast<FunctionTypeAST>(stmt->type);
		llvm::FunctionType* ftype = (llvm::FunctionType*)get_type(ftype_ast);
		auto storage = translation_unit.get_storage_specifier(ftype_ast->returning);
		auto linkage = get_linkage_type(storage);
		func = llvm::Function::Create(ftype, linkage, stmt->name->name, &*module);
	}

	llvm::BasicBlock* bb = llvm::BasicBlock::Create(*context, "entry", func);
	builder->SetInsertPoint(bb);

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
	if (!func->getReturnType()->isVoidTy())
	{
		auto retval = builder->CreateLoad(environment->lookup(LLVM_RETURN_NAME));
		builder->CreateRet(retval);
	}
	llvm::verifyFunction(*func);

	in_func_scope = false;
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
			environment->define(token->name, alloca);

			if (expr)
			{
				auto value = (llvm::Value*)expr->accept(this);
				builder->CreateStore(value, alloca);
			}
		}
		else
		{
			auto qualifiers = translation_unit.get_type_qualifiers(type);
			bool is_constant = std::any_of(qualifiers.begin(), qualifiers.end(), [](TypeQualifier qualifier) {
				return qualifier == TypeQualifier::const_;
			});
			auto storage = translation_unit.get_storage_specifier(type);
			auto linkage = get_linkage_type(storage);
			llvm::Type* ty = get_type(type);

			llvm::Constant* constant = nullptr;
			if (expr)
			{
				auto value = (llvm::Constant*)expr->accept(this);
				constant = cast_constant(value, ty);
			}

			auto global_variable = new llvm::GlobalVariable(*module, ty, is_constant, linkage, constant, token->name);
			environment->define(token->name, global_variable);
		}
	}
	return nullptr;
}
