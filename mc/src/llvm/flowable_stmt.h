#ifndef LLVM_FLOWABLE_STMT_H
#define LLVM_FLOWABLE_STMT_H 1

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"

enum class FlowableStmtType
{
	loop,
	switch_,
	return_,
};

struct FlowableStmt
{
	FlowableStmt(FlowableStmtType type, llvm::BasicBlock *endbb)
		: type(type)
		, endbb(endbb)
	{
	}

	FlowableStmtType type;
	llvm::BasicBlock *endbb;
};

struct LoopStmt : public FlowableStmt
{
	LoopStmt(llvm::BasicBlock *endbb, llvm::BasicBlock *condbb = nullptr)
		: FlowableStmt(FlowableStmtType::loop, endbb)
		, nextbb(condbb)
	{
	}

	llvm::BasicBlock *nextbb;
};

struct SwitchStmt : public FlowableStmt
{
	SwitchStmt(llvm::SwitchInst *inst, llvm::BasicBlock *endbb)
		: FlowableStmt(FlowableStmtType::switch_, endbb)
		, inst(inst)
	{
	}

	llvm::SwitchInst *inst;
};

struct ReturnStmt : public FlowableStmt
{
	ReturnStmt(llvm::BasicBlock *endbb)
		: FlowableStmt(FlowableStmtType::return_, endbb)
	{
	}
};

#endif
