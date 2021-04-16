#ifndef LLVM_FLOWABLE_STMT_H
#define LLVM_FLOWABLE_STMT_H 1

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"

enum class StmtBranchType
{
	loop,
	switch_,
	function,
};

struct StmtBranch
{
	StmtBranch(StmtBranchType type, llvm::BasicBlock *endbb)
		: type(type)
		, endbb(endbb)
	{
	}

	StmtBranchType type;
	llvm::BasicBlock *endbb;
};

struct LoopStmtBranch : public StmtBranch
{
	LoopStmtBranch(llvm::BasicBlock *endbb, llvm::BasicBlock *condbb = nullptr)
		: StmtBranch(StmtBranchType::loop, endbb)
		, nextbb(condbb)
	{
	}

	llvm::BasicBlock *nextbb;
};

struct SwitchStmtBranch : public StmtBranch
{
	SwitchStmtBranch(llvm::SwitchInst *inst, llvm::BasicBlock *endbb)
		: StmtBranch(StmtBranchType::switch_, endbb)
		, inst(inst)
	{
	}

	llvm::SwitchInst *inst;
};

struct FunctionStmtBranch : public StmtBranch
{
	FunctionStmtBranch(llvm::BasicBlock *endbb)
		: StmtBranch(StmtBranchType::function, endbb)
	{
	}
};

#endif
