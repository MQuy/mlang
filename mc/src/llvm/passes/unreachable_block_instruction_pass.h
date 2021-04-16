#ifndef UNREACHABLE_BLOCK_INSTRUCTION_PASS_H
#define UNREACHABLE_BLOCK_INSTRUCTION_PASS_H 1

#include "llvm/IR/Function.h"
#include "llvm/Pass.h"

struct UnreachableBlockInstructionPass : public llvm::FunctionPass
{
	static char ID;
	UnreachableBlockInstructionPass()
		: llvm::FunctionPass(ID)
	{
	}

	bool runOnFunction(llvm::Function& func) override;
};

#endif
