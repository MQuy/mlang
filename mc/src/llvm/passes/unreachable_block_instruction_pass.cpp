#include "unreachable_block_instruction_pass.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

bool UnreachableBlockInstructionPass ::runOnFunction(llvm::Function& func)
{
	if (skipFunction(func))
		return false;

	for (llvm::BasicBlock& bb : func)
	{
		auto deleted = false;
		auto& list = bb.getInstList();
		for (auto inst = list.begin(); inst != list.end();)
		{
			if (deleted)
				inst = list.erase(inst);
			else
			{
				auto value_id = inst->getValueID();
				if (value_id == llvm::Value::InstructionVal + llvm::Instruction::Br
					|| value_id == llvm::Value::InstructionVal + llvm::Instruction::Ret
					|| value_id == llvm::Value::InstructionVal + llvm::Instruction::Switch)
					deleted = true;
				inst++;
			}
		}
	}
}

char UnreachableBlockInstructionPass::ID = 0;

static RegisterPass<UnreachableBlockInstructionPass> X("Unreachable block instruction", "Unreachable block instruction pass", false /* Only looks at CFG */, false /* Analysis Pass */);

static RegisterStandardPasses Y(
	PassManagerBuilder::EP_EarlyAsPossible,
	[](const PassManagerBuilder& Builder,
	   legacy::PassManagerBase& PM)
	{
		PM.add(new UnreachableBlockInstructionPass());
	});
