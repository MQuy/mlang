#ifndef LLVM_ENVIRONMENT_H
#define LLVM_ENVIRONMENT_H 1

#include <unordered_map>

#include "llvm/IR/Instructions.h"

class Environment
{
public:
	Environment(Environment *enclosing)
		: symbols()
		, enclosing(enclosing)
	{
	}

	llvm::Value *lookup(std::string name);
	void define(std::string name, llvm::Value *value);
	Environment *get_enclosing();

private:
	Environment *enclosing;
	std::unordered_map<std::string, llvm::Value *> symbols;
};

#endif
