#ifndef LLVM_VALUE_ENVIRONMENT_H
#define LLVM_VALUE_ENVIRONMENT_H 1

#include <unordered_map>

#include "ast/type.h"
#include "llvm/IR/Instructions.h"

class ValueEnvironment
{
public:
	ValueEnvironment(ValueEnvironment *enclosing)
		: symbols()
		, enclosing(enclosing)
	{
	}

	llvm::Value *lookup(std::string name);
	llvm::Value *lookup(const std::shared_ptr<TokenIdentifier> &identifier);
	void define(std::string name, llvm::Value *value, bool override = false);
	void define(const std::shared_ptr<TokenIdentifier> &identifier, llvm::Value *value, bool override = false);
	ValueEnvironment *get_enclosing();

private:
	ValueEnvironment *enclosing;
	std::unordered_map<std::string, llvm::Value *> symbols;
};

#endif
