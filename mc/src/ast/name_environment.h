#ifndef LLVM_Name_ENVIRONMENT_H
#define LLVM_Name_ENVIRONMENT_H 1

#include <unordered_map>

#include "ast/type.h"

enum class SymbolType
{
	declarator,
	type,
};

class NameEnvironment
{
public:
	NameEnvironment(NameEnvironment *enclosing)
		: symbols()
		, enclosing(enclosing)
	{
	}

	SymbolType lookup(std::string name);
	SymbolType lookup(const std::shared_ptr<TokenIdentifier> &identifier);
	void define(std::string name, SymbolType value);
	void define(const std::shared_ptr<TokenIdentifier> &identifier, SymbolType value);
	bool is_typedef(const std::shared_ptr<TypeAST> &type);
	NameEnvironment *get_enclosing();

private:
	NameEnvironment *enclosing;
	std::unordered_map<std::string, SymbolType> symbols;
};

#endif
