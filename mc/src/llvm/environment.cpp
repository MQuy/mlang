#include "environment.h"

llvm::Value* Environment::lookup(std::string name)
{
	auto scope = this;
	for (; scope; scope = scope->enclosing)
	{
		auto symbols = scope->symbols;
		if (symbols.find(name) != symbols.end())
			return symbols[name];
	}
	return nullptr;
}

void Environment::define(std::string name, llvm::Value* value)
{
	if (symbols[name])
		throw std::runtime_error(name + " already exist");

	symbols[name] = value;
}

Environment* Environment::get_enclosing()
{
	return enclosing;
}
