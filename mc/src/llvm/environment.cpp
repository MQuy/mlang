#include "environment.h"

llvm::Value* ValueEnvironment::lookup(std::string name)
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

void ValueEnvironment::define(std::string name, llvm::Value* value)
{
	if (symbols[name])
		throw std::runtime_error(name + " already exist");

	symbols[name] = value;
}

ValueEnvironment* ValueEnvironment::get_enclosing()
{
	return enclosing;
}
