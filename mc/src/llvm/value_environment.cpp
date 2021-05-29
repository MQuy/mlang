#include "value_environment.h"

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

llvm::Value* ValueEnvironment::lookup(const std::shared_ptr<TokenIdentifier>& identifier)
{
	return lookup(identifier->name);
}

void ValueEnvironment::define(std::string name, llvm::Value* value, bool override)
{
	if (symbols[name] && !override)
		throw std::runtime_error(name + " already exist");

	symbols[name] = value;
}

void ValueEnvironment::define(const std::shared_ptr<TokenIdentifier>& identifier, llvm::Value* value, bool override)
{
	return define(identifier->name, value, override);
}

ValueEnvironment* ValueEnvironment::get_enclosing()
{
	return enclosing;
}
