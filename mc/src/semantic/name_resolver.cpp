#include "name_resolver.h"

bool NameResolver::contain(std::string name)
{
	auto scope = this;
	for (; scope; scope = scope->enclosing)
	{
		auto symbols = scope->names;
		if (symbols.find(name) != symbols.end())
			return true;
	}
	return false;
}

std::string NameResolver::get(std::string name)
{
	auto scope = this;
	for (; scope; scope = scope->enclosing)
	{
		auto symbols = scope->names;
		if (symbols.find(name) != symbols.end())
			return symbols[name];
	}
	throw std::runtime_error(name + " doesn't exist");
}

std::string NameResolver::unique_name(std::string name)
{
	if (name_occurred.find(name) != name_occurred.end())
		name_occurred[name] += 1;
	else
		name_occurred[name] = 0;
	return name + "." + std::to_string(name_occurred[name]);
}
