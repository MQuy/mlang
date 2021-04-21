#include "type_environment.h"

bool TypeEnvironment::contain_type_name(std::string name)
{
	auto scope = this;
	for (; scope; scope = scope->enclosing)
	{
		auto symbols = scope->type_names;
		if (symbols.find(name) != symbols.end())
			return true;
	}
	return false;
}

std::string TypeEnvironment::get_type_name(std::string name)
{
	auto scope = this;
	for (; scope; scope = scope->enclosing)
	{
		auto symbols = scope->type_names;
		if (symbols.find(name) != symbols.end())
			return symbols[name];
	}
	throw std::runtime_error(name + " doesn't exist");
}

std::string TypeEnvironment::generate_type_name(std::string name)
{
	if (duplicated_type_names.find(name) != duplicated_type_names.end())
		duplicated_type_names[name] += 1;
	else
		duplicated_type_names[name] = 0;
	return name + "." + std::to_string(duplicated_type_names[name]);
}
