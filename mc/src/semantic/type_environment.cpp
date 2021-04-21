#include "type_environment.h"

bool TypeEnvironment::contain_type_name(std::string name)
{
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto type_names = scope->type_names;
		if (type_names.find(name) != type_names.end())
			return true;
	}
	return false;
}

std::string TypeEnvironment::get_type_name(std::string name)
{
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto type_names = scope->type_names;
		if (type_names.find(name) != type_names.end())
			return type_names[name];
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

std::shared_ptr<TypeAST> TypeEnvironment::get_identifier_type(std::shared_ptr<TokenIdentifier> identifier)
{
	auto name = identifier->name;
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto variable_types = scope->identifier_types;
		if (variable_types.find(name) != variable_types.end())
			return variable_types[name];
	}
	throw std::runtime_error(name + " doesn't exist");
}
