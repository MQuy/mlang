#include "type_environment.h"

bool TypeEnvironment::contain_type_name(std::string name, bool in_current_scope)
{
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto type_names = scope->type_names;
		if (type_names.find(name) != type_names.end())
			return true;
		else if (in_current_scope)
			break;
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

std::shared_ptr<TypeAST> TypeEnvironment::get_type_type(std::shared_ptr<TokenIdentifier> identifier, bool in_current_scope)
{
	auto name = identifier->name;
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto type_types = scope->type_types;
		if (type_types.find(name) != type_types.end())
			return type_types[name];
		else if (in_current_scope)
			break;
	}
	return nullptr;
}

void TypeEnvironment::define_declarator_name(std::string oldname, std::string newname, bool override)
{
	if (declarator_names.find(oldname) != declarator_names.end() && !override)
		throw std::runtime_error("redefinition of " + newname);

	declarator_names[oldname] = newname;
}

std::string TypeEnvironment::get_declarator_name(std::string name)
{
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto declarator_names = scope->declarator_names;
		if (declarator_names.find(name) != declarator_names.end())
			return declarator_names[name];
	}
	throw std::runtime_error(name + " doesn't exist");
}

std::string TypeEnvironment::generate_declarator_name(std::shared_ptr<TokenIdentifier> identifier, StorageSpecifier storage, FunctionDefinitionAST *func)
{
	if (func && storage == StorageSpecifier::static_)
		return func->name->name + "." + identifier->name;
	else
		return identifier->name;
}

void TypeEnvironment::define_declarator_type(std::shared_ptr<TokenIdentifier> identifier, std::shared_ptr<TypeAST> type, bool override)
{
	if (declarator_types.find(identifier->name) != declarator_types.end() && !override)
		throw std::runtime_error("redefinition of " + identifier->name);

	declarator_types[identifier->name] = type;
}

std::shared_ptr<TypeAST> TypeEnvironment::get_declarator_type(std::shared_ptr<TokenIdentifier> identifier, bool in_current_scope)
{
	auto name = identifier->name;
	for (auto scope = this; scope; scope = scope->enclosing)
	{
		auto declartor_types = scope->declarator_types;
		if (declartor_types.find(name) != declartor_types.end())
			return declartor_types[name];
		else if (in_current_scope)
			break;
	}
	return nullptr;
}
