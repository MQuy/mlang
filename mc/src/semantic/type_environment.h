#ifndef SEMANTIC_TYPE_ENVIRONMENT_H
#define SEMANTIC_TYPE_ENVIRONMENT_H 1

#include <string>
#include <unordered_map>

#include "ast/type.h"

class TypeEnvironment
{
public:
	TypeEnvironment(TypeEnvironment *enclosing)
		: enclosing(enclosing)
	{
	}

	void define_type(std::string oldname, std::string newname) { type_names[oldname] = newname; }
	void define_variable(std::shared_ptr<TokenIdentifier> identifier, std::shared_ptr<TypeAST> type)
	{
		identifier_types[identifier->name] = type;
	}
	TypeEnvironment *get_enclosing() { return enclosing; }

	bool contain_type_name(std::string name);
	std::string get_type_name(std::string name);
	std::string generate_type_name(std::string name);
	std::shared_ptr<TypeAST> get_identifier_type(std::shared_ptr<TokenIdentifier> identifier);

private:
	TypeEnvironment *enclosing;
	std::unordered_map<std::string, std::string> type_names;
	std::unordered_map<std::string, unsigned> duplicated_type_names;
	std::unordered_map<std::string, std::shared_ptr<TypeAST>> identifier_types;
};

#endif
