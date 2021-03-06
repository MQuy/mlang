#ifndef SEMANTIC_TYPE_ENVIRONMENT_H
#define SEMANTIC_TYPE_ENVIRONMENT_H 1

#include <string>
#include <unordered_map>

#include "ast/stmt.h"
#include "ast/type.h"

class TypeEnvironment
{
public:
	TypeEnvironment(TypeEnvironment *enclosing)
		: enclosing(enclosing)
	{
	}

	TypeEnvironment *get_enclosing() { return enclosing; }

	void define_type_name(std::string oldname, std::string newname) { type_names[oldname] = newname; }
	std::string get_type_name(std::string name);
	bool contain_type_name(std::string name, bool in_current_scope = false);
	std::string generate_type_name(std::string name);

	void define_type_type(std::string name, const std::shared_ptr<TypeAST> &type) { type_types[name] = type; }
	std::shared_ptr<TypeAST> get_type_type(const std::shared_ptr<TokenIdentifier> &identifier, bool in_current_scope = false);

	void define_declarator_name(std::string oldname, std::string newname, bool override = false);
	std::string get_declarator_name(std::string name);
	std::string generate_declarator_name(const std::shared_ptr<TokenIdentifier> &identifier, StorageSpecifier storage, FunctionDefinitionAST *func);

	void define_declarator_type(const std::shared_ptr<TokenIdentifier> &identifier, const std::shared_ptr<TypeAST> &type, bool override = false);
	std::shared_ptr<TypeAST> get_declarator_type(const std::shared_ptr<TokenIdentifier> &identifier, bool in_current_scope = false);

private:
	TypeEnvironment *enclosing;
	std::unordered_map<std::string, std::shared_ptr<TypeAST>> type_types;
	std::unordered_map<std::string, std::string> type_names;
	std::unordered_map<std::string, unsigned> duplicated_type_names;
	std::unordered_map<std::string, std::shared_ptr<TypeAST>> declarator_types;
	std::unordered_map<std::string, std::string> declarator_names;
};

#endif
