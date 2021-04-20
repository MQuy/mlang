#ifndef SEMANTIC_NAME_RESOLVER_H
#define SEMANTIC_NAME_RESOLVER_H 1

#include <string>
#include <unordered_map>

#include "ast/type.h"

class NameResolver
{
public:
	NameResolver(NameResolver *enclosing)
		: enclosing(enclosing)
	{
	}

	void define(std::string oldname, std::string newname) { names[oldname] = newname; }
	NameResolver *get_enclosing() { return enclosing; }

	bool contain(std::string name);
	std::string get(std::string name);
	std::string unique_name(std::string name);

private:
	NameResolver *enclosing;
	std::unordered_map<std::string, std::string> names;
	std::unordered_map<std::string, unsigned> name_occurred;
};

#endif
