#ifndef AST_TRANSLATION_UNIT_H
#define AST_TRANSLATION_UNIT_H 1

#include <memory>
#include <unordered_map>
#include <vector>

#include "expr.h"
#include "scan/token.h"
#include "stmt.h"

class TranslationUnit
{
public:
	TranslationUnit()
		: declarations(std::vector<std::shared_ptr<ExternAST>>())
	{
	}
	std::set<TypeQualifier> get_type_qualifiers(std::shared_ptr<TypeAST> type);
	StorageSpecifier get_storage_specifier(std::shared_ptr<TypeAST> type);

	std::unordered_map<std::string, std::shared_ptr<TypeAST>> types;
	std::vector<std::shared_ptr<ExternAST>> declarations;
};

#endif
