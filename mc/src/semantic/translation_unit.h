#ifndef AST_TRANSLATION_UNIT_H
#define AST_TRANSLATION_UNIT_H 1

#include <memory>
#include <unordered_map>
#include <vector>

#include "ast/expr.h"
#include "ast/stmt.h"
#include "scan/token.h"

class TranslationUnit
{
public:
	TranslationUnit(std::vector<std::shared_ptr<ExternAST>> declarations = std::vector<std::shared_ptr<ExternAST>>())
		: declarations(declarations)
	{
		types["_Bool"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::_Bool, NBITS_BOOL, NBITS_BOOL);
		types["char"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::char_, NBITS_CHAR, NBITS_CHAR);
		types["unsigned char"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::signed_char, NBITS_CHAR, NBITS_CHAR);
		types["signed char"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::unsigned_char, NBITS_CHAR, NBITS_CHAR);
		types["short"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::short_, NBITS_SHORT, NBITS_SHORT);
		types["unsigned short"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::unsigned_short, NBITS_SHORT, NBITS_SHORT);
		types["int"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::int_, NBITS_INT, NBITS_INT);
		types["unsigned int"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::unsigned_int, NBITS_INT, NBITS_INT);
		types["long"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::long_, NBITS_LONG, NBITS_LONG);
		types["unsigned long"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::unsigned_long, NBITS_LONG, NBITS_LONG);
		types["long long"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::long_long, NBITS_LONG_LONG, NBITS_LONG_LONG);
		types["unsigned long long"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::unsigned_long_long, NBITS_LONG_LONG, NBITS_LONG_LONG);
		types["float"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::float_, NBITS_LONG_LONG, NBITS_LONG_LONG);
		types["double"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::double_, NBITS_LONG_LONG, NBITS_LONG_LONG);
		types["long double"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::long_double, NBITS_LONG_LONG, NBITS_LONG_LONG);
		types["const char *"] = std::make_shared<PointerTypeAST>(types["char"], std::set<TypeQualifier>{TypeQualifier::const_});
	}
	std::set<TypeQualifier> get_type_qualifiers(std::shared_ptr<TypeAST> type);
	StorageSpecifier get_storage_specifier(std::shared_ptr<TypeAST> type);

	std::shared_ptr<TypeAST> get_type(std::shared_ptr<TypeAST> type);
	std::shared_ptr<TypeAST> get_type(BuiltinTypeName name);
	std::shared_ptr<TypeAST> get_type(std::string name);

	// struct or union -> aggregate::xxx
	// alias -> same
	// builtin -> same
	std::unordered_map<std::string, std::shared_ptr<TypeAST>> types;
	std::vector<std::shared_ptr<ExternAST>> declarations;
};

#endif
