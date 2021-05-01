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
		types["void"] = std::make_shared<BuiltinTypeAST>(BuiltinTypeName::void_, 0, 0);
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
	std::shared_ptr<TypeAST> unbox_type(std::shared_ptr<TypeAST> type);

	bool is_integer_type(std::shared_ptr<TypeAST> type);
	bool is_signed_integer_type(std::shared_ptr<TypeAST> type);
	bool is_unsigned_integer_type(std::shared_ptr<TypeAST> type);
	bool is_real_float_type(std::shared_ptr<TypeAST> type);
	bool is_float_type(std::shared_ptr<TypeAST> type);
	bool is_double_type(std::shared_ptr<TypeAST> type);
	bool is_long_double_type(std::shared_ptr<TypeAST> type);
	bool is_void_type(std::shared_ptr<TypeAST> type);
	bool is_real_type(std::shared_ptr<TypeAST> type);
	bool is_arithmetic_type(std::shared_ptr<TypeAST> type);
	bool is_pointer_type(std::shared_ptr<TypeAST> type);
	bool is_scalar_type(std::shared_ptr<TypeAST> type);
	bool is_array_type(std::shared_ptr<TypeAST> type);
	bool is_aggregate_type(std::shared_ptr<TypeAST> type);
	bool is_struct_type(std::shared_ptr<TypeAST> type);
	bool is_function_type(std::shared_ptr<TypeAST> type);
	bool is_compatible_types(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2);
	bool is_same_types(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2);
	bool is_null_pointer(std::shared_ptr<TypeAST> type, std::shared_ptr<ExprAST> expr);
	bool is_void_pointer(std::shared_ptr<TypeAST> type);

	std::shared_ptr<TypeAST> get_type(std::shared_ptr<TypeAST> type);
	std::shared_ptr<TypeAST> get_type(BuiltinTypeName name);
	std::shared_ptr<TypeAST> get_type(std::shared_ptr<TokenIdentifier> identifier);
	std::shared_ptr<TypeAST> get_type(std::string name);
	std::shared_ptr<TypeAST> get_function_return_type(std::shared_ptr<TypeAST> type);
	void add_type(std::shared_ptr<TypeAST> type);
	void add_type(std::string name, std::shared_ptr<TypeAST> type);
	std::shared_ptr<TypeAST> convert_arithmetic_type(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2);
	std::shared_ptr<TypeAST> promote_integer(std::shared_ptr<TypeAST> type);
	std::shared_ptr<TypeAST> composite_type(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2);
	std::shared_ptr<TypeAST> convert_array_to_pointer(std::shared_ptr<TypeAST> type);
	std::shared_ptr<TypeAST> convert_function_to_pointer(std::shared_ptr<TypeAST> type);

	// struct or union -> aggregate::xxx
	// alias -> same
	// builtin -> same
	std::unordered_map<std::string, std::shared_ptr<TypeAST>> types;
	std::vector<std::shared_ptr<ExternAST>> declarations;
};

#endif
