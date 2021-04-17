#include "type.h"

std::unordered_map<BuiltinTypeName, unsigned> type_nbits;

void init_types()
{
	type_nbits[BuiltinTypeName::_Bool] = NBITS_BOOL;
	type_nbits[BuiltinTypeName::char_] = NBITS_CHAR;
	type_nbits[BuiltinTypeName::signed_char] = NBITS_CHAR;
	type_nbits[BuiltinTypeName::unsigned_char] = NBITS_CHAR;
	type_nbits[BuiltinTypeName::short_] = NBITS_SHORT;
	type_nbits[BuiltinTypeName::unsigned_short] = NBITS_SHORT;
	type_nbits[BuiltinTypeName::int_] = NBITS_INT;
	type_nbits[BuiltinTypeName::unsigned_int] = NBITS_INT;
	type_nbits[BuiltinTypeName::long_] = NBITS_LONG;
	type_nbits[BuiltinTypeName::unsigned_long] = NBITS_LONG;
	type_nbits[BuiltinTypeName::long_long] = NBITS_LONG_LONG;
	type_nbits[BuiltinTypeName::unsigned_long_long] = NBITS_LONG_LONG;
}

bool TypeAST::isInteger()
{
	if (kind != TypeKind::builtin)
		return false;
	auto builtin_type = (BuiltinTypeAST *)this;
	return builtin_type->name == BuiltinTypeName::_Bool
		   || builtin_type->name == BuiltinTypeName::char_
		   || builtin_type->name == BuiltinTypeName::unsigned_char
		   || builtin_type->name == BuiltinTypeName::signed_char
		   || builtin_type->name == BuiltinTypeName::short_
		   || builtin_type->name == BuiltinTypeName::unsigned_short
		   || builtin_type->name == BuiltinTypeName::int_
		   || builtin_type->name == BuiltinTypeName::unsigned_int
		   || builtin_type->name == BuiltinTypeName::long_
		   || builtin_type->name == BuiltinTypeName::unsigned_long
		   || builtin_type->name == BuiltinTypeName::long_long
		   || builtin_type->name == BuiltinTypeName::unsigned_long_long;
}

bool TypeAST::isSignedInteger()
{
	if (kind != TypeKind::builtin)
		return false;
	auto builtin_type = (BuiltinTypeAST *)this;
	return builtin_type->name == BuiltinTypeName::char_
		   || builtin_type->name == BuiltinTypeName::signed_char
		   || builtin_type->name == BuiltinTypeName::short_
		   || builtin_type->name == BuiltinTypeName::int_
		   || builtin_type->name == BuiltinTypeName::long_
		   || builtin_type->name == BuiltinTypeName::long_long;
}

bool TypeAST::isUnsignedInteger()
{
	if (kind != TypeKind::builtin)
		return false;
	auto builtin_type = (BuiltinTypeAST *)this;
	return builtin_type->name == BuiltinTypeName::_Bool
		   || builtin_type->name == BuiltinTypeName::unsigned_char
		   || builtin_type->name == BuiltinTypeName::unsigned_short
		   || builtin_type->name == BuiltinTypeName::unsigned_int
		   || builtin_type->name == BuiltinTypeName::unsigned_long
		   || builtin_type->name == BuiltinTypeName::unsigned_long_long;
}

bool TypeAST::isFloat()
{
	return kind == TypeKind::builtin && ((BuiltinTypeAST *)this)->name == BuiltinTypeName::float_;
}

bool TypeAST::isDouble()
{
	return kind == TypeKind::builtin && ((BuiltinTypeAST *)this)->name == BuiltinTypeName::double_;
}

bool TypeAST::isLongDouble()
{
	return kind == TypeKind::builtin && ((BuiltinTypeAST *)this)->name == BuiltinTypeName::long_double;
}

bool TypeAST::isPointer()
{
	return kind == TypeKind::pointer;
}
