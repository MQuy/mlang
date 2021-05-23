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
