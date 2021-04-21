#include "translation_unit.h"

std::set<TypeQualifier> TranslationUnit::get_type_qualifiers(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->qualifiers;
	}
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		return get_type_qualifiers(ptype->underlay);
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return get_type_qualifiers(atype->underlay);
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto stype = types[atype->name->name];
		return get_type_qualifiers(stype);
	}
	else if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		return atype->qualifiers;
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto etype = std::static_pointer_cast<EnumTypeAST>(type);
		return etype->qualifiers;
	}
	else if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
		return get_type_qualifiers(ftype->returning);
	}
	else
		assert_not_reached();
}

StorageSpecifier TranslationUnit::get_storage_specifier(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->storage;
	}
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		return get_storage_specifier(ptype->underlay);
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return get_storage_specifier(atype->underlay);
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		return atype->storage;
	}
	else if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		return atype->storage;
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto etype = std::static_pointer_cast<EnumTypeAST>(type);
		return etype->storage;
	}
	else if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
		return get_storage_specifier(ftype->returning);
	}
	else
		assert_not_reached();
}

std::shared_ptr<TypeAST> TranslationUnit::unbox_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin
		|| type->kind == TypeKind::aggregate
		|| type->kind == TypeKind::enum_)
		return type;
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		return unbox_type(ptype->underlay);
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return unbox_type(atype->underlay);
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto stype = types[atype->name->name];
		return unbox_type(stype);
	}
	else if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
		return unbox_type(ftype->returning);
	}
	else
		assert_not_reached();
}

std::shared_ptr<TypeAST> TranslationUnit::get_type(std::shared_ptr<TypeAST> type_ast)
{
	std::string name;
	if (type_ast->kind == TypeKind::alias)
		name = std::static_pointer_cast<AliasTypeAST>(type_ast)->name->name;
	else if (type_ast->kind == TypeKind::aggregate)
		name = "custom::" + std::static_pointer_cast<AggregateTypeAST>(type_ast)->name->name;
	else if (type_ast->kind == TypeKind::enum_)
		name = "custom::" + std::static_pointer_cast<EnumTypeAST>(type_ast)->name->name;

	return get_type(name);
}

std::shared_ptr<TypeAST> TranslationUnit::get_type(BuiltinTypeName name)
{
	std::string type_name;
	if (name == BuiltinTypeName::_Bool)
		type_name = "_Bool";
	else if (name == BuiltinTypeName::char_)
		type_name = "char";
	else if (name == BuiltinTypeName::unsigned_char)
		type_name = "unsigned char";
	else if (name == BuiltinTypeName::signed_char)
		type_name = "signed char";
	else if (name == BuiltinTypeName::short_)
		type_name = "short";
	else if (name == BuiltinTypeName::unsigned_short)
		type_name = "unsigned short";
	else if (name == BuiltinTypeName::int_)
		type_name = "int";
	else if (name == BuiltinTypeName::unsigned_int)
		type_name = "unsigned int";
	else if (name == BuiltinTypeName::long_)
		type_name = "long";
	else if (name == BuiltinTypeName::unsigned_long)
		type_name = "unsigned long";
	else if (name == BuiltinTypeName::long_long)
		type_name = "long long";
	else if (name == BuiltinTypeName::unsigned_long_long)
		type_name = "unsigned long long";
	else if (name == BuiltinTypeName::float_)
		type_name = "float";
	else if (name == BuiltinTypeName::double_)
		type_name = "double";
	else if (name == BuiltinTypeName::long_double)
		type_name = "long double";
	else
		assert_not_reached();

	return get_type(type_name);
}

std::shared_ptr<TypeAST> TranslationUnit::get_type(std::string name)
{
	auto type = types[name];
	if (!type)
		throw std::runtime_error(name + " doesn't not exist");
	return type;
}

void TranslationUnit::add_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		types["custom::" + atype->name->name] = atype;
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		types["custom::" + atype->name->name] = atype;
	}
	else
		assert_not_reached();
}

void TranslationUnit::add_type(std::string name, std::shared_ptr<TypeAST> type)
{
	types[name] = type;
}

std::shared_ptr<TypeAST> TranslationUnit::convert_arithmetic_type(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2)
{
	assert(type1->isAggregate() && type2->isAggregate());

	auto btype1 = std::static_pointer_cast<BuiltinTypeAST>(type1);
	auto btype2 = std::static_pointer_cast<BuiltinTypeAST>(type2);
	if (btype1->name == btype2->name)
		return btype1;
	else if (btype1->isLongDouble() || btype2->isLongDouble())
		return get_type("long double");
	else if (btype1->isDouble() || btype2->isDouble())
		return get_type("double");
	else if (btype1->isFloat() || btype2->isFloat())
		return get_type("float");
	else if ((btype1->isSignedInteger() && btype2->isSignedInteger())
			 || (btype1->isUnsignedInteger() && btype2->isUnsignedInteger()))
		return type_nbits[btype1->name] > type_nbits[btype2->name] ? btype1 : btype2;
	else
	{
		std::shared_ptr<BuiltinTypeAST> utype = nullptr;
		std::shared_ptr<BuiltinTypeAST> stype = nullptr;
		if (btype1->isUnsignedInteger())
		{
			utype = btype1;
			stype = btype2;
		}
		else
		{
			utype = btype2;
			stype = btype1;
		}
		if (type_nbits[utype->name] >= type_nbits[stype->name])
			return utype;
		else
			return stype;
	}
}

std::shared_ptr<TypeAST> TranslationUnit::promote_integer(std::shared_ptr<TypeAST> type)
{
	assert(type->isArithmetic());

	auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
	if (btype->isFloat() || btype->isDouble() || btype->isLongDouble())
		return type;
	else if (type_nbits[btype->name] < NBITS_INT || btype->name == BuiltinTypeName::int_)
		return get_type("int");
	else
		return get_type("unsigned int");
}
