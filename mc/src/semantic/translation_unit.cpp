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
		auto stype = types[atype->name->name];
		return get_storage_specifier(stype);
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

std::shared_ptr<TypeAST> TranslationUnit::get_type(std::shared_ptr<TypeAST> type_ast)
{
	std::string name;
	if (type_ast->kind == TypeKind::alias)
		name = std::static_pointer_cast<AliasTypeAST>(type_ast)->name->name;
	else if (type_ast->kind == TypeKind::aggregate)
		name = "aggregate::" + std::static_pointer_cast<AggregateTypeAST>(type_ast)->name->name;

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
