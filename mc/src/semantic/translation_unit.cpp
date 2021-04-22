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

std::shared_ptr<TypeAST> TranslationUnit::get_type(std::shared_ptr<TokenIdentifier> identifier)
{
	return get_type(identifier->name);
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
	assert(is_aggregate_type(type1) && is_aggregate_type(type2));

	auto btype1 = std::static_pointer_cast<BuiltinTypeAST>(type1);
	auto btype2 = std::static_pointer_cast<BuiltinTypeAST>(type2);
	if (btype1->name == btype2->name)
		return btype1;
	else if (is_long_double_type(btype1) || is_long_double_type(btype2))
		return get_type("long double");
	else if (is_double_type(btype1) || is_double_type(btype2))
		return get_type("double");
	else if (is_float_type(btype1) || is_float_type(btype2))
		return get_type("float");
	else if ((is_signed_integer_type(btype1) && is_signed_integer_type(btype2))
			 || (is_unsigned_integer_type(btype1) && is_unsigned_integer_type(btype2)))
		return type_nbits[btype1->name] > type_nbits[btype2->name] ? btype1 : btype2;
	else
	{
		std::shared_ptr<BuiltinTypeAST> utype = nullptr;
		std::shared_ptr<BuiltinTypeAST> stype = nullptr;
		if (is_unsigned_integer_type(btype1))
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
	assert(is_arithmetic_type(type));

	auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
	if (is_real_float_type(btype))
		return type;
	else if (type_nbits[btype->name] < NBITS_INT || btype->name == BuiltinTypeName::int_)
		return get_type("int");
	else
		return get_type("unsigned int");
}

std::shared_ptr<TypeAST> TranslationUnit::composite_type(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2)
{
	assert_not_implemented();
	return nullptr;
}

bool TranslationUnit::is_integer_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind != TypeKind::builtin)
		return false;
	auto builtin_type = std::static_pointer_cast<BuiltinTypeAST>(type);
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

bool TranslationUnit::is_signed_integer_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto builtin_type = std::static_pointer_cast<BuiltinTypeAST>(type);
		return builtin_type->name == BuiltinTypeName::char_
			   || builtin_type->name == BuiltinTypeName::signed_char
			   || builtin_type->name == BuiltinTypeName::short_
			   || builtin_type->name == BuiltinTypeName::int_
			   || builtin_type->name == BuiltinTypeName::long_
			   || builtin_type->name == BuiltinTypeName::long_long;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_signed_integer_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_unsigned_integer_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto builtin_type = std::static_pointer_cast<BuiltinTypeAST>(type);
		return builtin_type->name == BuiltinTypeName::_Bool
			   || builtin_type->name == BuiltinTypeName::unsigned_char
			   || builtin_type->name == BuiltinTypeName::unsigned_short
			   || builtin_type->name == BuiltinTypeName::unsigned_int
			   || builtin_type->name == BuiltinTypeName::unsigned_long
			   || builtin_type->name == BuiltinTypeName::unsigned_long_long;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_unsigned_integer_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_real_float_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->name == BuiltinTypeName::float_
			   || btype->name == BuiltinTypeName::double_
			   || btype->name == BuiltinTypeName::long_double;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_real_float_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_float_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->name == BuiltinTypeName::float_;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_float_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_double_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->name == BuiltinTypeName::double_;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_double_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_long_double_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->name == BuiltinTypeName::long_double;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_long_double_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_void_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		return btype->name == BuiltinTypeName::void_;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_void_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_real_type(std::shared_ptr<TypeAST> type)
{
	return is_integer_type(type) || is_real_float_type(type);
}

bool TranslationUnit::is_arithmetic_type(std::shared_ptr<TypeAST> type)
{
	return is_integer_type(type) || is_real_float_type(type);
}

bool TranslationUnit::is_pointer_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::pointer)
		return true;
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_pointer_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_scalar_type(std::shared_ptr<TypeAST> type)
{
	return is_arithmetic_type(type) || is_pointer_type(type);
}

bool TranslationUnit::is_aggregate_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::aggregate)
		return true;
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_aggregate_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_array_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::array)
		return true;
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = types[atype->name->name];
		return is_array_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_compatible_types(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2)
{
	assert_not_implemented();
	return false;
}

bool TranslationUnit::is_same_types(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2)
{
	if (type1->kind == type2->kind)
	{
		if (type1->kind == TypeKind::builtin)
		{
			auto btype1 = std::static_pointer_cast<BuiltinTypeAST>(type1);
			auto btype2 = std::static_pointer_cast<BuiltinTypeAST>(type2);
			return btype1->name == btype2->name;
		}
		else if (type1->kind == TypeKind::pointer)
		{
			auto ptype1 = std::static_pointer_cast<PointerTypeAST>(type1);
			auto ptype2 = std::static_pointer_cast<PointerTypeAST>(type2);
			return is_same_types(ptype1->underlay, ptype2->underlay);
		}
		else if (type1->kind == TypeKind::array)
		{
			auto atype1 = std::static_pointer_cast<ArrayTypeAST>(type1);
			auto atype2 = std::static_pointer_cast<ArrayTypeAST>(type2);
			return is_same_types(atype1->underlay, atype2->underlay);
		}
		else if (type1->kind == TypeKind::alias)
		{
			auto atype1 = std::static_pointer_cast<AliasTypeAST>(type1);
			auto atype2 = std::static_pointer_cast<AliasTypeAST>(type2);
			return is_same_types(get_type(atype1->name), get_type(atype2->name));
		}
		else if (type1->kind == TypeKind::aggregate)
		{
			auto atype1 = std::static_pointer_cast<AggregateTypeAST>(type1);
			auto atype2 = std::static_pointer_cast<AggregateTypeAST>(type2);
			return atype1->aggregate_kind == atype2->aggregate_kind && atype1->name->name == atype2->name->name;
		}
		else if (type1->kind == TypeKind::enum_)
		{
			auto etype1 = std::static_pointer_cast<EnumTypeAST>(type1);
			auto etype2 = std::static_pointer_cast<EnumTypeAST>(type2);
			return etype1->name == etype1->name;
		}
		else if (type1->kind == TypeKind::function)
		{
			auto ftype1 = std::static_pointer_cast<FunctionTypeAST>(type1);
			auto ftype2 = std::static_pointer_cast<FunctionTypeAST>(type2);

			if (ftype1->parameters.size() != ftype2->parameters.size())
				return false;
			for (auto i = 0; i < ftype1->parameters.size(); ++i)
			{
				auto ptype1 = std::get<1>(ftype1->parameters[i]);
				auto ptype2 = std::get<1>(ftype2->parameters[i]);
				if (!is_same_types(ptype1, ptype2))
					return false;
			}
			return is_same_types(ftype1->returning, ftype2->returning);
		}
		else
			assert_not_reached();
	}
	return false;
}

bool TranslationUnit::is_null_pointer(std::shared_ptr<TypeAST> type, std::shared_ptr<ExprAST> expr)
{
	if (is_integer_type(type) || is_void_pointer(type))
	{
		auto constant = std::dynamic_pointer_cast<LiteralExprAST<int>>(expr);
		return constant && constant->value->value == 0;
	}
	return false;
}

bool TranslationUnit::is_void_pointer(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		return is_void_type(ptype->underlay);
	}
	return false;
}
