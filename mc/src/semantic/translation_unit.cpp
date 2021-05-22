#include "translation_unit.h"

#include <regex>

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
		auto qualifiers = ptype->qualifiers;
		auto original_qualifiers = get_type_qualifiers(ptype->underlay);

		qualifiers.insert(original_qualifiers.begin(), original_qualifiers.end());
		return qualifiers;
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return get_type_qualifiers(atype->underlay);
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto qualifiers = atype->qualifiers;
		auto stype = types[atype->name->name];
		auto original_qualifiers = get_type_qualifiers(stype);

		qualifiers.insert(original_qualifiers.begin(), original_qualifiers.end());
		return qualifiers;
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

std::shared_ptr<TypeAST> TranslationUnit::unalias_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		return unalias_type(get_type(atype->name));
	}
	return type;
}

std::shared_ptr<TypeAST> TranslationUnit::get_type(std::shared_ptr<TypeAST> type)
{
	std::string name;
	if (type->kind == TypeKind::builtin)
		return type;
	if (type->kind == TypeKind::alias)
		name = std::static_pointer_cast<AliasTypeAST>(type)->name->name;
	else if (type->kind == TypeKind::aggregate)
		name = "custom::" + std::static_pointer_cast<AggregateTypeAST>(type)->name->name;
	else if (type->kind == TypeKind::enum_)
		name = "custom::" + std::static_pointer_cast<EnumTypeAST>(type)->name->name;
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		atype->underlay = get_type(atype->underlay);
		return atype;
	}
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<ArrayTypeAST>(type);
		ptype->underlay = get_type(ptype->underlay);
		return ptype;
	}
	else if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);

		for (auto idx = 0; idx < ftype->parameters.size(); ++idx)
		{
			auto [_, ptype] = ftype->parameters[idx];
			ftype->parameters[idx].second = get_type(ptype);
		}

		ftype->returning = get_type(ftype->returning);
		return ftype;
	}

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

	if (type)
	{
		if (type->kind == TypeKind::alias)
			return get_type(type);
		// if name doesn't contain custom:: and type is aggregate -> type looked up by name is alias
		else if (std::regex_match(name, std::regex("^custom::")) == false)
		{
			if (type->kind == TypeKind::aggregate)
			{
				auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
				if (atype->members.size() == 0)
					return get_type(atype);
			}
			else if (type->kind == TypeKind::enum_)
			{
				auto etype = std::static_pointer_cast<EnumTypeAST>(type);
				if (etype->members.size() == 0)
					return get_type(etype);
			}
		}
	}

	return type;
}

std::shared_ptr<TypeAST> TranslationUnit::get_type(std::shared_ptr<TokenIdentifier> identifier)
{
	return get_type(identifier->name);
}

std::shared_ptr<TypeAST> TranslationUnit::get_function_return_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
		return ftype->returning;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		return get_function_return_type(get_type(atype->name));
	}
	return nullptr;
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
	auto original_type1 = get_type(type1);
	auto original_type2 = get_type(type2);
	assert(is_arithmetic_type(original_type1) && is_arithmetic_type(original_type2));

	auto btype1 = std::static_pointer_cast<BuiltinTypeAST>(original_type1);
	auto btype2 = std::static_pointer_cast<BuiltinTypeAST>(original_type2);
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
	auto original_type = get_type(type);
	assert(is_arithmetic_type(original_type));

	auto btype = std::static_pointer_cast<BuiltinTypeAST>(original_type);
	if (is_real_float_type(btype) || is_long_type(btype))
		return type;
	else if (type_nbits[btype->name] < NBITS_INT || btype->name == BuiltinTypeName::int_)
		return get_type("int");
	else
		return get_type("unsigned int");
}

std::shared_ptr<TypeAST> TranslationUnit::composite_type(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2)
{
	auto original_type1 = get_type(type1);
	auto original_type2 = get_type(type2);

	if (is_arithmetic_type(original_type1) && is_arithmetic_type(original_type2))
		return convert_arithmetic_type(original_type1, original_type2);
	else if (is_void_pointer(original_type1))
		return type2;
	else if (is_void_pointer(original_type2))
		return type1;
	else if (is_array_type(original_type1) && is_array_type(original_type2))
	{
		auto atype1 = std::static_pointer_cast<ArrayTypeAST>(original_type1);
		auto atype2 = std::static_pointer_cast<ArrayTypeAST>(original_type2);
		auto underlay = composite_type(atype1->underlay, atype2->underlay);
		std::shared_ptr<ExprAST> expr = nullptr;

		if (!atype1->expr && atype2->expr)
			expr = atype2->expr;
		else if (atype1->expr && !atype2->expr)
			expr = atype1->expr;
		else
		{
			auto expr1 = std::dynamic_pointer_cast<LiteralExprAST<int>>(atype1->expr);
			auto expr2 = std::dynamic_pointer_cast<LiteralExprAST<int>>(atype2->expr);
			if (!(expr1 && expr2 && expr1->value->value == expr2->value->value))
				assert_not_implemented();
		}

		return std::make_shared<ArrayTypeAST>(ArrayTypeAST(underlay, atype2->expr));
	}
	else if (is_function_type(original_type1) && is_function_type(original_type2))
	{
		auto ftype1 = std::static_pointer_cast<FunctionTypeAST>(original_type1);
		auto ftype2 = std::static_pointer_cast<FunctionTypeAST>(original_type2);

		if (ftype1->parameters.size() != ftype2->parameters.size())
			assert_not_reached();

		std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters;
		for (auto idx = 0; idx < ftype1->parameters.size(); ++idx)
		{
			auto [fparam1_name, fparam1_type] = ftype1->parameters[idx];
			auto [fparam2_name, fparam2_type] = ftype2->parameters[idx];

			auto param = std::make_pair(fparam1_name ? fparam1_name : fparam2_name, composite_type(fparam1_type, fparam2_type));
			parameters.push_back(param);
		}
		auto return_type = composite_type(ftype1->returning, ftype2->returning);
		return std::make_shared<FunctionTypeAST>(parameters, return_type);
	}
	else
		assert_not_implemented();
}

std::shared_ptr<TypeAST> TranslationUnit::convert_array_to_pointer(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return std::make_shared<PointerTypeAST>(atype->underlay);
	}
	assert(type->kind == TypeKind::pointer);
	return type;
}

std::shared_ptr<TypeAST> TranslationUnit::convert_function_to_pointer(std::shared_ptr<TypeAST> type)
{
	assert(type->kind == TypeKind::function);
	auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
	return std::make_shared<PointerTypeAST>(ftype);
}

std::shared_ptr<TypeAST> TranslationUnit::duplicate_type_with_qualifier(std::shared_ptr<TypeAST> type, TypeQualifier qualifier, DuplicateTypeAction action)
{
	auto handle_qualifier = [&action, &qualifier](std::set<TypeQualifier> &qualifiers)
	{
		if (action == DuplicateTypeAction::add)
			qualifiers.insert(qualifier);
		else
			qualifiers.erase(qualifier);
	};

	if (type->kind == TypeKind::builtin)
	{
		auto builtin_type = std::static_pointer_cast<BuiltinTypeAST>(type);
		auto qualifiers = get_type_qualifiers(builtin_type);

		handle_qualifier(qualifiers);

		return std::make_shared<BuiltinTypeAST>(
			builtin_type->name,
			builtin_type->size,
			builtin_type->align,
			qualifiers,
			builtin_type->storage);
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto qualifiers = get_type_qualifiers(atype);

		handle_qualifier(qualifiers);

		return std::make_shared<AliasTypeAST>(atype->name, qualifiers, atype->storage);
	}
	else if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		auto qualifiers = get_type_qualifiers(atype);

		handle_qualifier(qualifiers);

		return std::make_shared<AggregateTypeAST>(atype->aggregate_kind, atype->name, atype->members, qualifiers, atype->storage);
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return std::make_shared<ArrayTypeAST>(duplicate_type_with_qualifier(atype->underlay, qualifier, action), atype->expr);
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto etype = std::static_pointer_cast<EnumTypeAST>(type);
		auto qualifiers = get_type_qualifiers(etype);

		handle_qualifier(qualifiers);

		return std::make_shared<EnumTypeAST>(etype->name, etype->members, qualifiers, etype->storage);
	}
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		auto qualifiers = get_type_qualifiers(ptype);

		handle_qualifier(qualifiers);

		return std::make_shared<PointerTypeAST>(ptype->underlay, qualifiers);
	}
	else if (type->kind == TypeKind::function)
		return type;
	else
		assert_not_reached();
}

bool TranslationUnit::is_integer_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::builtin)
	{
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
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = get_type(atype->name);
		return is_integer_type(original_type);
	}
	return is_enum_type(type);
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
		auto original_type = get_type(atype->name);
		return is_signed_integer_type(original_type);
	}
	else
		return is_enum_type(type);
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
		auto original_type = get_type(atype->name);
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
		auto original_type = get_type(atype->name);
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
		auto original_type = get_type(atype->name);
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
		auto original_type = get_type(atype->name);
		return is_double_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_long_type(std::shared_ptr<TypeAST> type)
{
	auto original_type = get_type(type);
	if (original_type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(original_type);
		return btype->name == BuiltinTypeName::long_ || btype->name == BuiltinTypeName::unsigned_long
			   || btype->name == BuiltinTypeName::long_long || btype->name == BuiltinTypeName::unsigned_long_long;
	}

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
		auto original_type = get_type(atype->name);
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
		auto original_type = get_type(atype->name);
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
	// TODO: MQ 2021-05-05 Include complex and imaginary types
	return is_integer_type(type) || is_real_float_type(type);
}

bool TranslationUnit::is_pointer_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::pointer)
		return true;
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = get_type(atype->name);
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
		auto original_type = get_type(atype->name);
		return is_aggregate_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_struct_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::aggregate
		&& std::static_pointer_cast<AggregateTypeAST>(type)->aggregate_kind == AggregateKind::struct_)
		return true;
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = get_type(atype->name);
		return is_struct_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_function_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		return is_function_type(get_type(type));
	}
	return type->kind == TypeKind::function;
}

bool TranslationUnit::is_enum_type(std::shared_ptr<TypeAST> type)
{
	if (type->kind == TypeKind::enum_)
		return true;
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = get_type(atype->name);
		return is_enum_type(original_type);
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
		auto original_type = get_type(atype->name);
		return is_array_type(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_compatible_types(std::shared_ptr<TypeAST> type1, std::shared_ptr<TypeAST> type2)
{
	auto original_type1 = get_type(type1);
	auto original_type2 = get_type(type2);

	if (is_same_types(original_type1, original_type2))
		return true;
	else if (is_pointer_type(original_type1) && is_pointer_type(original_type2))
	{
		auto ptype1 = std::static_pointer_cast<PointerTypeAST>(original_type1);
		auto ptype2 = std::static_pointer_cast<PointerTypeAST>(original_type2);
		return is_compatible_types(ptype1->underlay, ptype2->underlay);
	}
	else if (is_array_type(original_type1) && is_array_type(original_type2))
	{
		auto atype1 = std::static_pointer_cast<ArrayTypeAST>(original_type1);
		auto atype2 = std::static_pointer_cast<ArrayTypeAST>(original_type2);
		// TODO: MQ 2021-05-05 If both have constant size, only if that size is the same
		return is_compatible_types(atype1->underlay, atype2->underlay);
	}
	else if (is_aggregate_type(original_type1) && is_aggregate_type(original_type2))
	{
		auto atype1 = std::static_pointer_cast<AggregateTypeAST>(original_type1);
		auto atype2 = std::static_pointer_cast<AggregateTypeAST>(original_type2);

		if (atype1->aggregate_kind != atype2->aggregate_kind
			|| atype1->name->name != atype2->name->name
			|| atype1->members.size() != atype2->members.size())
			return false;

		// NOTE: MQ 2021-05-05 Only support compatible union with the same order members
		for (auto idx = 0; atype1->members.size(); ++idx)
		{
			auto [mname1, mtype1] = atype1->members[idx];
			auto [mname2, mtype2] = atype2->members[idx];

			if (mname1->name != mname2->name || !is_compatible_types(mtype1, mtype2))
				return false;
		}
		return true;
	}
	else if (is_enum_type(original_type1) && is_enum_type(original_type2))
	{
		auto etype1 = std::static_pointer_cast<EnumTypeAST>(original_type1);
		auto etype2 = std::static_pointer_cast<EnumTypeAST>(original_type2);

		if (etype1->name->name != etype2->name->name || etype1->members.size() != etype2->members.size())
			return false;
	}
	else if (is_function_type(original_type1) && is_function_type(original_type2))
	{
		auto ftype1 = std::static_pointer_cast<FunctionTypeAST>(original_type1);
		auto ftype2 = std::static_pointer_cast<FunctionTypeAST>(original_type2);

		if (ftype1->parameters.size() != ftype2->parameters.size())
			return false;

		for (auto idx = 0; idx < ftype1->parameters.size(); ++idx)
		{
			auto [fparam1_name, fparam1_type] = ftype1->parameters[idx];
			auto [fparam2_name, fparam2_type] = ftype2->parameters[idx];

			if (!is_compatible_types(fparam1_type, fparam1_type))
				return false;
		}
		return is_compatible_types(ftype1->returning, ftype2->returning);
	}

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
		auto utype = ptype->underlay;
		return utype->kind == TypeKind::builtin && std::static_pointer_cast<BuiltinTypeAST>(utype)->name == BuiltinTypeName::void_;
	}
	else if (type->kind == TypeKind::alias)
	{
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		auto original_type = get_type(atype->name);
		return is_void_pointer(original_type);
	}
	else
		return false;
}

bool TranslationUnit::is_volatile_type(std::shared_ptr<TypeAST> type)
{
	auto qualifiers = get_type_qualifiers(type);
	return std::find(qualifiers.begin(), qualifiers.end(), TypeQualifier::volatile_) != qualifiers.end();
}

bool TranslationUnit::is_array_or_pointer_type(std::shared_ptr<TypeAST> type)
{
	return is_array_type(type) || is_pointer_type(type);
}
