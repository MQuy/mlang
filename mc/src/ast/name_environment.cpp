#include "name_environment.h"

SymbolType NameEnvironment::lookup(std::string name)
{
	auto scope = this;
	for (; scope; scope = scope->enclosing)
	{
		auto symbols = scope->symbols;
		if (symbols.find(name) != symbols.end())
			return symbols[name];
	}
	throw std::runtime_error(name + " doesnt exist");
}

SymbolType NameEnvironment::lookup(std::shared_ptr<TokenIdentifier> identifier)
{
	return lookup(identifier->name);
}

void NameEnvironment::define(std::string name, SymbolType value)
{
	symbols[name] = value;
}

void NameEnvironment::define(std::shared_ptr<TokenIdentifier> identifier, SymbolType value)
{
	return define(identifier->name, value);
}

bool NameEnvironment::is_typedef(std::shared_ptr<TypeAST> type)
{
	StorageSpecifier storage = StorageSpecifier::auto_;
	if (type->kind == TypeKind::builtin)
	{
		auto btype = std::static_pointer_cast<BuiltinTypeAST>(type);
		storage = btype->storage;
	}
	else if (type->kind == TypeKind::pointer)
	{
		auto ptype = std::static_pointer_cast<PointerTypeAST>(type);
		return is_typedef(ptype->underlay);
	}
	else if (type->kind == TypeKind::array)
	{
		auto atype = std::static_pointer_cast<ArrayTypeAST>(type);
		return is_typedef(atype->underlay);
	}
	else if (type->kind == TypeKind::aggregate)
	{
		auto atype = std::static_pointer_cast<AggregateTypeAST>(type);
		storage = atype->storage;
	}
	else if (type->kind == TypeKind::enum_)
	{
		auto etype = std::static_pointer_cast<EnumTypeAST>(type);
		storage = etype->storage;
	}
	else if (type->kind == TypeKind::function)
	{
		auto ftype = std::static_pointer_cast<FunctionTypeAST>(type);
		return is_typedef(ftype->returning);
	}
	else
	{
		assert(type->kind == TypeKind::alias);
		auto atype = std::static_pointer_cast<AliasTypeAST>(type);
		storage = atype->storage;
	}

	return storage == StorageSpecifier::typedef_;
}

NameEnvironment* NameEnvironment::get_enclosing()
{
	return enclosing;
}
