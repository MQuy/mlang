#ifndef AST_TYPE_H
#define AST_TYPE_H 1

#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "scan/token.h"

class ExprAST;

enum class BuiltinTypeName
{
	void_,
	char_,
	signed_char,
	unsigned_char,
	short_,
	unsigned_short,
	int_,
	unsigned_int,
	long_,
	unsigned_long,
	long_long,			 // signed integer types
	unsigned_long_long,	 // unsigned integer types
	_Bool,
	float_,
	double_,
	long_double,
};

enum class TypeQualifier
{
	const_,
	volatile_,
	restrict,
};

enum class StorageSpecifier
{
	auto_,
	register_,
	static_,
	extern_,
	typedef_,
};
enum class BinaryOperator
{
	assignment,				   // =
	addition_assigment,		   // +=
	subtraction_assignment,	   // -=
	multiplication_assigment,  // *=
	division_assignment,	   // /=
	remainder_assignment,	   // %=
	bitwise_and_assigment,	   // &=
	bitwise_or_assigment,	   // |=
	bitwise_xor_assigment,	   // ^=
	shift_left_assignment,	   // <<=
	shift_right_assignment,	   // >>=

	addition,
	subtraction,
	multiplication,
	division,
	remainder,	  // %
	bitwise_and,  // &
	bitwise_or,	  // |
	bitwise_xor,  // ^
	shift_left,	  // >>
	shift_right,  // <<

	and_,  // &&
	or_,   // ||

	equal,			   // ==
	not_equal,		   // !=
	less,			   // <
	greater_than,	   // >
	less_or_equal,	   // <=
	greater_or_equal,  // >=

	array_subscript,  // []
	member_access,	  // .

	comma,	//
};

enum class UnaryOperator
{
	prefix_increment,
	postfix_increment,
	prefix_decrement,
	postfix_decrement,

	minus,
	plus,
	complement,

	not_,

	dereference,
	address_of,

	cast,
	sizeof_,
	alignof_,
};

enum class TypeKind
{
	builtin,
	alias,
	aggregate,
	enum_,
	array,
	function,
};

class TypeAST
{
public:
	TypeAST(TypeKind kind)
		: kind(kind)
	{
	}
	std::shared_ptr<TypeAST> redirect(std::shared_ptr<TypeAST> type);

protected:
	TypeKind kind;
};

class BuiltinTypeAST : public TypeAST
{
public:
	BuiltinTypeAST(std::shared_ptr<BuiltinTypeName> name, int size, int align, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>(), std::shared_ptr<StorageSpecifier> storage = std::make_shared<StorageSpecifier>(StorageSpecifier::auto_))
		: TypeAST(TypeKind::builtin)
		, name(name)
		, size(size)
		, align(align)
		, qualifiers(qualifiers)
		, storage(storage)
	{
	}

private:
	int size;
	int align;

	std::shared_ptr<StorageSpecifier> storage;
	std::set<TypeQualifier> qualifiers;

	std::shared_ptr<BuiltinTypeName> name;
};

class PointerTypeAST : public TypeAST
{
public:
	PointerTypeAST(std::shared_ptr<TypeAST> underlay, std::set<TypeQualifier> qualifiers)
		: TypeAST(TypeKind::array)
		, underlay(underlay)
		, qualifiers(qualifiers)
	{
	}
	std::shared_ptr<TypeAST> consume(std::shared_ptr<TypeAST> type)
	{
		underlay = type;
	}

private:
	std::shared_ptr<TypeAST> underlay;
	std::set<TypeQualifier> qualifiers;
};

class ArrayTypeAST : public TypeAST
{
public:
	ArrayTypeAST(std::shared_ptr<TypeAST> underlay, std::shared_ptr<ExprAST> expr)
		: TypeAST(TypeKind::array)
		, underlay(underlay)
		, expr(expr)
	{
	}
	std::shared_ptr<TypeAST> consume(std::shared_ptr<TypeAST> type)
	{
		underlay = type;
	}

private:
	std::shared_ptr<TypeAST> underlay;
	std::shared_ptr<ExprAST> expr;
};

class AliasTypeAST : public TypeAST
{
public:
	AliasTypeAST(std::shared_ptr<TokenIdentifier> name, std::shared_ptr<TypeAST> underlay = nullptr)
		: TypeAST(TypeKind::alias)
		, name(name)
		, underlay(underlay)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
	std::shared_ptr<TypeAST> underlay;
};

enum class AggregateKind
{
	struct_,
	union_,
};

class AggregateTypeAST : public TypeAST
{
public:
	AggregateTypeAST(AggregateKind kind, std::shared_ptr<TokenIdentifier> name, std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members)
		: TypeAST(TypeKind::aggregate)
		, kind(kind)
		, name(name)
		, members(members)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
	AggregateKind kind;
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members;	 // not support bit fields
};

class EnumTypeAST : public TypeAST
{
public:
	EnumTypeAST(std::shared_ptr<TokenIdentifier> name, std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members)
		: TypeAST(TypeKind::enum_)
		, name(name)
		, members(members)
	{
	}

private:
	std::shared_ptr<TokenIdentifier> name;
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members;
};

class FunctionTypeAST : public TypeAST
{
public:
	FunctionTypeAST(std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters, std::shared_ptr<TypeAST> returning)
		: TypeAST(TypeKind::function)
		, parameters(parameters)
		, returning(returning)
	{
	}

private:
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters;
	std::shared_ptr<TypeAST> returning;
};

class ASTNode
{
};

#endif
