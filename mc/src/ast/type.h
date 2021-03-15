#ifndef AST_TYPE_H
#define AST_TYPE_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

#include "scan/token.h"

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

class TypeAST
{
public:
	TypeAST(int size, int align, TypeAST *pointer_to = nullptr, TypeAST *array_of = nullptr, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>())
		: size(size)
		, align(align)
		, pointer_to(pointer_to)
		, array_of(array_of)
		, qualifiers(qualifiers)
	{
	}

protected:
	int size;
	int align;

	TypeAST *pointer_to;
	TypeAST *array_of;	// TODO: MQ 2021-03-08 Support VLA

	std::set<TypeQualifier> qualifiers;
};

class BuiltinTypeAST : public TypeAST
{
public:
	BuiltinTypeAST(BuiltinTypeName name, int size, int align, TypeAST *pointer_to = nullptr, TypeAST *array_of = nullptr, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>())
		: name(name)
		, TypeAST(size, align, nullptr, nullptr, qualifiers)
	{
	}

private:
	BuiltinTypeName name;
};

class AliasTypeAST : public TypeAST
{
private:
	TokenIdentifier name;
	TypeAST underlay;
};

enum class AggregateKind
{
	struct_,
	union_,
};

class AggregateTypeAST : public TypeAST
{
private:
	TokenIdentifier name;
	AggregateKind kind;
	std::vector<std::pair<std::optional<std::string>, TypeAST>> members;
};

class EnumTypeAST : public TypeAST
{
private:
	TokenIdentifier name;
	std::vector<std::pair<std::string, int>> members;
};

class FunctionTypeAST : public TypeAST
{
private:
	std::pair<std::optional<std::string>, TypeAST> parameters;
	TypeAST returning;
};

class ASTNode
{
};

#endif
