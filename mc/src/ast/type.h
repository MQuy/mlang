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
protected:
	int size;
	int align;

	TypeAST *pointer_to;
	TypeAST *array_of;	// TODO: MQ 2021-03-08 VLA?

	std::set<TypeQualifier> qualifiers;
};

class BuiltinTypeAST : TypeAST
{
private:
	BuiltinTypeName name;
};

class AliasTypeAST : TypeAST
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

class AggregateTypeAST : TypeAST
{
private:
	TokenIdentifier name;
	AggregateKind kind;
	std::vector<std::pair<std::optional<std::string>, TypeAST>> members;
};

class EnumTypeAST : TypeAST
{
private:
	TokenIdentifier name;
	std::vector<std::pair<std::string, int>> members;
};

class FunctionTypeAST : TypeAST
{
private:
	std::pair<std::optional<std::string>, TypeAST> parameters;
	TypeAST returning;
};

class ASTNode
{
};

#endif
