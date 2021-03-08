#ifndef AST_TYPE_H
#define AST_TYPE_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

#include "token.h"

enum class BuiltinTypeName
{
	__identifier(void),
	__identifier(char),
	signed_char,
	__identifier(short),
	__identifier(int),
	__identifier(long),
	long_long,	// signed integer types
	_Bool,
	unsigned_char,
	unsigned_short,
	unsigned_int,
	unsigned_long,
	unsigned_long_long,	 // unsigned integer types
	__identifier(float),
	__identifier(double),
	long_double,
};

enum class TypeQualifier
{
	__identifier(const),
	__identifier(volatile),
	__identifier(restrict),
};

enum class StorageSpecifier
{
	__identifier(auto),
	__identifier(register),
	__identifier(static),
	__identifier(extern),
	__identifier(typedef),
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

	__identifier(and),	// &&
	__identifier(or),	// ||

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

	__identifier(not ),

	dereference,
	address_of,

	cast,
	__identifier(sizeof),
	__identifier(alignof),
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
	__identifier(struct),
	__identifier(union),
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
