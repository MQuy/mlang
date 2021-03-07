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
	TypeAST *array_of;

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

class ExprAST : ASTNode
{
protected:
	std::optional<TypeAST> type;
};

template <class T>
class LiteralExprAST : ExprAST
{
private:
	TokenLiteral<T> token;
};

class IdentifierExprAST : ExprAST  // function and variable
{
private:
	TokenIdentifier name;
};

class BinaryExprAST : ExprAST
{
private:
	ExprAST left;
	ExprAST right;
	BinaryOperator __identifier(opeartor);
};

class UnaryExprAST : ExprAST
{
private:
	ExprAST expr;
	UnaryOperator __identifier(operator);
};

class TenaryExprAST : ExprAST
{
private:
	ExprAST condition;
	ExprAST expr1;
	ExprAST expr2;
};

class MemberAccessExprAST : ExprAST
{
private:
	IdentifierExprAST identifier;
	TokenIdentifier member;
};

class FunctionCallExprAST : ExprAST
{
private:
	IdentifierExprAST callee;
	std::vector<ExprAST> arguments;
};

class TypeCastExprAST : ExprAST
{
private:
	ExprAST expr;
};

class StmtAST : ASTNode
{
};

class ExprStmtAST : ASTNode
{
private:
	ExprAST expr;
};

class LabelStmtAST : StmtAST
{
private:
	TokenIdentifier name;
	StmtAST stmt;
};

class CaseStmtAST : StmtAST
{
private:
	ExprAST constant;
	StmtAST stmt;
};

class DefaultStmtAST : StmtAST
{
private:
	StmtAST stmt;
};

class IfStmtAST : StmtAST
{
private:
	ExprAST condition;
	StmtAST if_body;
	StmtAST else_body;
};

class ForStmtAST : StmtAST	// use for while
{
private:
	StmtAST initializer;
	ExprAST condition;
	StmtAST increment;
	StmtAST body;
};

class JumpStmtAST : StmtAST
{
private:
	TokenIdentifier name;
};

class ReturnStmtAST : StmtAST
{
private:
	std::vector<ExprAST> expr;
};

class BreakStmtAST : StmtAST
{
};

class ContinueStmtAST : StmtAST
{
};

class CompoundStmtAST : StmtAST
{
};

class FunctionDefStmtAST : StmtAST
{
private:
	TokenIdentifier name;
	FunctionTypeAST type;
	StorageSpecifier storage;
	CompoundStmtAST body;
};

class DeclarationStmtAST : StmtAST
{
protected:
	TypeAST type;
	std::optional<StorageSpecifier> storage;
	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> declarators;  // null declarator's type means declaration's type
};

class FunctionProtoStmtAST : DeclarationStmtAST
{
private:
	TokenIdentifier name;
	std::pair<std::optional<std::string>, TypeAST> parameters;
};

class AggregateStmtAST : DeclarationStmtAST
{
private:
	AggregateKind kind;
	std::optional<TokenIdentifier> name;
	std::vector<std::pair<std::optional<std::string>, TypeAST>> members;
};

class EnumStmtAST : DeclarationStmtAST
{
private:
	std::optional<TokenIdentifier> name;
	std::vector<std::pair<std::string, int>> members;
};

class Program
{
private:
	std::vector<StmtAST> stmts;
};
