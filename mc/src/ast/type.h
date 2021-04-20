#ifndef AST_TYPE_H
#define AST_TYPE_H 1

#define NBITS_BOOL 1
#define NBITS_CHAR 8
#define NBITS_SHORT 16
#define NBITS_INT 32
#define NBITS_LONG 32
#define NBITS_LONG_LONG 64
#define NBITS_FLOAT 32
#define NBITS_DOUBLE 64

#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "scan/token.h"
#include "utils.h"

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
	shift_left,	  // <<
	shift_right,  // >>

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
	prefix_increment,	// ++
	postfix_increment,	// ++
	prefix_decrement,	// --
	postfix_decrement,	// --

	minus,		 // -
	plus,		 // +
	complement,	 // ~

	not_,  // !

	dereference,  // *
	address_of,	  // &

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
	pointer,
	function,
};

class TypeAST
{
public:
	TypeAST(TypeKind kind)
		: kind(kind)
	{
	}
	virtual void relate(std::shared_ptr<TypeAST> type) { assert_not_reached(); }

	bool isInteger();
	bool isSignedInteger();
	bool isUnsignedInteger();
	bool isFloat();
	bool isDouble();
	bool isLongDouble();
	bool isPointer();

	TypeKind kind;
};

class BuiltinTypeAST : public TypeAST
{
public:
	BuiltinTypeAST(BuiltinTypeName name, int size, int align, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>(), StorageSpecifier storage = StorageSpecifier::auto_)
		: TypeAST(TypeKind::builtin)
		, name(name)
		, size(size)
		, align(align)
		, qualifiers(qualifiers)
		, storage(storage)
	{
	}

	int size;
	int align;

	StorageSpecifier storage;
	std::set<TypeQualifier> qualifiers;

	BuiltinTypeName name;
};

class PointerTypeAST : public TypeAST
{
public:
	PointerTypeAST(std::shared_ptr<TypeAST> underlay, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>())
		: TypeAST(TypeKind::pointer)
		, underlay(underlay)
		, qualifiers(qualifiers)
	{
	}
	void relate(std::shared_ptr<TypeAST> type) override { underlay = type; }

	std::shared_ptr<TypeAST> underlay;
	std::set<TypeQualifier> qualifiers;
};

class ArrayTypeAST : public TypeAST
{
public:
	ArrayTypeAST(std::shared_ptr<TypeAST> underlay, std::shared_ptr<ExprAST> expr = nullptr)
		: TypeAST(TypeKind::array)
		, underlay(underlay)
		, expr(expr)
	{
	}
	void relate(std::shared_ptr<TypeAST> type) override { underlay = type; }

	std::shared_ptr<TypeAST> underlay;
	std::shared_ptr<ExprAST> expr;
};

class AliasTypeAST : public TypeAST
{
public:
	AliasTypeAST(std::shared_ptr<TokenIdentifier> name, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>(), StorageSpecifier storage = StorageSpecifier::auto_)
		: TypeAST(TypeKind::alias)
		, name(name)
		, qualifiers(qualifiers)
		, storage(storage)
	{
	}

	std::shared_ptr<TokenIdentifier> name;
	StorageSpecifier storage;
	std::set<TypeQualifier> qualifiers;
};

enum class AggregateKind
{
	struct_,
	union_,
};

class AggregateTypeAST : public TypeAST
{
public:
	AggregateTypeAST(AggregateKind aggregate_kind, std::shared_ptr<TokenIdentifier> name, std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>(), StorageSpecifier storage = StorageSpecifier::auto_)
		: TypeAST(TypeKind::aggregate)
		, aggregate_kind(aggregate_kind)
		, name(name)
		, members(members)
		, qualifiers(qualifiers)
		, storage(storage)
	{
	}

	std::shared_ptr<TokenIdentifier> name;
	AggregateKind aggregate_kind;
	StorageSpecifier storage;
	std::set<TypeQualifier> qualifiers;
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members;	 // not support bit fields
};

class EnumTypeAST : public TypeAST
{
public:
	EnumTypeAST(std::shared_ptr<TokenIdentifier> name, std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>(), StorageSpecifier storage = StorageSpecifier::auto_)
		: TypeAST(TypeKind::enum_)
		, name(name)
		, members(members)
		, qualifiers(qualifiers)
		, storage(storage)
	{
	}

	std::shared_ptr<TokenIdentifier> name;
	StorageSpecifier storage;
	std::set<TypeQualifier> qualifiers;
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

	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters;
	std::shared_ptr<TypeAST> returning;
};

enum ASTNodeType
{
	expr_literal,
	expr_identifier,
	expr_binary,
	expr_unary,
	expr_tenary,
	expr_member_access,
	expr_function_call,
	expr_typecast,
	expr_initializer,
	stmt_label,
	stmt_case,
	stmt_default_,
	stmt_expr,
	stmt_compound,
	stmt_if,
	stmt_switch,
	stmt_for,
	stmt_while,
	stmt_dowhile,
	stmt_jump,
	stmt_continue,
	stmt_break,
	stmt_return,
	extern_declaration,
	extern_function,
};

template <class T>
class LiteralExprAST;
class IdentifierExprAST;
class BinaryExprAST;
class UnaryExprAST;
class TenaryExprAST;
class MemberAccessExprAST;
class FunctionCallExprAST;
class TypeCastExprAST;
class InitializerExprAST;
class LabelStmtAST;
class CaseStmtAST;
class DefaultStmtAST;
class ExprStmtAST;
class CompoundStmtAST;
class IfStmtAST;
class SwitchStmtAST;
class ForStmtAST;
class WhileStmtAST;
class DoWhileStmtAST;
class JumpStmtAST;
class ContinueStmtAST;
class BreakStmtAST;
class ReturnStmtAST;
class FunctionDefinitionAST;
class DeclarationAST;

class NodeVisitor
{
public:
	virtual void *visit_literal_expr(LiteralExprAST<int> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned int> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned long long> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<float> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<double> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long double> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned char> *expr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<std::string> *expr) = 0;
	virtual void *visit_identifier_expr(IdentifierExprAST *expr) = 0;
	virtual void *visit_binary_expr(BinaryExprAST *expr) = 0;
	virtual void *visit_unary_expr(UnaryExprAST *expr) = 0;
	virtual void *visit_tenary_expr(TenaryExprAST *expr) = 0;
	virtual void *visit_member_access_expr(MemberAccessExprAST *expr) = 0;
	virtual void *visit_function_call_expr(FunctionCallExprAST *expr) = 0;
	virtual void *visit_typecast_expr(TypeCastExprAST *expr) = 0;
	virtual void *visit_initializer_expr(InitializerExprAST *expr) = 0;

	virtual void *visit_label_stmt(LabelStmtAST *stmt) = 0;
	virtual void *visit_case_stmt(CaseStmtAST *stmt) = 0;
	virtual void *visit_default_stmt(DefaultStmtAST *stmt) = 0;
	virtual void *visit_expr_stmt(ExprStmtAST *stmt) = 0;
	virtual void *visit_compound_stmt(CompoundStmtAST *stmt) = 0;
	virtual void *visit_if_stmt(IfStmtAST *stmt) = 0;
	virtual void *visit_switch_stmt(SwitchStmtAST *stmt) = 0;
	virtual void *visit_for_stmt(ForStmtAST *stmt) = 0;
	virtual void *visit_while_stmt(WhileStmtAST *stmt) = 0;
	virtual void *visit_dowhile_stmt(DoWhileStmtAST *stmt) = 0;
	virtual void *visit_jump_stmt(JumpStmtAST *stmt) = 0;
	virtual void *visit_continue_stmt(ContinueStmtAST *stmt) = 0;
	virtual void *visit_break_stmt(BreakStmtAST *stmt) = 0;
	virtual void *visit_return_stmt(ReturnStmtAST *stmt) = 0;

	virtual void *visit_function_definition(FunctionDefinitionAST *stmt) = 0;
	virtual void *visit_declaration(DeclarationAST *stmt) = 0;
};
class ASTNode
{
public:
	ASTNode(ASTNodeType node_type)
		: node_type(node_type)
	{
	}
	virtual void *accept(NodeVisitor *visitor) = 0;

	ASTNodeType node_type;
};

void init_types();

extern std::unordered_map<BuiltinTypeName, unsigned> type_nbits;

#endif
