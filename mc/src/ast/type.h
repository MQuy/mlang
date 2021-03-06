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
#define NBITS_LONG_DOUBLE 128

#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "scan/token.h"
#include "utils.h"

#define AGGREGATE_ANONYMOUS "anon"

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
		, anonymous(!name)
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
	bool anonymous;
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> members;	 // not support bit fields
};

class EnumTypeAST : public TypeAST
{
public:
	EnumTypeAST(std::shared_ptr<TokenIdentifier> name, std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members, std::set<TypeQualifier> qualifiers = std::set<TypeQualifier>(), StorageSpecifier storage = StorageSpecifier::auto_)
		: TypeAST(TypeKind::enum_)
		, anonymous(!name)
		, name(name)
		, members(members)
		, qualifiers(qualifiers)
		, storage(storage)
	{
	}

	std::shared_ptr<TokenIdentifier> name;
	StorageSpecifier storage;
	std::set<TypeQualifier> qualifiers;
	bool anonymous;
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>>> members;
};

class FunctionTypeAST : public TypeAST
{
public:
	FunctionTypeAST(std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters, std::shared_ptr<TypeAST> returning, bool is_variadic_args = false)
		: TypeAST(TypeKind::function)
		, parameters(parameters)
		, returning(returning)
		, is_variadic_args(is_variadic_args)
	{
	}

	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parameters;
	std::shared_ptr<TypeAST> returning;
	bool is_variadic_args;
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
	expr_sizeof,
	expr_alignof,
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
class SizeOfExprAST;
class AlignOfExprAST;
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
	virtual void *visit_literal_expr(LiteralExprAST<int> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long long> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned int> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned long> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned long long> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<float> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<double> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<long double> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<unsigned char> *expr, void *data = nullptr) = 0;
	virtual void *visit_literal_expr(LiteralExprAST<std::string> *expr, void *data = nullptr) = 0;
	virtual void *visit_identifier_expr(IdentifierExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_binary_expr(BinaryExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_unary_expr(UnaryExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_tenary_expr(TenaryExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_member_access_expr(MemberAccessExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_function_call_expr(FunctionCallExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_typecast_expr(TypeCastExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_sizeof_expr(SizeOfExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_alignof_expr(AlignOfExprAST *expr, void *data = nullptr) = 0;
	virtual void *visit_initializer_expr(InitializerExprAST *expr, void *data = nullptr) = 0;

	virtual void *visit_label_stmt(LabelStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_case_stmt(CaseStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_default_stmt(DefaultStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_expr_stmt(ExprStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_compound_stmt(CompoundStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_if_stmt(IfStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_switch_stmt(SwitchStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_for_stmt(ForStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_while_stmt(WhileStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_dowhile_stmt(DoWhileStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_jump_stmt(JumpStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_continue_stmt(ContinueStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_break_stmt(BreakStmtAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_return_stmt(ReturnStmtAST *stmt, void *data = nullptr) = 0;

	virtual void *visit_function_definition(FunctionDefinitionAST *stmt, void *data = nullptr) = 0;
	virtual void *visit_declaration(DeclarationAST *stmt, void *data = nullptr) = 0;
};
class ASTNode
{
public:
	ASTNode(ASTNodeType node_type)
		: node_type(node_type)
	{
	}
	virtual void *accept(NodeVisitor *visitor, void *data = nullptr) = 0;

	ASTNodeType node_type;
};

void init_types();

extern std::unordered_map<BuiltinTypeName, unsigned> type_nbits;

#endif
