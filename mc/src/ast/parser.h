#ifndef AST_PARSER_H
#define AST_PARSER_H 1

#include <functional>
#include <memory>
#include <vector>

#include "expr.h"
#include "name_environment.h"
#include "scan/token.h"
#include "stmt.h"

class Parser
{
public:
	Parser(std::vector<std::shared_ptr<Token>> tokens)
		: tokens(tokens)
		, tokens_length(tokens.size())
		, declarations()
		, current(0)
		, runner(0)
		, environment(new NameEnvironment(nullptr))
	{
	}

	std::vector<std::shared_ptr<ExternAST>> parse();

private:
	std::shared_ptr<StmtAST> parse_stmt();
	std::shared_ptr<LabelStmtAST> parse_label_stmt(const std::shared_ptr<TokenIdentifier> &identifier);
	std::shared_ptr<CaseStmtAST> parse_case_stmt();
	std::shared_ptr<DefaultStmtAST> parse_default_stmt();
	std::shared_ptr<ExprStmtAST> parse_expr_stmt();
	std::shared_ptr<CompoundStmtAST> parse_compound_stmt();
	std::shared_ptr<IfStmtAST> parse_if_stmt();
	std::shared_ptr<SwitchStmtAST> parse_switch_stmt();
	std::shared_ptr<ForStmtAST> parse_for_stmt();
	std::shared_ptr<WhileStmtAST> parse_while_stmt();
	std::shared_ptr<DoWhileStmtAST> parse_do_while_stmt();
	std::shared_ptr<JumpStmtAST> parse_goto_stmt();
	std::shared_ptr<ContinueStmtAST> parse_continue_stmt();
	std::shared_ptr<BreakStmtAST> parse_break_stmt();
	std::shared_ptr<ReturnStmtAST> parse_return_stmt();

	std::shared_ptr<ExprAST> parse_initializer();
	std::shared_ptr<ExprAST> parse_expr();
	std::shared_ptr<ExprAST> parse_assignment_expr();
	std::shared_ptr<ExprAST> parse_tenary_expr();
	std::shared_ptr<ExprAST> parse_logical_or_expr();
	std::shared_ptr<ExprAST> parse_logical_and_expr();
	std::shared_ptr<ExprAST> parse_bitwise_or_expr();
	std::shared_ptr<ExprAST> parse_bitwise_xor_expr();
	std::shared_ptr<ExprAST> parse_bitwise_and_expr();
	std::shared_ptr<ExprAST> parse_equality_expr();
	std::shared_ptr<ExprAST> parse_relational_expr();
	std::shared_ptr<ExprAST> parse_shift_expr();
	std::shared_ptr<ExprAST> parse_additive_expr();
	std::shared_ptr<ExprAST> parse_multiplice_expr();
	std::shared_ptr<ExprAST> parse_cast_expr();
	std::shared_ptr<ExprAST> parse_unary_expr();
	std::shared_ptr<ExprAST> parse_postfix_expr();
	std::shared_ptr<ExprAST> parse_primary_expr();

	std::shared_ptr<ExternAST> parse_function_definition();
	std::shared_ptr<ExternAST> parse_declaration(bool global_scope);
	std::shared_ptr<TypeAST> parse_declaration_specifiers(bool global_scope, bool include_storage = true, bool include_qualifier = true);
	void parse_storage_specifier(StorageSpecifier &storage_specifier);
	void parse_type_qualifier(std::set<TypeQualifier> &type_qualifiers);
	void parse_qualifier(std::set<TypeQualifier> &type_qualifiers);
	std::tuple<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>, std::shared_ptr<ExprAST>> parse_init_declarator(const std::shared_ptr<TypeAST> &type);
	std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>> parse_struct_declaration();
	std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<ExprAST>> parse_enumerator();
	std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>> parse_declarator(const std::shared_ptr<TypeAST> &type, bool abstract = false);
	std::shared_ptr<ArrayTypeAST> parse_declarator_array(const std::shared_ptr<TypeAST> &type);
	std::shared_ptr<std::pair<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>, bool>> parse_declarator_parameters();
	void revert_type_relation(const std::shared_ptr<TypeAST> &source_type, const std::shared_ptr<TypeAST> dest_type, const std::shared_ptr<TypeAST> &anchor_type);
	std::shared_ptr<TypeAST> parse_typename();

	void enter_scope();
	void leave_scope();
	void reset();
	std::shared_ptr<Token> advance();
	std::nullptr_t parse_not_match();
	bool match(TokenName name, bool strict = false, bool advance = true);
	bool match(std::function<bool(TokenName)> comparator, bool strict = false, bool advance = true);
	bool match(std::string name, bool strict = false, bool advance = true);
	bool match(TokenType type, bool strict = false, bool advance = true);
	bool match(std::function<bool(TokenType)> comparator, bool strict = false, bool advance = true);

	long current;
	long runner;
	long tokens_length;
	std::vector<std::shared_ptr<Token>> tokens;
	std::vector<std::shared_ptr<ExternAST>> declarations;
	NameEnvironment *environment;
};

class ParserError : public std::runtime_error
{
public:
	ParserError(std::string message)
		: std::runtime_error(message)
	{
	}
};

void init_operators();

#endif
