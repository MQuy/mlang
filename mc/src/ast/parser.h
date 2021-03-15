#ifndef AST_PARSER_H
#define AST_PARSER_H 1

#include <functional>
#include <memory>
#include <vector>

#include "expr.h"
#include "scan/token.h"
#include "stmt.h"

class Parser
{
public:
	Parser(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens)
		: tokens(tokens)
		, tokens_length(tokens != nullptr ? tokens->size() : 0)
		, program()
		, current(0)
	{
	}
	std::shared_ptr<Program> parse();

private:
	std::shared_ptr<IfStmtAST> parse_if_stmt();
	std::shared_ptr<ForStmtAST> parse_for_stmt();
	std::shared_ptr<DoWhileStmtAST> parse_do_while_stmt();
	std::shared_ptr<JumpStmtAST> parse_jump_stmt();
	std::shared_ptr<ContinueStmtAST> parse_continue_stmt();
	std::shared_ptr<BreakStmtAST> parse_break_stmt();
	std::shared_ptr<ReturnStmtAST> parse_return_stmt();
	std::shared_ptr<CompoundStmtAST> parse_compound_stmt();

	std::shared_ptr<ExprAST> parse_expr();

	std::pair<std::shared_ptr<DclAST>, std::shared_ptr<TypeAST>> Parser::parse_function_definition();
	std::shared_ptr<TypeAST> parse_declaration_specifiers();
	void parse_storage_or_qualifier(StorageSpecifier &storage_specifier, std::set<TypeQualifier> &type_qualifiers);
	void parse_qualifier(std::set<TypeQualifier> &type_qualifiers);

	std::shared_ptr<ArrayTypeAST> parse_array_type(std::shared_ptr<TypeAST> type);
	std::shared_ptr<std::vector<std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>>>> parser_parameters();
	std::pair<std::shared_ptr<TokenIdentifier>, std::shared_ptr<TypeAST>> parse_declarator(std::shared_ptr<TypeAST> type);

	std::shared_ptr<DclAST> parse_declaration(std::shared_ptr<TypeAST> type);
	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> parser_init_declarator_list();

	std::shared_ptr<Token> advance();
	bool match(TokenName name, bool strict = false);
	bool match(std::string name, bool strict = false);

	long current;
	long runner;
	long tokens_length;
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens;
	std::shared_ptr<Program> program;
};

class ParserError : public std::runtime_error
{
public:
	ParserError(std::string message)
		: std::runtime_error(message)
	{
	}
};

#endif
