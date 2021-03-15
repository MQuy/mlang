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
	Parser(std::shared_ptr<std::vector<Token>> tokens)
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
	std::shared_ptr<FunctionDefStmtAST> parse_function_def_stmt(std::shared_ptr<DclAST>);

	std::vector<std::tuple<std::string, std::optional<TypeAST>, std::optional<ExprAST>>> parser_init_declarator_list();
	std::shared_ptr<DclAST> parse_declaration_specifiers();
	void Parser::parse_storage_or_qualifier(StorageSpecifier &storage_specifier, std::set<TypeQualifier> &type_qualifiers);
	std::shared_ptr<DclAST> parse_type_specifier(std::shared_ptr<TokenSymbol> storage, std::shared_ptr<TokenSymbol> qualifier);

	bool assert_token(TokenSymbol symbol);
	bool look_ahead_and_match(TokenName name);
	bool look_ahead_and_match(std::function<bool(TokenName)> comparator);

	long current;
	long tokens_length;
	std::shared_ptr<std::vector<Token>> tokens;
	std::shared_ptr<Program> program;
};

#endif
