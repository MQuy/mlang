#ifndef SCAN_LEXER_H
#define SCAN_LEXER_H 1

#include "token.h"
#define TAB_WIDTH 4

class Lexer
{
public:
	Lexer(std::string source)
		: source(source)
		, tokens()
		, current(0)
		, runner(0)
		, column(0)
		, row(0)
	{
	}

	void scan();

private:
	Token scan_token();
	Token scan_character();
	Token scan_string();
	Token scan_number();
	Token scan_word();
	char advance();
	char peek();
	void move_cursor(int distance);
	void new_line();
	void skip_spaces();
	bool match(char ch);

	size_t current;
	size_t runner;
	unsigned column;
	unsigned row;
	std::string source;
	std::vector<Token> tokens;
};

#endif
