#ifndef SCAN_LEXER_H
#define SCAN_LEXER_H 1

#include "token.h"

class Lexer
{
public:
	Lexer(std::string source)
		: source(source)
		, source_length(source.length())
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
	Token Lexer::scan_decimal();
	Token Lexer::scan_hexadecimal();
	Token Lexer::scan_octal();
	Token Lexer::scan_binary();
	Token Lexer::scan_whole_number(std::function<bool(char)> comparator, unsigned base);

	Token scan_word();
	char advance();
	void move_cursor(int distance);
	void new_line();
	void skip_spaces();
	bool look_ahead_for_match(char ch);
	bool Lexer::look_ahead_for_match(std::function<bool(char)> comparator);

	size_t current;
	size_t runner;
	unsigned column;
	unsigned row;
	std::string source;
	size_t source_length;
	std::vector<Token> tokens;
};

class UnexpectedToken : public std::runtime_error
{
public:
	UnexpectedToken(std::string message)
		: std::runtime_error(message)
	{
	}
};

#endif
