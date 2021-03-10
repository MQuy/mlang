#include "lexer.h"

#include <sstream>
#include <unordered_map>

std::unordered_map<std::string, TokenName> keywords;

void init_keywords()
{
	keywords["auto"] = TokenName::tk_auto;
	keywords["break"] = TokenName::tk_break;
	keywords["case"] = TokenName::tk_case;
	keywords["char"] = TokenName::tk_char;
	keywords["const"] = TokenName::tk_const;
	keywords["continue"] = TokenName::tk_continue;
	keywords["default"] = TokenName::tk_default;
	keywords["do"] = TokenName::tk_do;
	keywords["double"] = TokenName::tk_double;
	keywords["else"] = TokenName::tk_else;
	keywords["enum"] = TokenName::tk_enum;
	keywords["extern"] = TokenName::tk_extern;
	keywords["float"] = TokenName::tk_float;
	keywords["for"] = TokenName::tk_for;
	keywords["goto"] = TokenName::tk_goto;
	keywords["if"] = TokenName::tk_if;
	keywords["inline"] = TokenName::tk_inline;
	keywords["int"] = TokenName::tk_int;
	keywords["long"] = TokenName::tk_long;
	keywords["register"] = TokenName::tk_register;
	keywords["restrict"] = TokenName::tk_restrict;
	keywords["return"] = TokenName::tk_return;
	keywords["short"] = TokenName::tk_short;
	keywords["signed"] = TokenName::tk_signed;
	keywords["sizeof"] = TokenName::tk_sizeof;
	keywords["static"] = TokenName::tk_static;
	keywords["struct"] = TokenName::tk_struct;
	keywords["switch"] = TokenName::tk_switch;
	keywords["typedef"] = TokenName::tk_typedef;
	keywords["union"] = TokenName::tk_union;
	keywords["unsigned"] = TokenName::tk_unsigned;
	keywords["void"] = TokenName::tk_void;
	keywords["volatile"] = TokenName::tk_volatile;
	keywords["while"] = TokenName::tk_while;
}

void Lexer::reset()
{
	column = 0;
	row = 0;
	current = 0;
	runner = 0;
	tokens->clear();
}

std::shared_ptr<std::vector<std::shared_ptr<Token>>> Lexer::scan()
{
	reset();
	skip_spaces();
	while (current < source_length)
	{
		std::shared_ptr<Token> token = scan_token();
		token->set_position(SourcePosition(current, row), SourcePosition(runner, row));
		tokens->push_back(token);
		move_cursor(1);
		skip_spaces();
	}

	tokens->push_back(std::make_shared<TokenSymbol>(TokenName::tk_eof));
	return tokens;
}

std::shared_ptr<Token> Lexer::scan_token()
{
	char ch = source.at(runner);
	switch (ch)
	{
	case '[':
		return std::make_shared<TokenSymbol>(TokenName::tk_left_bracket);
	case ']':
		return std::make_shared<TokenSymbol>(TokenName::tk_right_bracket);
	case '{':
		return std::make_shared<TokenSymbol>(TokenName::tk_left_brace);
	case '}':
		return std::make_shared<TokenSymbol>(TokenName::tk_right_brace);
	case '(':
		return std::make_shared<TokenSymbol>(TokenName::tk_left_paren);
	case ')':
		return std::make_shared<TokenSymbol>(TokenName::tk_right_paren);
	case ',':
		return std::make_shared<TokenSymbol>(TokenName::tk_comma);
	case ':':
		return std::make_shared<TokenSymbol>(TokenName::tk_colon);
	case ';':
		return std::make_shared<TokenSymbol>(TokenName::tk_semicolon);
	case '#':
		return std::make_shared<TokenSymbol>(TokenName::tk_hash);
	case '~':
		return std::make_shared<TokenSymbol>(TokenName::tk_tilde);
	case '?':
		return std::make_shared<TokenSymbol>(TokenName::tk_question_mark);

	case '=':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_equal_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_equal);

	case '!':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_bang_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_bang);

	case '+':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_plus_equal);
		else if (look_ahead_and_match('+'))
			return std::make_shared<TokenSymbol>(TokenName::tk_plus_plus);
		return std::make_shared<TokenSymbol>(TokenName::tk_plus);

	case '-':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_minus_equal);
		else if (look_ahead_and_match('-'))
			return std::make_shared<TokenSymbol>(TokenName::tk_minus_minus);
		return std::make_shared<TokenSymbol>(TokenName::tk_minus);

	case '*':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_asterisk_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_asterisk);

	case '/':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_slash_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_slash);

	case '%':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_percent_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_percent);

	case '&':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_ampersand_equal);
		else if (look_ahead_and_match('&'))
			return std::make_shared<TokenSymbol>(TokenName::tk_ampersand_ampersand);
		return std::make_shared<TokenSymbol>(TokenName::tk_ampersand);

	case '|':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_vertical_equal);
		else if (look_ahead_and_match('|'))
			return std::make_shared<TokenSymbol>(TokenName::tk_vertical_vertical);
		return std::make_shared<TokenSymbol>(TokenName::tk_vertical);

	case '^':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_caret_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_caret);

	case '<':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_less_equal);
		else if (look_ahead_and_match('<'))
		{
			if (look_ahead_and_match('='))
				return std::make_shared<TokenSymbol>(TokenName::tk_much_less_equal);
			return std::make_shared<TokenSymbol>(TokenName::tk_much_less);
		}
		return std::make_shared<TokenSymbol>(TokenName::tk_less);

	case '>':
		if (look_ahead_and_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_greater_equal);
		else if (look_ahead_and_match('>'))
		{
			if (look_ahead_and_match('='))
				return std::make_shared<TokenSymbol>(TokenName::tk_much_greater_equal);
			return std::make_shared<TokenSymbol>(TokenName::tk_much_greater);
		}
		return std::make_shared<TokenSymbol>(TokenName::tk_greater);

	case '.':
		if (look_ahead([](char nxt_ch) {
				return '0' <= nxt_ch && nxt_ch <= '9';
			}))
		{
			move_cursor(-1);
			return scan_decimal();
		}
		return std::make_shared<TokenSymbol>(TokenName::tk_dot);

	case '\'':
		return scan_character();

	case '"':
		return scan_string();

	default:
		if ('0' <= ch && ch <= '9')
			return scan_number();
		else if (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_')
			return scan_word();
		else
			throw UnexpectedToken(ch + " is not valid");
	}
}

std::shared_ptr<Token> Lexer::scan_character()
{
	char ch = scan_escape_sequences();
	if (!look_ahead_and_match('\''))
		throw UnexpectedToken("' is expected");

	return std::make_shared<TokenLiteral<char>>(ch);
}

std::shared_ptr<Token> Lexer::scan_string()
{
	std::stringstream ss;
	while (runner < source_length)
	{
		if (look_ahead_and_match('"'))
			return std::make_shared<TokenLiteral<std::string>>(ss.str());
		else
			ss << scan_escape_sequences();
	}
	throw UnexpectedToken("\" is expected");
}

char Lexer::scan_escape_sequences()
{
	if (look_ahead_and_match('\\'))
	{
		if (look_ahead_and_match([](char nxt_ch) {
				return nxt_ch == '\'' || nxt_ch == '"' || nxt_ch == '?' || nxt_ch == '\\';
			}))
			return source[runner];
		else if (look_ahead_and_match('a'))
			return '\a';
		else if (look_ahead_and_match('b'))
			return '\b';
		else if (look_ahead_and_match('f'))
			return '\f';
		else if (look_ahead_and_match('n'))
			return '\n';
		else if (look_ahead_and_match('r'))
			return '\r';
		else if (look_ahead_and_match('t'))
			return '\t';
		else if (look_ahead_and_match('v'))
			return '\v';
		else if (look_ahead_and_match([](char nxt_ch) {
					 return '0' <= nxt_ch && nxt_ch <= '9';
				 }))
		{
			unsigned start = runner;
			for (int i = 0; i < 2 && look_ahead_and_match([](char nxt_ch) {
								return '0' <= nxt_ch && nxt_ch <= '7';
							});
				 ++i)
				;
			std::string octal = source.substr(runner, runner - start + 1);
			return strtol(octal.c_str(), nullptr, 8);
		}
		else if (look_ahead_and_match('x'))
		{
			unsigned start = runner + 1;
			for (int i = 0; i < 2 && look_ahead_and_match([](char nxt_ch) {
								return ('0' <= nxt_ch && nxt_ch <= '9') || ('A' <= nxt_ch && nxt_ch <= 'F') || ('a' <= nxt_ch && nxt_ch <= 'f');
							});
				 ++i)
				;
			std::string hex = source.substr(start, runner - start + 1);
			return strtol(hex.c_str(), nullptr, 16);
		}
		else
			throw LexerError("\\" + source.substr(runner, 1) + " is not supported");
	}
	else
	{
		move_cursor(1);
		char ch = source[runner];
		return ch;
	}
}

// TODO: MQ 2021-03-09 Support floating
std::shared_ptr<Token> Lexer::scan_number()
{
	char ch = source.at(current);
	if (ch == '0' && !look_ahead('.'))
	{
		if (look_ahead_and_match([](char nxt_ch) {
				return nxt_ch == 'x' || nxt_ch == 'X';
			}))
			return scan_hexadecimal();
		else if (look_ahead_and_match('b'))
			return scan_binary();
		return scan_octal();
	}
	else
		return scan_decimal();
}

std::shared_ptr<Token> Lexer::scan_binary()
{
	return scan_binary_or_octal(
		[](char nxt_ch) {
			return '0' <= nxt_ch && nxt_ch <= '1';
		},
		2);
}

std::shared_ptr<Token> Lexer::scan_octal()
{
	return scan_binary_or_octal(
		[](char nxt_ch) {
			return '0' <= nxt_ch && nxt_ch <= '7';
		},
		8);
}

std::shared_ptr<Token> Lexer::scan_hexadecimal()
{
	return scan_decimal_or_hexa(
		[](char nxt_ch) {
			return ('0' <= nxt_ch && nxt_ch <= '9') || ('A' <= nxt_ch && nxt_ch <= 'F') || ('a' <= nxt_ch && nxt_ch <= 'f');
		},
		'p',
		16);
}

std::shared_ptr<Token> Lexer::scan_decimal()
{
	return scan_decimal_or_hexa(
		[](char nxt_ch) {
			return '0' <= nxt_ch && nxt_ch <= '9';
		},
		'e',
		10);
}

std::shared_ptr<Token> Lexer::scan_decimal_or_hexa(std::function<bool(char)> comparator, char exponent, unsigned base)
{
	unsigned dot_counter = 0;
	unsigned e_counter = 0;
	while (runner < source_length)
	{
		if (look_ahead_and_match(comparator))
			continue;
		else if (look_ahead_and_match('.'))
		{
			if (e_counter)
				throw LexerError("exponent cannot appear before dot");
			if (dot_counter)
				throw LexerError("dot cannot appear twice");
			dot_counter++;
		}
		else if (look_ahead_and_match(exponent))
		{
			if (e_counter)
				throw LexerError("exponent cannot appear twice");
			e_counter++;

			look_ahead_and_match([](char nxt_ch) {
				return nxt_ch == '+' || nxt_ch == '-';
			});
		}
		else
			break;
	}
	std::string number = source.substr(current, runner - current + 1);
	if (dot_counter || e_counter)
		return scan_fractional_number_suffix(number, base);
	else
		return scan_whole_number_suffix(number, base);
}

std::shared_ptr<Token> Lexer::scan_binary_or_octal(std::function<bool(char)> comparator, unsigned base)
{
	while (runner < source_length)
	{
		if (!look_ahead_and_match(comparator))
			break;
	}
	std::string number = source.substr(current, runner - current + 1);
	return scan_whole_number_suffix(number, base);
}

std::shared_ptr<Token> Lexer::scan_whole_number_suffix(std::string number, unsigned base)
{
	unsigned u_counter = 0;
	unsigned l_counter = 0;
	while (runner < source_length)
	{
		if (look_ahead_and_match([](char nxt_ch) {
				return nxt_ch == 'u' || nxt_ch == 'U';
			}))
		{
			if (u_counter)
				throw UnexpectedToken("unsigned suffix U appears more than one time");
			u_counter++;
		}
		else if (look_ahead_and_match([](char nxt_ch) {
					 return nxt_ch == 'l' || nxt_ch == 'L';
				 }))
		{
			if (l_counter > 1)
				throw UnexpectedToken("long suffix L appears more than two times");
			l_counter++;
		}
		else
			break;
	}

	if (u_counter)
		switch (l_counter)
		{
		case 0:
			return std::make_shared<TokenLiteral<unsigned int>>(number, base);
		case 1:
			return std::make_shared<TokenLiteral<unsigned long>>(number, base);
		case 2:
			return std::make_shared<TokenLiteral<unsigned long long>>(number, base);
		}
	else
		switch (l_counter)
		{
		case 0:
			return std::make_shared<TokenLiteral<int>>(number, base);
		case 1:
			return std::make_shared<TokenLiteral<long>>(number, base);
		case 2:
			return std::make_shared<TokenLiteral<long long>>(number, base);
		}
}

std::shared_ptr<Token> Lexer::scan_fractional_number_suffix(std::string number, unsigned base)
{
	unsigned f_counter = 0;
	unsigned l_counter = 0;
	while (runner < source_length)
	{
		if (look_ahead_and_match([](char nxt_ch) {
				return nxt_ch == 'f' || nxt_ch == 'F';
			}))
		{
			if (l_counter)
				throw LexerError("both suffix f and l appear");
			if (f_counter)
				throw UnexpectedToken("float suffix f appears more than one time");
			f_counter++;
		}
		else if (look_ahead_and_match([](char nxt_ch) {
					 return nxt_ch == 'l' || nxt_ch == 'L';
				 }))
		{
			if (f_counter)
				throw LexerError("both suffix f and l appear");
			if (l_counter)
				throw UnexpectedToken("long suffix l appears more than one time");
			l_counter++;
		}
		else
			break;
	}

	if (f_counter)
		return std::make_shared<TokenLiteral<float>>(number, base);
	else if (l_counter)
		return std::make_shared<TokenLiteral<long double>>(number, base);
	else
		return std::make_shared<TokenLiteral<double>>(number, base);
}

std::shared_ptr<Token> Lexer::scan_word()
{
	while (runner < source_length)
	{
		if (!look_ahead_and_match([](char nxt_ch) {
				return ('0' <= nxt_ch && nxt_ch <= '9')
					   || ('A' <= nxt_ch && nxt_ch <= 'Z')
					   || ('a' <= nxt_ch && nxt_ch <= 'z')
					   || nxt_ch == '_';
			}))
			break;
	};

	std::string word = source.substr(current, runner - current + 1);
	auto name = keywords.find(word);
	if (name != keywords.end())
		return std::make_shared<TokenSymbol>(name->second);
	else
		return std::make_shared<TokenIdentifier>(word);
}

void Lexer::skip_spaces()
{
	while (runner < source_length)
	{
		char ch = source.at(runner);
		if (ch == ' ' || ch == '\t')
			move_cursor(1);
		else if (ch == '\n')
		{
			move_cursor(1);
			new_line();
		}
		else
			break;
	}
	current = runner;
}

bool Lexer::look_ahead_and_match(char target)
{
	return look_ahead_and_match([&target](char nxt_ch) {
		return nxt_ch == target;
	});
}

bool Lexer::look_ahead_and_match(std::function<bool(char)> comparator)
{
	if (runner + 1 == source_length || !comparator(source.at(runner + 1)))
		return false;

	move_cursor(1);
	return true;
}

bool Lexer::look_ahead(char target)
{
	return look_ahead([&target](char nxt_ch) {
		return nxt_ch == target;
	});
}

bool Lexer::look_ahead(std::function<bool(char)> comparator)
{
	return runner + 1 < source_length && comparator(source.at(runner + 1));
}

char Lexer::advance()
{
	move_cursor(1);
	return source.at(runner);
}

void Lexer::move_cursor(int distance)
{
	column += distance;
	runner += distance;
}

void Lexer::new_line()
{
	column = 0;
	row += 1;
}
