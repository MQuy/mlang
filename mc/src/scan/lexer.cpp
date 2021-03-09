#include "lexer.h"

#include <functional>
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

void Lexer::scan()
{
	skip_spaces();
	while (current < source_length)
	{
		Token token = scan_token();
		tokens.push_back(token);
		move_cursor(1);
		skip_spaces();
	}

	tokens.push_back(TokenSymbol(TokenName::tk_eof));
}

Token Lexer::scan_token()
{
	char ch = source.at(runner);
	switch (ch)
	{
	case '[':
		return TokenSymbol(TokenName::tk_left_bracket);
	case ']':
		return TokenSymbol(TokenName::tk_right_bracket);
	case '{':
		return TokenSymbol(TokenName::tk_left_brace);
	case '}':
		return TokenSymbol(TokenName::tk_right_brace);
	case '(':
		return TokenSymbol(TokenName::tk_left_paren);
	case ')':
		return TokenSymbol(TokenName::tk_right_paren);
	case '.':
		return TokenSymbol(TokenName::tk_dot);
	case ',':
		return TokenSymbol(TokenName::tk_comma);
	case ':':
		return TokenSymbol(TokenName::tk_colon);
	case ';':
		return TokenSymbol(TokenName::tk_semicolon);
	case '#':
		return TokenSymbol(TokenName::tk_hash);
	case '~':
		return TokenSymbol(TokenName::tk_tilde);

	case '=':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_equal_equal);
		return TokenSymbol(TokenName::tk_equal);

	case '!':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_bang_equal);
		return TokenSymbol(TokenName::tk_bang);

	case '+':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_plus_equal);
		else if (look_ahead_for_match('+'))
			return TokenSymbol(TokenName::tk_plus_plus);
		return TokenSymbol(TokenName::tk_plus);

	case '-':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_minus_equal);
		else if (look_ahead_for_match('-'))
			return TokenSymbol(TokenName::tk_minus_minus);
		return TokenSymbol(TokenName::tk_minus);

	case '*':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_asterisk_equal);
		return TokenSymbol(TokenName::tk_asterisk);

	case '/':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_slash_equal);
		return TokenSymbol(TokenName::tk_slash);

	case '%':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_percent_equal);
		return TokenSymbol(TokenName::tk_percent);

	case '&':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_ampersand_equal);
		else if (look_ahead_for_match('&'))
			return TokenSymbol(TokenName::tk_ampersand_ampersand);
		return TokenSymbol(TokenName::tk_ampersand);

	case '|':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_vertical_equal);
		else if (look_ahead_for_match('|'))
			return TokenSymbol(TokenName::tk_vertical_vertical);
		return TokenSymbol(TokenName::tk_vertical);

	case '^':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_caret_equal);
		return TokenSymbol(TokenName::tk_caret);

	case '<':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_less_equal);
		else if (look_ahead_for_match('<'))
		{
			if (look_ahead_for_match('='))
				return TokenSymbol(TokenName::tk_much_less_equal);
			return TokenSymbol(TokenName::tk_much_less);
		}
		return TokenSymbol(TokenName::tk_less);

	case '>':
		if (look_ahead_for_match('='))
			return TokenSymbol(TokenName::tk_greater_equal);
		else if (look_ahead_for_match('>'))
		{
			if (look_ahead_for_match('='))
				return TokenSymbol(TokenName::tk_much_greater_equal);
			return TokenSymbol(TokenName::tk_much_greater);
		}
		return TokenSymbol(TokenName::tk_greater);

	case '\'':
		return scan_character();

	case '"':
		return scan_string();

	case '0' ... '9':
		return scan_number();

	case 'a' ... 'z':
	case 'A' ... 'Z':
	case '_':
		return scan_word();
	default:
		throw UnexpectedToken(ch + " is not valid");
	}
}

// TODO: MQ 2021-03-09 Support escape sequence
Token Lexer::scan_character()
{
	char ch = advance();
	if (!look_ahead_for_match('\''))
		throw UnexpectedToken("' is expected");
	return TokenLiteral<char>(ch);
}

// TODO: MQ 2021-03-09 Support escape sequence
Token Lexer::scan_string()
{
	while (runner < source_length)
	{
		if (advance() == '"')
			return TokenLiteral<std::string>(source.substr(current + 1, runner - current - 1));
	}
	throw UnexpectedToken("\" is expected");
}

// TODO: MQ 2021-03-09 Support floating
Token Lexer::scan_number()
{
	char ch = source.at(current);
	if (ch == '0')
	{
		if (look_ahead_for_match('x'))
			return scan_hexadecimal();
		else if (look_ahead_for_match('b'))
			return scan_binary();
		return scan_octal();
	}
	else
		return scan_decimal();
}

Token Lexer::scan_binary()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return !('0' <= nxt_ch && nxt_ch <= '1');
		},
		2);
}

Token Lexer::scan_octal()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return !('0' <= nxt_ch && nxt_ch <= '7');
		},
		8);
}

Token Lexer::scan_hexadecimal()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return !(('0' <= nxt_ch && nxt_ch <= '9') || ('A' <= nxt_ch && nxt_ch <= 'F') || ('a' <= nxt_ch && nxt_ch <= 'f'));
		},
		16);
}

Token Lexer::scan_decimal()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return !('0' <= nxt_ch && nxt_ch <= '9');
		},
		10);
}

Token Lexer::scan_whole_number(std::function<bool(char)> comparator, unsigned base)
{
	while (runner < source_length)
	{
		if (look_ahead_for_match(comparator))
			break;
	}
	std::string number = source.substr(current, runner - current + 1);

	unsigned u_counter, l_counter;
	while (runner < source_length)
	{
		if (look_ahead_for_match([](char nxt_ch) {
				return nxt_ch == 'u' || nxt_ch == 'U';
			}))
			u_counter++;
		if (look_ahead_for_match([](char nxt_ch) {
				return nxt_ch == 'l' || nxt_ch == 'L';
			}))
			l_counter++;
	}

	if (u_counter > 1)
		throw UnexpectedToken("unsigned prefix U appears more than one time");
	if (l_counter > 2)
		throw UnexpectedToken("long prefix L appears more than two times");

	if (u_counter)
		switch (l_counter)
		{
		case 0:
			return TokenLiteral<unsigned int>(number, base);
		case 1:
			return TokenLiteral<unsigned long>(number, base);
		case 2:
			return TokenLiteral<unsigned long long>(number, base);
		}
	else
		switch (l_counter)
		{
		case 0:
			return TokenLiteral<int>(number, base);
		case 1:
			return TokenLiteral<long>(number, base);
		case 2:
			return TokenLiteral<long long>(number, base);
		}
}

Token Lexer::scan_word()
{
	while (runner < source_length)
	{
		if (look_ahead_for_match([](char nxt_ch) {
				return !(('0' <= nxt_ch && nxt_ch <= '9')
						 || ('A' <= nxt_ch && nxt_ch <= 'Z')
						 || ('a' <= nxt_ch && nxt_ch <= 'z')
						 || nxt_ch == '_');
			}))
			break;
	};

	std::string word = source.substr(current, runner - current + 1);
	auto name = keywords.find(word);
	if (name != keywords.end())
		return TokenSymbol(name->second);
	else
		return TokenIdentifier(word);
}

void Lexer::skip_spaces()
{
	while (current < source_length)
	{
		char ch = source.at(current);
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

bool Lexer::look_ahead_for_match(char target)
{
	return look_ahead_for_match([&target](char ch) {
		return ch == target;
	});
}

bool Lexer::look_ahead_for_match(std::function<bool(char)> comparator)
{
	if (runner == source_length || comparator(source.at(runner + 1)))
		return false;

	move_cursor(1);
	return true;
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
