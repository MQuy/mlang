#include "lexer.h"

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
		std::shared_ptr<Token> token = scan_token();
		token->set_position(SourcePosition(current, row), SourcePosition(runner, row));
		tokens.push_back(token);
		move_cursor(1);
		skip_spaces();
	}

	tokens.push_back(std::make_shared<TokenSymbol>(TokenName::tk_eof));
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
	case '.':
		return std::make_shared<TokenSymbol>(TokenName::tk_dot);
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

	case '=':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_equal_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_equal);

	case '!':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_bang_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_bang);

	case '+':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_plus_equal);
		else if (look_ahead_for_match('+'))
			return std::make_shared<TokenSymbol>(TokenName::tk_plus_plus);
		return std::make_shared<TokenSymbol>(TokenName::tk_plus);

	case '-':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_minus_equal);
		else if (look_ahead_for_match('-'))
			return std::make_shared<TokenSymbol>(TokenName::tk_minus_minus);
		return std::make_shared<TokenSymbol>(TokenName::tk_minus);

	case '*':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_asterisk_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_asterisk);

	case '/':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_slash_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_slash);

	case '%':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_percent_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_percent);

	case '&':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_ampersand_equal);
		else if (look_ahead_for_match('&'))
			return std::make_shared<TokenSymbol>(TokenName::tk_ampersand_ampersand);
		return std::make_shared<TokenSymbol>(TokenName::tk_ampersand);

	case '|':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_vertical_equal);
		else if (look_ahead_for_match('|'))
			return std::make_shared<TokenSymbol>(TokenName::tk_vertical_vertical);
		return std::make_shared<TokenSymbol>(TokenName::tk_vertical);

	case '^':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_caret_equal);
		return std::make_shared<TokenSymbol>(TokenName::tk_caret);

	case '<':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_less_equal);
		else if (look_ahead_for_match('<'))
		{
			if (look_ahead_for_match('='))
				return std::make_shared<TokenSymbol>(TokenName::tk_much_less_equal);
			return std::make_shared<TokenSymbol>(TokenName::tk_much_less);
		}
		return std::make_shared<TokenSymbol>(TokenName::tk_less);

	case '>':
		if (look_ahead_for_match('='))
			return std::make_shared<TokenSymbol>(TokenName::tk_greater_equal);
		else if (look_ahead_for_match('>'))
		{
			if (look_ahead_for_match('='))
				return std::make_shared<TokenSymbol>(TokenName::tk_much_greater_equal);
			return std::make_shared<TokenSymbol>(TokenName::tk_much_greater);
		}
		return std::make_shared<TokenSymbol>(TokenName::tk_greater);

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

// TODO: MQ 2021-03-09 Support escape sequence
std::shared_ptr<Token> Lexer::scan_character()
{
	char ch = advance();
	if (!look_ahead_for_match('\''))
		throw UnexpectedToken("' is expected");
	return std::make_shared<TokenLiteral<char>>(ch);
}

// TODO: MQ 2021-03-09 Support escape sequence
std::shared_ptr<Token> Lexer::scan_string()
{
	while (runner < source_length)
	{
		if (advance() == '"')
			return std::make_shared<TokenLiteral<std::string>>(source.substr(current + 1, runner - current - 1));
	}
	throw UnexpectedToken("\" is expected");
}

// TODO: MQ 2021-03-09 Support floating
std::shared_ptr<Token> Lexer::scan_number()
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

std::shared_ptr<Token> Lexer::scan_binary()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return '0' <= nxt_ch && nxt_ch <= '1';
		},
		2);
}

std::shared_ptr<Token> Lexer::scan_octal()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return '0' <= nxt_ch && nxt_ch <= '7';
		},
		8);
}

std::shared_ptr<Token> Lexer::scan_hexadecimal()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return ('0' <= nxt_ch && nxt_ch <= '9') || ('A' <= nxt_ch && nxt_ch <= 'F') || ('a' <= nxt_ch && nxt_ch <= 'f');
		},
		16);
}

std::shared_ptr<Token> Lexer::scan_decimal()
{
	return scan_whole_number(
		[](char nxt_ch) {
			return '0' <= nxt_ch && nxt_ch <= '9';
		},
		10);
}

std::shared_ptr<Token> Lexer::scan_whole_number(std::function<bool(char)> comparator, unsigned base)
{
	while (runner < source_length)
	{
		if (!look_ahead_for_match(comparator))
			break;
	}
	std::string number = source.substr(current, runner - current + 1);

	unsigned u_counter = 0;
	unsigned l_counter = 0;
	while (runner < source_length)
	{
		if (look_ahead_for_match([](char nxt_ch) {
				return nxt_ch == 'u' || nxt_ch == 'U';
			}))
			u_counter++;
		else if (look_ahead_for_match([](char nxt_ch) {
					 return nxt_ch == 'l' || nxt_ch == 'L';
				 }))
			l_counter++;
		else
			break;
	}

	if (u_counter > 1)
		throw UnexpectedToken("unsigned prefix U appears more than one time");
	if (l_counter > 2)
		throw UnexpectedToken("long prefix L appears more than two times");

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

std::shared_ptr<Token> Lexer::scan_word()
{
	while (runner < source_length)
	{
		if (!look_ahead_for_match([](char nxt_ch) {
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

bool Lexer::look_ahead_for_match(char target)
{
	return look_ahead_for_match([&target](char ch) {
		return ch == target;
	});
}

bool Lexer::look_ahead_for_match(std::function<bool(char)> comparator)
{
	if (runner == source_length || !comparator(source.at(runner + 1)))
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
