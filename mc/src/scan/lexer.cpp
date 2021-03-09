#include "lexer.h"

#include <unordered_map>

std::unordered_map<std::string, TokenName> keywords;

void init_keywords()
{
	keywords["auto"] = TokenName::tk_auto;
}

void Lexer::scan()
{
	int length = source.length();

	skip_spaces();
	while (current < length)
	{
		Token token = scan_token();
		tokens.push_back(token);
		skip_spaces();
	}

	tokens.push_back(TokenSymbol(TokenName::tk_eof));
}

Token Lexer::scan_token()
{
	char ch = advance();
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
		if (match('='))
			return TokenSymbol(TokenName::tk_equal_equal);
		return TokenSymbol(TokenName::tk_equal);

	case '!':
		if (match('='))
			return TokenSymbol(TokenName::tk_bang_equal);
		return TokenSymbol(TokenName::tk_bang);

	case '+':
		if (match('='))
			return TokenSymbol(TokenName::tk_plus_equal);
		else if (match('+'))
			return TokenSymbol(TokenName::tk_plus_plus);
		return TokenSymbol(TokenName::tk_plus);

	case '-':
		if (match('='))
			return TokenSymbol(TokenName::tk_minus_equal);
		else if (match('-'))
			return TokenSymbol(TokenName::tk_minus_minus);
		return TokenSymbol(TokenName::tk_minus);

	case '*':
		if (match('='))
			return TokenSymbol(TokenName::tk_asterisk_equal);
		return TokenSymbol(TokenName::tk_asterisk);

	case '/':
		if (match('='))
			return TokenSymbol(TokenName::tk_slash_equal);
		return TokenSymbol(TokenName::tk_slash);

	case '%':
		if (match('='))
			return TokenSymbol(TokenName::tk_percent_equal);
		return TokenSymbol(TokenName::tk_percent);

	case '&':
		if (match('='))
			return TokenSymbol(TokenName::tk_ampersand_equal);
		else if (match('&'))
			return TokenSymbol(TokenName::tk_ampersand_ampersand);
		return TokenSymbol(TokenName::tk_ampersand);

	case '|':
		if (match('='))
			return TokenSymbol(TokenName::tk_vertical_equal);
		else if (match('|'))
			return TokenSymbol(TokenName::tk_vertical_vertical);
		return TokenSymbol(TokenName::tk_vertical);

	case '^':
		if (match('='))
			return TokenSymbol(TokenName::tk_caret_equal);
		return TokenSymbol(TokenName::tk_caret);

	case '<':
		if (match('='))
			return TokenSymbol(TokenName::tk_less_equal);
		else if (match('<'))
		{
			if (match('='))
				return TokenSymbol(TokenName::tk_much_less_equal);
			return TokenSymbol(TokenName::tk_much_less);
		}
		return TokenSymbol(TokenName::tk_less);

	case '>':
		if (match('='))
			return TokenSymbol(TokenName::tk_greater_equal);
		else if (match('>'))
		{
			if (match('='))
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
		return TokenInvalid(ch);
	}
}

// TODO: MQ 2021-03-09 Support escape sequence
Token Lexer::scan_character()
{
	char ch = advance();
	if (!match('\''))
		throw std::runtime_error("' is expected");
	return TokenLiteral<int>(ch);
}

// TODO: MQ 2021-03-09 Support escape sequence
Token Lexer::scan_string()
{
	auto length = source.length();
	while (runner < length)
	{
		if (advance() == '"')
			return TokenLiteral<std::string>(source.substr(current, runner - current - 1));
	}
	throw std::runtime_error("\" is expected");
}

Token Lexer::scan_number()
{
}

Token Lexer::scan_word()
{
	int length = source.length();
	while (runner < length)
	{
		char ch = peek();
		if (('0' <= ch && ch <= '9') || ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') || ch == '_')
			move_cursor(1);
		else
			break;
	};

	auto word = source.substr(current, runner - current - 1);
	auto name = keywords.find(word);
	if (name != keywords.end())
		return TokenSymbol(name->second);
	else
		return TokenIdentifier(word);
}

void Lexer::skip_spaces()
{
	for (off_t length = source.length(); current < length;)
	{
		char& ch = source.at(current);
		if (ch == ' ')
			move_cursor(1);
		else if (ch == '\t')
			move_cursor(TAB_WIDTH);
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

char Lexer::advance()
{
	char ch = source.at(runner);
	move_cursor(1);
	return ch;
}

bool Lexer::match(char target)
{
	if (peek() != target)
		return false;

	move_cursor(1);
	return true;
}

char Lexer::peek()
{
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
