#ifndef SCAN_TOKEN_H
#define SCAN_TOKEN_H 1

#include <optional>
#include <set>
#include <string>
#include <vector>

enum class TokenName
{
	// reversed words
	tk_auto,
	tk_break,
	tk_case,
	tk_char,
	tk_const,
	tk_continue,
	tk_default,
	tk_do,
	tk_double,
	tk_else,
	tk_enum,
	tk_extern,
	tk_float,
	tk_for,
	tk_goto,
	tk_if,
	tk_inline,
	tk_int,
	tk_long,
	tk_register,
	tk_restrict,
	tk_return,
	tk_short,
	tk_signed,
	tk_sizeof,
	tk_static,
	tk_struct,
	tk_switch,
	tk_typedef,
	tk_union,
	tk_unsigned,
	tk_void,
	tk_volatile,
	tk_while,

	// operator
	tk_equal,				 // =
	tk_equal_equal,			 // ==
	tk_bang,				 // !
	tk_bang_equal,			 // !=
	tk_plus,				 // +
	tk_plus_equal,			 // +=
	tk_plus_plus,			 // ++
	tk_minus,				 // -
	tk_minus_equal,			 // -=
	tk_minus_minus,			 // --
	tk_asterisk,			 // *
	tk_asterisk_equal,		 // *=
	tk_slash,				 // /
	tk_slash_equal,			 // /=
	tk_percent,				 // %
	tk_percent_equal,		 // %=
	tk_ampersand,			 // &
	tk_ampersand_equal,		 // &=
	tk_ampersand_ampersand,	 // &&
	tk_vertical,			 // |
	tk_vertical_equal,		 // |=
	tk_vertical_vertical,	 // ||
	tk_caret,				 // ^
	tk_caret_equal,			 // ^=
	tk_less,				 // <
	tk_less_equal,			 // <=
	tk_much_less,			 // <<
	tk_much_less_equal,		 // <<=
	tk_greater,				 // >
	tk_greater_equal,		 // >=
	tk_much_greater,		 // >>
	tk_much_greater_equal,	 // >>=

	// special symbol
	tk_dot,			   // .
	tk_left_bracket,   // [
	tk_right_bracket,  // ]
	tk_left_paren,	   // (
	tk_right_paren,	   // )
	tk_left_brace,	   // {
	tk_right_brace,	   // }
	tk_comma,		   // ,
	tk_colon,		   // :
	tk_semicolon,	   // ;
	tk_hash,		   // #
	tk_tilde,		   // ~

	// eof
	tk_eof,
};

enum class TokenType
{
	tk_constant,
	tk_symbol,
	tk_identifier,
	tk_invalid,
};

struct SourcePosition
{
	int line;
	int column;
};

class Token
{
public:
	Token(TokenType type)
		: type(type)
	{
	}
	Token(TokenType type, SourcePosition start, SourcePosition end)
		: type(type)
		, start(start)
		, end(end)
	{
	}

protected:
	enum TokenType type;
	struct SourcePosition start;
	struct SourcePosition end;
};

class TokenSymbol : public Token
{
public:
	TokenSymbol(TokenName name)
		: name(name)
		, Token(TokenType::tk_symbol)
	{
	}

private:
	enum TokenName name;  // keyword, opeartor, special symbol and eof
};

class TokenInvalid : public Token
{
public:
	TokenInvalid(char ch)
		: value(ch)
		, Token(TokenType::tk_invalid)
	{
	}

private:
	char value;
};

class TokenIdentifier : public Token
{
public:
	TokenIdentifier(std::string name)
		: name(name)
		, Token(TokenType::tk_identifier)
	{
	}

private:
	std::string name;  // Identifier
};

template <class T>
class TokenLiteral : public Token
{
public:
	TokenLiteral(T value)
		: value(value)
		, Token(TokenType::tk_constant)
	{
	}

private:
	T value;  // constant, string
};

#endif
