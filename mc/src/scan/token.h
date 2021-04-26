#ifndef SCAN_TOKEN_H
#define SCAN_TOKEN_H 1

#include <functional>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

#include "utils.h"

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
	tk_bool,  // _Bool
	tk_int,
	tk_long,
	tk_register,
	tk_restrict,
	tk_return,
	tk_short,
	tk_signed,
	tk_sizeof,
	tk_alignof,
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
	tk_arrow,				 // ->
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
	tk_hash_hash,	   // ##
	tk_tilde,		   // ~
	tk_question_mark,  // ?

	tk_newline,	 // \n
	tk_space,	 //
	tk_tab,		 // \t

	// eof
	tk_eof,
};

enum class TokenType
{
	tk_literal,
	tk_symbol,
	tk_identifier,
	tk_eof,
};

struct SourcePosition
{
	int line;
	int column;

	SourcePosition()
		: line(0)
		, column(0)
	{
	}
	SourcePosition(off_t line_, off_t column_)
		: line(line_)
		, column(column_)
	{
	}
};

class ExprAST;

extern std::unordered_map<std::string, TokenName> keywords;
extern std::unordered_map<TokenName, std::string, EnumClassHash> token_name_str;

struct Token
{
	Token(TokenType type, std::string lexeme)
		: type(type)
		, lexeme(lexeme)
	{
	}
	Token(TokenType type, std::string lexeme, SourcePosition start, SourcePosition end)
		: type(type)
		, lexeme(lexeme)
		, start(start)
		, end(end)
	{
	}

	void set_position(SourcePosition start, SourcePosition end);
	virtual std::shared_ptr<ExprAST> create_ast() = 0;

	bool match(TokenName name, bool strict = false);
	bool match(std::function<bool(TokenName)> comparator, bool strict = false);
	bool match(TokenType type, bool strict = false);
	bool match(std::string name, bool strict = false);

	std::unordered_map<std::string, bool> hide_set;
	TokenType type;
	std::string lexeme;
	SourcePosition start;
	SourcePosition end;
};

struct TokenSymbol : Token
{
	TokenSymbol(TokenName name)
		: Token(TokenType::tk_symbol, token_name_str[name])
		, name(name)
	{
	}

	std::shared_ptr<ExprAST> create_ast() { throw std::runtime_error("not allow to create ast from token symbol"); }

	enum TokenName name;  // keyword, opeartor, special symbol and eof
};

struct TokenIdentifier : Token
{
	TokenIdentifier(std::string name)
		: Token(TokenType::tk_identifier, name)
		, name(name)
	{
	}

	std::shared_ptr<ExprAST> create_ast();

	std::string name;  // identifier
};

template <class T>
struct TokenLiteral : Token
{
	TokenLiteral(T value, std::string lexeme)
		: Token(TokenType::tk_literal, lexeme)
		, value(value)
	{
	}
	TokenLiteral(std::string text, unsigned base);

	std::shared_ptr<ExprAST> create_ast();

	T value;
};

void init_keywords();

#endif
