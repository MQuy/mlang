#include "token.h"

#include "ast/expr.h"

std::unordered_map<std::string, TokenName> keywords;
std::unordered_map<TokenName, std::string, EnumClassHash> token_name_str;

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
	keywords["_Bool"] = TokenName::tk_bool;

	for (auto kw : keywords)
		token_name_str[kw.second] = kw.first;
	token_name_str[TokenName::tk_equal] = "=";
	token_name_str[TokenName::tk_equal_equal] = "==";
	token_name_str[TokenName::tk_bang] = "!";
	token_name_str[TokenName::tk_bang_equal] = "!=";
	token_name_str[TokenName::tk_plus] = "+";
	token_name_str[TokenName::tk_plus_equal] = "+=";
	token_name_str[TokenName::tk_plus_plus] = "++";
	token_name_str[TokenName::tk_minus] = "-";
	token_name_str[TokenName::tk_minus_equal] = "-=";
	token_name_str[TokenName::tk_minus_minus] = "--";
	token_name_str[TokenName::tk_arrow] = "->";
	token_name_str[TokenName::tk_asterisk] = "*";
	token_name_str[TokenName::tk_asterisk_equal] = "*=";
	token_name_str[TokenName::tk_slash] = "/";
	token_name_str[TokenName::tk_slash_equal] = "/=";
	token_name_str[TokenName::tk_percent] = "%";
	token_name_str[TokenName::tk_percent_equal] = "%=";
	token_name_str[TokenName::tk_ampersand] = "&";
	token_name_str[TokenName::tk_ampersand_equal] = "&=";
	token_name_str[TokenName::tk_ampersand_ampersand] = "&&";
	token_name_str[TokenName::tk_vertical] = "|";
	token_name_str[TokenName::tk_vertical_equal] = "|=";
	token_name_str[TokenName::tk_vertical_vertical] = "||";
	token_name_str[TokenName::tk_caret] = "^";
	token_name_str[TokenName::tk_caret_equal] = "^=";
	token_name_str[TokenName::tk_less] = "<";
	token_name_str[TokenName::tk_less_equal] = "<=";
	token_name_str[TokenName::tk_much_less] = "<<";
	token_name_str[TokenName::tk_much_less_equal] = "<<=";
	token_name_str[TokenName::tk_greater] = ">";
	token_name_str[TokenName::tk_greater_equal] = ">=";
	token_name_str[TokenName::tk_much_greater] = ">>";
	token_name_str[TokenName::tk_much_greater_equal] = ">>=";
	token_name_str[TokenName::tk_dot] = ".";
	token_name_str[TokenName::tk_left_bracket] = "[";
	token_name_str[TokenName::tk_right_bracket] = "]";
	token_name_str[TokenName::tk_left_paren] = "(";
	token_name_str[TokenName::tk_right_paren] = ")";
	token_name_str[TokenName::tk_left_brace] = "{";
	token_name_str[TokenName::tk_right_brace] = "}";
	token_name_str[TokenName::tk_comma] = ",";
	token_name_str[TokenName::tk_colon] = ":";
	token_name_str[TokenName::tk_semicolon] = ";";
	token_name_str[TokenName::tk_hash] = "#";
	token_name_str[TokenName::tk_hash_hash] = "##";
	token_name_str[TokenName::tk_tilde] = "~";
	token_name_str[TokenName::tk_question_mark] = "?";
	token_name_str[TokenName::tk_newline] = "\n";
	token_name_str[TokenName::tk_space] = " ";
	token_name_str[TokenName::tk_tab] = "\t";
}

void Token::set_position(SourcePosition start_, SourcePosition end_)
{
	start = start_;
	end = end_;
}

bool Token::match(TokenName name, bool strict)
{
	return match([&name](TokenName nxt_name) {
		return nxt_name == name;
	},
				 strict);
}

bool Token::match(std::function<bool(TokenName)> comparator, bool strict)
{
	if (!match(TokenType::tk_symbol))
		return false;

	auto token = (TokenSymbol *)this;
	if (comparator(token->name))
		return true;
	else if (strict)
		throw std::runtime_error("token symbol is not matched");
	else
		return false;
}

bool Token::match(TokenType type, bool strict)
{
	if (this->type == type)
		return true;
	else if (strict)
		throw std::runtime_error("token's type is not matched");
	else
		return false;
}

bool Token::match(std::string name, bool strict)
{
	if (!match(TokenType::tk_identifier))
		return false;

	auto token = (TokenIdentifier *)this;
	if (token->name == name)
		return true;
	else if (strict)
		throw std::runtime_error("token identifier " + token->name + " is not matched with " + name);
	else
		return false;
}

std::shared_ptr<ExprAST> TokenIdentifier::create_ast()
{
	auto identifier = std::make_shared<TokenIdentifier>(TokenIdentifier(name));
	return std::make_shared<IdentifierExprAST>(IdentifierExprAST(identifier));
}

template <>
TokenLiteral<int>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtol(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtol(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<long long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtoll(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<unsigned int>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtoul(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<unsigned long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtoul(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<unsigned long long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtoull(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<float>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtof(text.c_str(), nullptr))
{
}

template <>
TokenLiteral<double>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtod(text.c_str(), nullptr))
{
}

template <>
TokenLiteral<long double>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal, text)
	, value(strtold(text.c_str(), nullptr))
{
}

template <class T>
std::shared_ptr<ExprAST> TokenLiteral<T>::create_ast()
{
	auto token = std::make_shared<TokenLiteral<T>>(TokenLiteral(value, lexeme));
	auto ast = LiteralExprAST(token);
	return std::make_shared<LiteralExprAST<T>>(ast);
}

// FIXME: MQ 2021-03-19 why i need explicitly specialize for unsigned char
template <>
std::shared_ptr<ExprAST> TokenLiteral<unsigned char>::create_ast()
{
	auto token = std::make_shared<TokenLiteral<unsigned char>>(TokenLiteral(value, lexeme));
	auto ast = LiteralExprAST(token);
	return std::make_shared<LiteralExprAST<unsigned char>>(ast);
}

// FIXME: MQ 2021-03-19 why i need explicitly specialize for string
template <>
std::shared_ptr<ExprAST> TokenLiteral<std::string>::create_ast()
{
	auto token = std::make_shared<TokenLiteral<std::string>>(TokenLiteral(value, lexeme));
	auto ast = LiteralExprAST(token);
	return std::make_shared<LiteralExprAST<std::string>>(ast);
}
