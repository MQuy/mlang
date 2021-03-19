#include "token.h"

#include "ast/expr.h"

void Token::set_position(SourcePosition start_, SourcePosition end_)
{
	start = start_;
	end = end_;
}

std::shared_ptr<ExprAST> TokenIdentifier::create_ast()
{
	auto identifier = std::make_shared<TokenIdentifier>(TokenIdentifier(name));
	return std::make_shared<ExprAST>(IdentifierExprAST(identifier));
}

template <>
TokenLiteral<int>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtol(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtol(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<long long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtoll(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<unsigned int>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtoul(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<unsigned long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtoul(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<unsigned long long>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtoull(text.c_str(), nullptr, base))
{
}

template <>
TokenLiteral<float>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtof(text.c_str(), nullptr))
{
}

template <>
TokenLiteral<double>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtod(text.c_str(), nullptr))
{
}

template <>
TokenLiteral<long double>::TokenLiteral(std::string text, unsigned base)
	: Token(TokenType::tk_literal)
	, value(strtold(text.c_str(), nullptr))
{
}

template <class T>
std::shared_ptr<ExprAST> TokenLiteral<T>::create_ast()
{
	auto token = std::make_shared<TokenLiteral<T>>(TokenLiteral(value));
	auto ast = LiteralExprAST(token);
	return std::make_shared<LiteralExprAST<T>>(ast);
}

// FIXME: MQ 2021-03-19 why i need explicitly specialize for unsigned char
template <>
std::shared_ptr<ExprAST> TokenLiteral<unsigned char>::create_ast()
{
	auto token = std::make_shared<TokenLiteral<unsigned char>>(TokenLiteral(value));
	auto ast = LiteralExprAST(token);
	return std::make_shared<LiteralExprAST<unsigned char>>(ast);
}

// FIXME: MQ 2021-03-19 why i need explicitly specialize for string
template <>
std::shared_ptr<ExprAST> TokenLiteral<std::string>::create_ast()
{
	auto token = std::make_shared<TokenLiteral<std::string>>(TokenLiteral(value));
	auto ast = LiteralExprAST(token);
	return std::make_shared<LiteralExprAST<std::string>>(ast);
}
