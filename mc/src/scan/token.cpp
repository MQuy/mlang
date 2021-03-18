#include "token.h"

void Token::set_position(SourcePosition start_, SourcePosition end_)
{
	start = start_;
	end = end_;
}

TokenNumber<char>::TokenNumber(char ch)
	: TokenLiteral(LiteralType::char_)
	, value(ch)
{
}

TokenNumber<unsigned char>::TokenNumber(unsigned char ch)
	: TokenLiteral(LiteralType::unsigned_char)
	, value(ch)
{
}

TokenNumber<int>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::int_)
	, value(strtol(text.c_str(), nullptr, base))
{
}

TokenNumber<long>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::long_)
	, value(strtol(text.c_str(), nullptr, base))
{
}

TokenNumber<long long>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::long_long)
	, value(strtoll(text.c_str(), nullptr, base))
{
}

TokenNumber<unsigned int>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::unsigned_int)
	, value(strtoul(text.c_str(), nullptr, base))
{
}

TokenNumber<unsigned long>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::unsigned_long)
	, value(strtoul(text.c_str(), nullptr, base))
{
}

TokenNumber<unsigned long long>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::unsigned_long_long)
	, value(strtoull(text.c_str(), nullptr, base))
{
}

TokenNumber<float>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::float_)
	, value(strtof(text.c_str(), nullptr))
{
}

TokenNumber<double>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::double_)
	, value(strtod(text.c_str(), nullptr))
{
}

TokenNumber<long double>::TokenNumber(std::string text, unsigned base)
	: TokenLiteral(LiteralType::long_double)
	, value(strtold(text.c_str(), nullptr))
{
}
