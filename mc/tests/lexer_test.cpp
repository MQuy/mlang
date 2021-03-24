#include "src/scan/lexer.h"

#include <regex>

#include "gtest/gtest.h"

std::string characters_set_without_quote = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_[]{}(),:;#~=!+-*/%&|^<>.?";
std::string supported_characters_set = characters_set_without_quote + "\'\"";

TEST(SpecialSymbol, Standalone_FollowingByAnyCharacter_NotCombine)
{
	init_keywords();

	std::vector<std::pair<char, TokenName>> standalones = {
		std::make_pair('[', TokenName::tk_left_bracket),
		std::make_pair(']', TokenName::tk_right_bracket),
		std::make_pair('{', TokenName::tk_left_brace),
		std::make_pair('}', TokenName::tk_right_brace),
		std::make_pair('(', TokenName::tk_left_paren),
		std::make_pair(')', TokenName::tk_right_paren),
		std::make_pair(',', TokenName::tk_comma),
		std::make_pair(':', TokenName::tk_colon),
		std::make_pair(';', TokenName::tk_semicolon),
		std::make_pair('#', TokenName::tk_hash),
		std::make_pair('~', TokenName::tk_tilde),
		std::make_pair('?', TokenName::tk_question_mark),
		std::make_pair('\n', TokenName::tk_newline),
	};
	for (auto s : standalones)
	{
		for (auto c : characters_set_without_quote)
		{
			std::stringstream text;
			text << s.first << c;
			Lexer lexer = Lexer(text.str());
			auto tokens = lexer.scan();
			auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
			ASSERT_EQ(token->name, s.second);
		}
	}
}

TEST(Operator, Equal_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("==t");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_equal_equal);
}

TEST(Operator, Equal_FollowedByNonEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), ""))
	{
		std::string text("=");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_equal);
	}
}

TEST(Operator, Bang_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("!=>");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_bang_equal);
}

TEST(Operator, Bang_FollowedByNonEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), ""))
	{
		std::string text("!");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_bang);
	}
}

TEST(Operator, Plus_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("+=h");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_plus_equal);
}

TEST(Operator, Plus_FollowedByPlus_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("++=");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_plus_plus);
}

TEST(Operator, Plus_FollowedByNonPlusOrEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\+=]"), ""))
	{
		std::string text("+");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_plus);
	}
}

TEST(Operator, Minus_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("-=a");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_minus_equal);
}

TEST(Operator, Minus_FollowedByMinus_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("--=");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_minus_minus);
}

TEST(Operator, Minus_FollowedByGreaterThan_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("->=");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_arrow);
}

TEST(Operator, Mius_FollowedByNonMinusOrEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[->=]"), ""))
	{
		std::string text("-");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_minus);
	}
}

TEST(Operator, Asterisk_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("*=+");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_asterisk_equal);
}

TEST(Operator, Asterisk_FollowedByNonEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), ""))
	{
		std::string text("*");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_asterisk);
	}
}

TEST(Operator, Slash_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("/=(");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_slash_equal);
}

TEST(Operator, Slash_FollowedByNonEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[=\/\*]"), ""))
	{
		std::string text("/");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_slash);
	}
}

TEST(Operator, Percent_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("%=)");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_percent_equal);
}

TEST(Operator, Percent_FollowedByNonEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), ""))
	{
		std::string text("%");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_percent);
	}
}

TEST(Operator, Ampersand_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("&=^");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_ampersand_equal);
}

TEST(Operator, Ampersand_FollowedByAmbersand_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("&&=");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_ampersand_ampersand);
}

TEST(Operator, Ampersand_FollowedByNonAmpersandOrEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[&=]"), ""))
	{
		std::string text("&");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_ampersand);
	}
}

TEST(Operator, Vertical_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("|=#");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_vertical_equal);
}

TEST(Operator, Vertical_FollowedByVertical_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("||=K");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_vertical_vertical);
}

TEST(Operator, Vertical_FollowedByNonVerticalOrEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\|=]"), ""))
	{
		std::string text("|");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_vertical);
	}
}

TEST(Operator, Caret_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("^=*");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_caret_equal);
}

TEST(Operator, Caret_FollowedByNonEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), ""))
	{
		std::string text("^");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_caret);
	}
}

TEST(Operator, Less_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("<=9");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_less_equal);
}

TEST(Operator, Less_FollowedByLess_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer("<<=;");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_much_less_equal);
}

TEST(Operator, Less_FollowedByLess_FollowedByNonEqual_Combine)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\<=]"), ""))
	{
		std::string text("<<");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_much_less);
	}
}

TEST(Operator, Less_FollowedByNonLessOrEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\<=]"), ""))
	{
		std::string text("<");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_less);
	}
}

TEST(Operator, Greater_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer(">= ");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_greater_equal);
}

TEST(Operator, Greater_FollowedByGreater_FollowedByEqual_Combine)
{
	init_keywords();

	Lexer lexer = Lexer(">>=p");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_much_greater_equal);
}

TEST(Operator, Greater_FollowedByGreater_FollowedByNonEqual_Combine)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\>=]"), ""))
	{
		std::string text(">>");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_much_greater);
	}
}

TEST(Operator, Greater_FollowedByNonGreaterOrEqual_Standalone)
{
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\>=]"), ""))
	{
		std::string text(">");
		Lexer lexer = Lexer(text + c);
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_greater);
	}
}

TEST(Operator, Dot_FollowedByNonDigit_Standalone)
{
	init_keywords();

	Lexer lexer = Lexer(".=");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenSymbol>(tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_dot);
}

TEST(Literal, Dot_FollowedByDigit_Standalone)
{
	init_keywords();

	Lexer lexer = Lexer(".012+x");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<double>>(tokens->front());
	ASSERT_EQ(token->value, 0.012);
}

TEST(Literal, Dot_FollowedByDigitAndExponent_Standalone)
{
	init_keywords();

	Lexer lexer = Lexer(".012e5;");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<double>>(tokens->front());
	ASSERT_EQ(token->value, 0.012e5);
}

TEST(Literal, CharacterConstant_OctalEscapeSequence_FullForm)
{
	init_keywords();

	Lexer lexer = Lexer("'\\012'5");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<char>>(tokens->front());
	ASSERT_EQ(token->value, 012);
}

TEST(Literal, CharacterConstant_OctalEscapeSequence_ShortForm)
{
	init_keywords();

	Lexer lexer = Lexer("'\\01'>");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<char>>(tokens->front());
	ASSERT_EQ(token->value, 01);
}

TEST(Literal, CharacterConstant_HexaEscapeSequence_FullForm)
{
	init_keywords();

	Lexer lexer = Lexer("'\\xfa'*");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>>(tokens->front());
	ASSERT_EQ(token->value, 0xfa);
}

TEST(Literal, CharacterConstant_HexaEscapeSequence_ShortForm)
{
	init_keywords();

	Lexer lexer = Lexer("'\\x5'(");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>>(tokens->front());
	ASSERT_EQ(token->value, 0x5);
}

TEST(Literal, CharacterConstant_Newline)
{
	init_keywords();

	Lexer lexer = Lexer("'\\n'?");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>>(tokens->front());
	ASSERT_EQ(token->value, '\n');
}

TEST(Literal, CharacterConstant_Printable)
{
	init_keywords();

	Lexer lexer = Lexer("'a';");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>>(tokens->front());
	ASSERT_EQ(token->value, 'a');
}

TEST(Literal, StringConstant_HexaEscapeSequence_FullForm)
{
	init_keywords();

	Lexer lexer = Lexer("\"a\\xfabb\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>>(tokens->front());
	ASSERT_EQ(token->value,
			  "a\xfa"
			  "bb");
}

TEST(Literal, StringConstant_HexaEscapeSequence_ShortForm)
{
	init_keywords();

	Lexer lexer = Lexer("\"a\\xamf\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>>(tokens->front());
	ASSERT_EQ(token->value,
			  "a\xa"
			  "mf");
}

TEST(Literal, StringConstant_OctalEscapeSequence_FullForm)
{
	init_keywords();

	Lexer lexer = Lexer("\"a\\0121f\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>>(tokens->front());
	ASSERT_EQ(token->value, "a\0121f");
}

TEST(Literal, StringConstant_OctalEscapeSequence_ShortForm)
{
	init_keywords();

	Lexer lexer = Lexer("\"a\\01f1\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>>(tokens->front());
	ASSERT_EQ(token->value, "a\01f1");
}

TEST(Literal, WholeNumber_Decimal_WithoutAnyThing)
{
	init_keywords();

	Lexer lexer = Lexer("121+1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->front());
	ASSERT_EQ(token->value, 121);
}

TEST(Literal, WholeNumber_Decimal_WithUnsignedSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121u>1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned int>>(tokens->front());
	ASSERT_EQ(token->value, 121u);
}

TEST(Literal, WholeNumber_Decimal_WithLongSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121l - 10");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<long>>(tokens->front());
	ASSERT_EQ(token->value, 121l);
}

TEST(Literal, WholeNumber_Decimal_WithLongLongSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121ll;");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<long long>>(tokens->front());
	ASSERT_EQ(token->value, 121ll);
}

TEST(Literal, WholeNumber_Decimal_WithUnsignedLongSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121ul/x");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned long>>(tokens->front());
	ASSERT_EQ(token->value, 121ul);
}

TEST(Literal, WholeNumber_Decimal_WithUnsignedLongLongSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121ull|9");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned long long>>(tokens->front());
	ASSERT_EQ(token->value, 121ull);
}

TEST(Literal, WholeNumber_Decimal_WithLongUnsignedSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121lu!x");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned long>>(tokens->front());
	ASSERT_EQ(token->value, 121ul);
}

TEST(Literal, WholeNumber_Decimal_WithLongLongUnsignedSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("121llu,2");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned long long>>(tokens->front());
	ASSERT_EQ(token->value, 121ull);
}

TEST(Literal, WholeNumber_Binary)
{
	init_keywords();

	Lexer lexer = Lexer("0b10&1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->front());
	ASSERT_EQ(token->value, 0b10);
}

TEST(Literal, WholeNumber_Octal)
{
	init_keywords();

	Lexer lexer = Lexer("017~k");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->front());
	ASSERT_EQ(token->value, 017);
}

TEST(Literal, WholeNumber_Hexadecimal)
{
	init_keywords();

	Lexer lexer = Lexer("0x1f;");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->front());
	ASSERT_EQ(token->value, 0x1f);
}

TEST(Literal, FloatNumber_Decimal_WithAnything)
{
	init_keywords();

	Lexer lexer = Lexer("1.15+1 ");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<double>>(tokens->front());
	ASSERT_EQ(token->value, 1.15);
}

TEST(Literal, FloatNumber_Decimal_WithFloatSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("1.15f a");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<float>>(tokens->front());
	ASSERT_EQ(token->value, 1.15f);
}

TEST(Literal, FloatNumber_Decimal_WithLongSuffix)
{
	init_keywords();

	Lexer lexer = Lexer("1.15l?");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<long double>>(tokens->front());
	ASSERT_EQ(token->value, 1.15l);
}

TEST(Literal, FloatNumber_Decimal_WithExponent)
{
	init_keywords();

	Lexer lexer = Lexer("1.15e+15 ? ");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<double>>(tokens->front());
	ASSERT_EQ(token->value, 1.15e+15);
}

TEST(Literal, FloatNumber_Hexadecimal_WithFloatSuffixAndExponent)
{
	init_keywords();

	Lexer lexer = Lexer("0x1.15p-2f + w");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<float>>(tokens->front());
	ASSERT_EQ(token->value, 0x1.15p-2f);
}

TEST(Literal, FloatNumber_Hexadecimal_WithLongSuffixAndExponent)
{
	init_keywords();

	Lexer lexer = Lexer("0x1.15p+2l");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<long double>>(tokens->front());
	ASSERT_EQ(token->value, 0x1.15p+2l);
}

TEST(Symbol, ReserverdWords_WithSpaceAsSeparator)
{
	auto keywords = init_keywords();

	for (auto iter : *keywords.get())
	{
		Lexer lexer = Lexer(iter.first + " 123");
		auto tokens = lexer.scan();
		auto token = std::static_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, iter.second);
	}
}

TEST(Symbol, ReserverdWords_WithSemicolonAsSeparator)
{
	auto keywords = init_keywords();

	for (auto iter : *keywords.get())
	{
		Lexer lexer = Lexer(iter.first + ";");
		auto tokens = lexer.scan();
		auto token = std::static_pointer_cast<TokenSymbol>(tokens->front());
		ASSERT_EQ(token->name, iter.second);
	}
}

TEST(Identifier, Name_WithKeywordNamePlusRandomString)
{
	auto keywords = init_keywords();

	for (auto iter : *keywords.get())
	{
		Lexer lexer = Lexer(iter.first + "123");
		auto tokens = lexer.scan();
		auto token = std::static_pointer_cast<TokenIdentifier>(tokens->front());
		ASSERT_EQ(token->name, iter.first + "123");
	}
}

TEST(Identifier, Name_StartedWithUnderscore)
{
	init_keywords();

	Lexer lexer = Lexer("_he;");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenIdentifier>(tokens->front());
	ASSERT_EQ(token->name, "_he");
}

TEST(Identifier, Name_WithBodyHasNumber)
{
	init_keywords();

	Lexer lexer = Lexer("_he1,");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenIdentifier>(tokens->front());
	ASSERT_EQ(token->name, "_he1");
}

TEST(Ignored, Slash_AtTheEndOfLine)
{
	init_keywords();

	Lexer lexer = Lexer("/\n1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->front());
	ASSERT_EQ(token->value, 1);
}

TEST(Ignored, Comment_SingleLine)
{
	init_keywords();

	Lexer lexer = Lexer("// hello world\n1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->at(1));
	ASSERT_EQ(token->value, 1);
}

TEST(Ignored, Comment_MultiLines)
{
	init_keywords();

	Lexer lexer = Lexer("/* hello world */1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<int>>(tokens->front());
	ASSERT_EQ(token->value, 1);
}
