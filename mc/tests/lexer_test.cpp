#include "gtest/gtest.h"
#include "src/scan/lexer.h"
#include <regex>

std::string characters_set_without_quote = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_[]{}(),:;#~=!+-*/%&|^<>.?";
std::string supported_characters_set = characters_set_without_quote + "\'\"";

TEST(SpecialSymbol, Standalone_FollowingByAnyCharacter_NotCombine) {
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
	};
	for (auto s : standalones) {
		for (auto c : characters_set_without_quote) {
			std::stringstream text;
			text << s.first << c;
			Lexer lexer = Lexer(text.str());
			auto tokens = lexer.scan();
			auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
			ASSERT_EQ(token->name, s.second);
		}
	}
}

TEST(Operator, Equal_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("==+1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_equal_equal);
}

TEST(Operator, Equal_FollowedByNonEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), "")) {
		std::string text("=");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_equal);
	}
}

TEST(Operator, Bang_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("!=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_bang_equal);
}

TEST(Operator, Bang_FollowedByNonEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), "")) {
		std::string text("!");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_bang);
	}
}

TEST(Operator, Plus_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("+=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_plus_equal);
}

TEST(Operator, Plus_FollowedByPlus_Combine) {
	init_keywords();

	Lexer lexer = Lexer("++=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_plus_plus);
}

TEST(Operator, Plus_FollowedByNonPlusOrEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\+=]"), "")) {
		std::string text("+");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_plus);
	}
}

TEST(Operator, Minus_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("-=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_minus_equal);
}

TEST(Operator, Minus_FollowedByMinus_Combine) {
	init_keywords();

	Lexer lexer = Lexer("--=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_minus_minus);
}

TEST(Operator, Mius_FollowedByNonMinusOrEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[-=]"), "")) {
		std::string text("-");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_minus);
	}
}

TEST(Operator, Asterisk_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("*=+1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_asterisk_equal);
}

TEST(Operator, Asterisk_FollowedByNonEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), "")) {
		std::string text("*");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_asterisk);
	}
}

TEST(Operator, Slash_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("/=+1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_slash_equal);
}

TEST(Operator, Slash_FollowedByNonEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), "")) {
		std::string text("/");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_slash);
	}
}

TEST(Operator, Percent_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("%=+1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_percent_equal);
}

TEST(Operator, Percent_FollowedByNonEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), "")) {
		std::string text("%");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_percent);
	}
}

TEST(Operator, Ampersand_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("&=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_ampersand_equal);
}

TEST(Operator, Ampersand_FollowedByAmbersand_Combine) {
	init_keywords();

	Lexer lexer = Lexer("&&=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_ampersand_ampersand);
}

TEST(Operator, Ampersand_FollowedByNonAmpersandOrEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[&=]"), "")) {
		std::string text("&");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_ampersand);
	}
}

TEST(Operator, Vertical_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("|=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_vertical_equal);
}

TEST(Operator, Vertical_FollowedByVertical_Combine) {
	init_keywords();

	Lexer lexer = Lexer("||=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_vertical_vertical);
}

TEST(Operator, Vertical_FollowedByNonVerticalOrEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\|=]"), "")) {
		std::string text("|");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_vertical);
	}
}

TEST(Operator, Caret_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("^=+1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_caret_equal);
}

TEST(Operator, Caret_FollowedByNonEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("="), "")) {
		std::string text("^");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_caret);
	}
}

TEST(Operator, Less_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("<=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_less_equal);
}

TEST(Operator, Less_FollowedByLess_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer("<<=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_much_less_equal);
}

TEST(Operator, Less_FollowedByLess_FollowedByNonEqual_Combine) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\<=]"), "")) {
		std::string text("<<");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_much_less);
	}
}

TEST(Operator, Less_FollowedByNonLessOrEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\<=]"), "")) {
		std::string text("<");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_less);
	}
}

TEST(Operator, Greater_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer(">=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_greater_equal);
}

TEST(Operator, Greater_FollowedByGreater_FollowedByEqual_Combine) {
	init_keywords();

	Lexer lexer = Lexer(">>=1");
	auto tokens = lexer.scan();
	auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_much_greater_equal);
}

TEST(Operator, Greater_FollowedByGreater_FollowedByNonEqual_Combine) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\>=]"), "")) {
		std::string text(">>");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_much_greater);
	}
}

TEST(Operator, Greater_FollowedByNonGreaterOrEqual_Standalone) {
	init_keywords();

	for (auto c : std::regex_replace(characters_set_without_quote, std::regex("[\>=]"), "")) {
		std::string text(">");
		Lexer lexer = Lexer(text + c + "1");
		auto tokens = lexer.scan();
		auto token = std::reinterpret_pointer_cast<TokenSymbol> (tokens->front());
		ASSERT_EQ(token->name, TokenName::tk_greater);
	}
}

TEST(Operator, Dot_FollowedByNonDigit_Standalone) {
	init_keywords();

	Lexer lexer = Lexer(".=1");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenSymbol> (tokens->front());
	ASSERT_EQ(token->name, TokenName::tk_dot);
}

TEST(Operator, Dot_FollowedByDigit_Standalone) {
	init_keywords();

	Lexer lexer = Lexer(".012");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<double>> (tokens->front());
	ASSERT_EQ(token->value, 0.012);
}

TEST(Operator, Dot_FollowedByDigitAndExponent_Standalone) {
	init_keywords();

	Lexer lexer = Lexer(".012e5");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<double>> (tokens->front());
	ASSERT_EQ(token->value, 0.012e5);
}

TEST(Operator, CharacterConstant_OctalEscapeSequence_FullForm) {
	init_keywords();

	Lexer lexer = Lexer("\'\\012\'");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<char>> (tokens->front());
	ASSERT_EQ(token->value, 012);
}

TEST(Operator, CharacterConstant_OctalEscapeSequence_ShortForm) {
	init_keywords();

	Lexer lexer = Lexer("\'\\01\'");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<char>> (tokens->front());
	ASSERT_EQ(token->value, 01);
}


TEST(Operator, CharacterConstant_HexaEscapeSequence_FullForm) {
	init_keywords();

	Lexer lexer = Lexer("\'\\xfa\'");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>> (tokens->front());
	ASSERT_EQ(token->value, 0xfa);
}

TEST(Operator, CharacterConstant_HexaEscapeSequence_ShortForm) {
	init_keywords();

	Lexer lexer = Lexer("\'\\x5\'");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>> (tokens->front());
	ASSERT_EQ(token->value, 0x5);
}

TEST(Operator, CharacterConstant_Newline) {
	init_keywords();

	Lexer lexer = Lexer("\'\\n'");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>> (tokens->front());
	ASSERT_EQ(token->value, '\n');
}

TEST(Operator, CharacterConstant_Printable) {
	init_keywords();

	Lexer lexer = Lexer("\'a'");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<unsigned char>> (tokens->front());
	ASSERT_EQ(token->value, 'a');
}

TEST(Operator, StringConstant_HexaEscapeSequence_FullForm) {
	init_keywords();

	Lexer lexer = Lexer("\"a\\xfabb\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>> (tokens->front());
	ASSERT_EQ(token->value, "a\xfa" "bb");
}

TEST(Operator, StringConstant_HexaEscapeSequence_ShortForm) {
	init_keywords();

	Lexer lexer = Lexer("\"a\\xamf\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>> (tokens->front());
	ASSERT_EQ(token->value, "a\xa" "mf");
}

TEST(Operator, StringConstant_OctalEscapeSequence_FullForm) {
	init_keywords();

	Lexer lexer = Lexer("\"a\\0121f\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>> (tokens->front());
	ASSERT_EQ(token->value, "a\0121f");
}

TEST(Operator, StringConstant_OctalEscapeSequence_ShortForm) {
	init_keywords();

	Lexer lexer = Lexer("\"a\\01f1\"");
	auto tokens = lexer.scan();
	auto token = std::static_pointer_cast<TokenLiteral<std::string>> (tokens->front());
	ASSERT_EQ(token->value, "a\01f1");
}
