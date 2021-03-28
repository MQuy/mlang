#include "preprocesssor/preprocessor.h"

#include <regex>

#include "gtest/gtest.h"

/*
1. without any directives
2. #define (object, function) and a variable definition uses that #define
3. recursive #define
4. #define function, use # to stringize argument
5. #define, use ## to concat empty/non-empty argument
6. #undef
7. #ifndef for exist/non exist identifier
8. #ifdef for exist/non exist identifier
9. #if-#endif
10. #if-#elif-#endif
11. #if-#else-#endif
12. #if-#elif-#else-#endif
13. #include for " and <
14. adjacent string literals are concatenated
*/

std::vector<std::shared_ptr<Token>> preprocess(std::string content)
{
	init_keywords();
	Lexer lexer(content);
	Preprocessor preprocessor(lexer.get_source(), lexer.scan());
	return preprocessor.process();
}

TEST(Preprocessor, NewlineAndSpaceAreRemoved)
{
	auto tokens = preprocess(
		"int x;\n"
		"x = 1 + 2;");
	for (auto token : tokens)
		if (token->match(TokenType::tk_symbol))
		{
			auto token_symbol = std::dynamic_pointer_cast<TokenSymbol>(token);
			ASSERT_TRUE(token_symbol->match([](TokenName name) {
				return name != TokenName::tk_space && name != TokenName::tk_tab && name != TokenName::tk_newline;
			}));
		}
}

TEST(PreprocessorDefine, ObjectMacro_MacroIsReplaced)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"foo + 2;");
	ASSERT_EQ(tokens.front()->lexeme, "1");
}

TEST(PreprocessorDefine, ObjectMacro_MacroInsideAnotherMacroIsReplaced)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"#define baz foo\n"
		"baz;");
	ASSERT_EQ(tokens.front()->lexeme, "1");
}

TEST(PreprocessorDefine, ObjectMacro_RecursivedMacroIsNotReplaced)
{
	auto tokens = preprocess(
		"#define foo baz\n"
		"#define baz foo\n"
		"baz + 2;");
	ASSERT_EQ(tokens.front()->lexeme, "baz");
}

TEST(PreprocessorDefine, FunctionMacro_MacroIsReplaced)
{
	auto tokens = preprocess(
		"#define foo(x, y) x + y\n"
		"foo(1, 2);");
	ASSERT_EQ(tokens.at(0)->lexeme, "1");
	ASSERT_EQ(tokens.at(1)->lexeme, "+");
	ASSERT_EQ(tokens.at(2)->lexeme, "2");
}

TEST(PreprocessorDefine, FunctionMacro_ContainsObjectMacro)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"#define baz(x) foo + x\n"
		"baz(2);");
	ASSERT_EQ(tokens.at(0)->lexeme, "1");
	ASSERT_EQ(tokens.at(1)->lexeme, "+");
	ASSERT_EQ(tokens.at(2)->lexeme, "2");
}

TEST(PreprocessorDefine, FunctionMacro_Stringize_RemoveTrailingAndSequenceSpacesInMiddle)
{
	auto tokens = preprocess(
		"#define foo(x) #x\n"
		"foo(   1     +     2   );");
	ASSERT_EQ(tokens.at(0)->lexeme, "\"1 + 2\"");
}

TEST(PreprocessorDefine, FunctionMacro_Concatenate_TwoNumbers)
{
	auto tokens = preprocess(
		"#define foo(x, y) x ## y\n"
		"foo(1, 2);");
	ASSERT_EQ(tokens.at(0)->lexeme, "12");
}

TEST(PreprocessorDefine, FunctionMacro_Concatenate_PartsOfMacro)
{
	auto tokens = preprocess(
		"#define foo 1111\n"
		"#define baz(x, y) x ## y\n"
		"baz(fo, o);");
	ASSERT_EQ(tokens.at(0)->lexeme, "1111");
}

TEST(PreprocessorDefine, FunctionMacro_Concatenate_LeftEmptyArgumentIsIgnored)
{
	auto tokens = preprocess(
		"#define baz(x, y) x ## y\n"
		"baz(, xxx);");
	ASSERT_EQ(tokens.at(0)->lexeme, "xxx");
}

TEST(PreprocessorDefine, FunctionMacro_Concatenate_RightEmptyArgumentIsIgnored)
{
	auto tokens = preprocess(
		"#define baz(x, y) x ## y\n"
		"baz(\"hello\",);");
	ASSERT_EQ(tokens.at(0)->lexeme, "\"hello\"");
}

TEST(PreprocessorUndef, MacroIsNotDefinedAfterThatPoint)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"#undef foo\n"
		"foo\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "foo");
}