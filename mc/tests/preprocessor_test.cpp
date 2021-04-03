#include "preprocesssor/preprocessor.h"

#include <filesystem>
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

	std::filesystem::path current_path = __FILE__;
	std::filesystem::path library_path = "C:\\Program Files\\mingw-w64\\x86_64-8.1.0-posix-seh-rt_v6-rev0\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\8.1.0\\include-fixed";
	std::vector<std::string> libraries_path = {library_path.string()};
	Config config(libraries_path, current_path.parent_path().string());
	Preprocessor preprocessor(lexer.get_source(), lexer.scan(), std::make_shared<Config>(config));

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

TEST(Preprocessor, AdjacentStringsAreConcatenated)
{
	auto tokens = preprocess("\"hello \" \"world\"");
	ASSERT_EQ(tokens.front()->lexeme, "\"hello world\"");
}

TEST(PreprocessorDefine, ObjectMacro_MacroIsReplaced)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"foo + 2;");
	ASSERT_EQ(tokens.front()->lexeme, "1");
}

TEST(PreprocessorDefine, ObjectMacro_EmptyMacroIsConsideredAsDefined)
{
	auto tokens = preprocess(
		"#define foo\n"
		"#ifdef foo\n"
		"x\n"
		"#endif");
	ASSERT_EQ(tokens.front()->lexeme, "x");
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

TEST(PreprocessorIfdef, DefinedIdentifier_BlockIsProceed)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"#ifdef foo\n"
		"x\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "x");
}

TEST(PreprocessorIfdef, NondefinedIdentifier_BlockIsIgnored)
{
	auto tokens = preprocess(
		"#ifdef foo\n"
		"x\n"
		"#endif\n"
		"y;");
	ASSERT_EQ(tokens.at(0)->lexeme, "y");
}

TEST(PreprocessorIfdef, NondefinedIdentifier_BlockContainDirectivesIsIgnored)
{
	auto tokens = preprocess(
		"#ifdef foo\n"
		"#if 1\n"
		"#elif 1\n"
		"#endif 1\n"
		"#endif\n"
		"y;");
	ASSERT_EQ(tokens.at(0)->lexeme, "y");
}

TEST(PreprocessorIfndef, DefinedIdentifier_BlockIsIgnored)
{
	auto tokens = preprocess(
		"#define foo 1\n"
		"#ifndef foo\n"
		"int\n"
		"#endif\n"
		"char");
	ASSERT_EQ(tokens.at(0)->lexeme, "char");
}

TEST(PreprocessorIfndef, NondefinedIdentifier_BlockIsProceed)
{
	auto tokens = preprocess(
		"#ifndef foo\n"
		"int\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "int");
}

TEST(PreprocessorIf, DefineOperatorForDefinedIdentifier_BlockIsProceed)
{
	auto tokens = preprocess(
		"#define auto 1\n"
		"#if defined auto\n"
		"1111\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "1111");
}

TEST(PreprocessorIf, DefineOperatorForNondefinedIdentifier_BlockIsIgnored)
{
	auto tokens = preprocess(
		"#if defined(auto)\n"
		"1111\n"
		"#endif\n"
		"2222");
	ASSERT_EQ(tokens.at(0)->lexeme, "2222");
}

TEST(PreprocessorIf, DefinedIdentifier_BlockIsProceed)
{
	auto tokens = preprocess(
		"#define auto 1\n"
		"#if auto\n"
		"'a'\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "'a'");
}

TEST(PreprocessorIf, NondefinedIdentifier_BlockIsIgnored)
{
	auto tokens = preprocess(
		"#if auto\n"
		"\n"
		"#endif\n"
		"0x15");
	ASSERT_EQ(tokens.at(0)->lexeme, "0x15");
}

TEST(PreprocessorIf, PlusTwoConstantNumbers_BlockIsProceed)
{
	auto tokens = preprocess(
		"#if 1 + 2 >= 3\n"
		"1111"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "1111");
}

TEST(PreprocessorIf, CompareSmallToLargerNumber_BlockIsIgnored)
{
	auto tokens = preprocess(
		"#if 1 > 2\n"
		"\n"
		"#endif\n"
		"ping");
	ASSERT_EQ(tokens.at(0)->lexeme, "ping");
}

TEST(PreprocessorElif, IfConditionIsTrue_ElifBlockIsIgnored)
{
	auto tokens = preprocess(
		"#if 1\n"
		"x\n"
		"#elif 2\n"
		"y\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "x");
}

TEST(PreprocessorElif, IfConditionIsFalse_ElifConditionIsTrue_BlockIsProceed)
{
	auto tokens = preprocess(
		"#if 0\n"
		"x\n"
		"#elif 2\n"
		";\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, ";");
}

TEST(PreprocessorElif, IfConditionIsFalse_ElifConditionIsFalse_BlockIsIgnored)
{
	auto tokens = preprocess(
		"#if 0\n"
		"x\n"
		"#elif 0\n"
		";\n"
		"#endif\n"
		"1111;");
	ASSERT_EQ(tokens.at(0)->lexeme, "1111");
}

TEST(PreprocessorElse, IfConditionIsTrue_ElseBlockIsIgnored)
{
	auto tokens = preprocess(
		"#if 1\n"
		"x\n"
		"#else\n"
		";\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "x");
}

TEST(PreprocessorElse, IfConditionIsFalse_ElseBlockIsProceed)
{
	auto tokens = preprocess(
		"#if 0\n"
		"x\n"
		"#else\n"
		";\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, ";");
}

TEST(PreprocessorElse, ElifConditionIsTrue_ElseBlockIsIgnored)
{
	auto tokens = preprocess(
		"#if 0\n"
		"x\n"
		"#elif 1\n"
		"y\n"
		"#else\n"
		"z\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "y");
}

TEST(PreprocessorElse, ElifConditionIsFalse_ElseBlockIsProceed)
{
	auto tokens = preprocess(
		"#if 0\n"
		"x\n"
		"#elif 0\n"
		"y\n"
		"#else\n"
		"int\n"
		"#endif\n");
	ASSERT_EQ(tokens.at(0)->lexeme, "int");
}

TEST(PreprocessorInclude, DefineMacroInIncludedFile)
{
	auto tokens = preprocess(
		"#include \"fixtures/basic_include.h\"\n"
		"foo");
	ASSERT_EQ(tokens.at(0)->lexeme, "1");
}

TEST(PreprocessorInclude, StandardIncludeDirectories)
{
	auto tokens = preprocess(
		"#include \"limits.h\"\n"
		"#ifdef CHAR_BIT\n"
		"x\n"
		"#endif");
	ASSERT_EQ(tokens.at(0)->lexeme, "x");
}
