#include "scan/lexer.h"
#include "preprocesssor/preprocessor.h"
#include "ast/parser.h"

#include <filesystem>
#include <regex>

#include "gtest/gtest.h"

std::shared_ptr<Program> parse(std::string content)
{
	init_keywords();
	Lexer lexer(content);

	std::filesystem::path current_path = __FILE__;
	std::filesystem::path library_path = "C:\\Program Files\\mingw-w64\\x86_64-8.1.0-posix-seh-rt_v6-rev0\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\8.1.0\\include-fixed";
	std::vector<std::string> libraries_path = { library_path.string() };
	Config config(libraries_path, current_path.parent_path().string());
	Preprocessor preprocess(content, lexer.scan(), std::make_shared<Config>(config));

	Parser parser(preprocess.process());
	return parser.parse();
}

TEST(ASTExtern, IntDeclaration_OnlyInt)
{
}

TEST(ASTExtern, IntDeclaration_WithStaticStorage)
{
}

TEST(ASTExtern, IntDeclaration_WithConstQualifer)
{
}

TEST(ASTExtern, IntDeclaration_WithStaticStorageAndConstQualifer_WithDeclarator)
{
}

TEST(ASTExtern, IntDeclaration_WithStaticStorageAndConstQualifer_WithDeclaratorAndInit)
{
}

TEST(ASTExtern, IntDeclaration_WithDeclarator)
{
}

TEST(ASTExtern, EnumDeclaration_Forward)
{
}

TEST(ASTExtern, EnumDeclaration_WithEmptyBody)
{
}

TEST(ASTExtern, EnumDeclaration_WithConstantInitialForIdentifer)
{
}

TEST(ASTExtern, EnumDeclaration_WithDeclarator)
{
}

TEST(ASTExtern, StructDeclaration_Forward)
{
}

TEST(ASTExtern, StructDeclaration_WithEmptyBody)
{
}

TEST(ASTExtern, StructDeclaration_WithFlatMembers)
{
}

TEST(ASTExtern, StructDeclaration_WithNestedStruct)
{
}

TEST(ASTExtern, StructDeclaration_WithDeclarator)
{
}

TEST(ASTExtern, StructDeclaration_WithDeclaratorAndInit)
{
}

TEST(ASTExtern, Typedef_MapIntToIdentifier)
{
}

TEST(ASTExtern, Typedef_MapArrayOfIntToIdentifier)
{
}

TEST(ASTExtern, FunctionDeclaration_VoidReturnAndEmptyParameters)
{
}

TEST(ASTExtern, FunctionDeclaration_IntReturnAndOneIntParameter)
{
}

TEST(ASTExtern, FunctionDeclaration_StructReturnAndPointerStructParameterWithoutName)
{
}
