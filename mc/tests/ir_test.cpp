#include "llvm/ir.h"

#include <filesystem>
#include <regex>

#include "ast/parser.h"
#include "ast/stmt.h"
#include "ast/type.h"
#include "gtest/gtest.h"
#include "preprocesssor/preprocessor.h"
#include "scan/lexer.h"
#include "semantic/type_inference.h"

std::string generate(std::string content)
{
	init_keywords();
	init_operators();
	Lexer lexer(content);

	std::filesystem::path current_path = __FILE__;
	std::filesystem::path library_path = "C:\\Program Files\\mingw-w64\\x86_64-8.1.0-posix-seh-rt_v6-rev0\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\8.1.0\\include-fixed";
	std::vector<std::string> libraries_path = {library_path.string()};
	Config config(libraries_path, current_path.parent_path().string());
	Preprocessor preprocess(content, lexer.scan(), std::make_shared<Config>(config));

	Parser parser(preprocess.process());
	auto declarations = parser.parse();
	SemanticTypeInference type_inference(declarations);
	auto translation_unit = type_inference.analyze();
	IR ir(translation_unit);
	return ir.generate();
}

TEST(IR, demo)
{
	std::string text = generate(
		"struct foo {\n"
		"	struct {\n"
		"		int x, y;\n"
		"	};\n"
		"	struct {\n"
		"		float a, b;\n"
		"	};\n"
		"};\n"
		"int mc() {\n"
		"	struct foo x = {{11, 22}, {33, 44}};\n"
		"	return x.y;\n"
		"}\n");
}
