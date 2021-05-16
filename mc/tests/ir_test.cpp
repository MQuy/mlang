#include "llvm/ir.h"

#include <cstdio>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <regex>

#include "ast/parser.h"
#include "ast/stmt.h"
#include "ast/type.h"
#include "gtest/gtest.h"
#include "preprocesssor/preprocessor.h"
#include "scan/lexer.h"
#include "semantic/type_inference.h"

std::string generate(std::string content, std::string output_path)
{
	init_keywords();
	init_operators();
	Lexer lexer(content);

	std::filesystem::path current_path = __FILE__;
	std::filesystem::path library_path = current_path.parent_path().string() + "\\fixtures";
	std::vector<std::string> libraries_path = {library_path.string()};
	Config config(libraries_path, current_path.parent_path().string());
	Preprocessor preprocess(content, lexer.scan(), std::make_shared<Config>(config));

	Parser parser(preprocess.process());
	auto declarations = parser.parse();
	SemanticTypeInference type_inference(declarations);
	auto translation_unit = type_inference.analyze();
	IR ir(translation_unit);
	return ir.generate(output_path);
}

std::string read_content(std::filesystem::path path)
{
	std::ifstream fs(path);
	const auto sz = std::filesystem::file_size(path);
	std::string content(sz, '\0');
	fs.read(content.data(), sz);
	return content;
}

std::string exec(std::string cmd)
{
	std::array<char, 128> buffer;
	std::string result;
	std::unique_ptr<FILE, decltype(&_pclose)> pipe(_popen(cmd.c_str(), "r"), _pclose);
	if (!pipe)
	{
		throw std::runtime_error("popen() failed!");
	}
	while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr)
	{
		result += buffer.data();
	}
	return result;
}

TEST(IR, testcase)
{
	std::filesystem::path current_path = __FILE__;
	std::string test_name = "10_pointer";
	std::filesystem::path testcase_folder = current_path.parent_path().string() + "\\testcases";
	std::filesystem::path testcase_path = testcase_folder.string() + "\\" + test_name + ".c";
	std::filesystem::path testcase_expected_path = testcase_folder.string() + "\\" + test_name + ".expect";

	std::string expect_output = read_content(testcase_expected_path);
	std::string test_content = read_content(testcase_path);

	std::string output_object = test_name + ".o";
	generate(test_content, output_object);

	std::string object_exe = test_name + ".exe";
	exec("clang.exe " + output_object + " -o " + object_exe);
	std::string program_output = exec(".\\" + object_exe);

	ASSERT_STRCASEEQ(program_output.c_str(), expect_output.c_str());
}
