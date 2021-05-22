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

std::string generate(std::filesystem::path testcase_folder, std::string content, std::string output_path)
{
	init_keywords();
	init_operators();
	init_types();
	Lexer lexer(content);

	std::filesystem::path current_path = __FILE__;
	std::filesystem::path library_path = current_path.parent_path().string() + "\\fixtures";
	std::vector<std::string> libraries_path = {library_path.string()};
	Config config(libraries_path, testcase_folder.string());
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

TEST(IR, testcases)
{
	std::vector<std::string> testcases({
		"00_assignment",
		"01_comment",
		"02_printf",
		"03_struct",
		"04_for",
		"05_array",
		"06_case",
		"07_function",
		"08_while",
		"09_do_while",
		"10_pointer",
		"11_precedence",
		"12_hashdefine",
		"13_integer_literals",
		"14_if",
		"15_recursion",
		"16_nesting",
		"17_enum",
		"18_include",
		"19_pointer_arithmetic",
		"20_pointer_comparison",
		"21_char_array",
		"22_floating_point",
		"23_type_coercion",
		"24_math_library",
		"25_quicksort",
		"26_character_constants",
		"27_sizeof",
		"28_strings",
		"29_array_address",
		"30_hanoi",
		"31_args",
		"32_led",
		"33_ternary_op",
		"34_array_assignment",
		"35_sizeof",
		"36_array_initialisers",
		// 37_sprintf
		"38_multiple_array_index",
		"39_typedef",
		// 40_stdio
		"41_hashif",
		// 42_function_pointer
		"43_void_param",
		"44_scoped_declarations",
		"45_empty_for",
		// 46_grep
		"47_switch_return",
		"48_nested_break",
		"49_bracket_evaluation",
		"50_logical_second_arg",
		"51_static",
		"52_unnamed_enum",
		"54_goto",
		"55_lshift_type",
		// 60_errors_and_warnings
		"61_integers",
	});
	std::filesystem::path current_path = __FILE__;

	for (auto test_name : testcases)
	{
		std::filesystem::path testcase_folder = current_path.parent_path().string() + "\\testcases";
		std::filesystem::path testcase_path = testcase_folder.string() + "\\" + test_name + ".c";
		std::filesystem::path testcase_expected_path = testcase_folder.string() + "\\" + test_name + ".expect";

		std::string expect_output = read_content(testcase_expected_path);
		std::string test_content = read_content(testcase_path);

		std::string output_object = test_name + ".o";
		generate(testcase_folder, test_content, output_object);

		std::string object_exe = test_name + ".exe";
		exec("clang.exe " + output_object + " -o " + object_exe);
		std::string program_output = exec(".\\" + object_exe);

		ASSERT_STRCASEEQ(program_output.c_str(), expect_output.c_str());
	}
}

TEST(IR, test)
{
	std::filesystem::path current_path = __FILE__;
	std::string test_name = "64_macro_nesting";
	std::filesystem::path testcase_folder = current_path.parent_path().string() + "\\testcases";
	std::filesystem::path testcase_path = testcase_folder.string() + "\\" + test_name + ".c";
	std::filesystem::path testcase_expected_path = testcase_folder.string() + "\\" + test_name + ".expect";

	std::string expect_output = read_content(testcase_expected_path);
	std::string test_content = read_content(testcase_path);

	std::string output_object = test_name + ".o";
	generate(testcase_folder, test_content, output_object);

	std::string object_exe = test_name + ".exe";
	exec("clang.exe " + output_object + " -o " + object_exe);
	std::string program_output = exec(".\\" + object_exe);

	ASSERT_STRCASEEQ(program_output.c_str(), expect_output.c_str());
}
