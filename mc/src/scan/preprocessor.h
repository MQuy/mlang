#ifndef SCAN_PREPROCESSOR
#define SCAN_PREPROCESSOR 1

#include <unordered_map>

#include "lexer.h"

enum class MacroType
{
	object,
	function,
};

struct Macro
{
	Macro(MacroType type, std::shared_ptr<Token> name, std::vector<std::shared_ptr<Token>> replacement)
		: type(type)
		, name(name)
		, replacement(replacement)
	{
	}

	MacroType type;
	std::shared_ptr<Token> name;
	std::vector<std::shared_ptr<Token>> replacement;
};

struct ObjectMacro : Macro
{
	ObjectMacro(std::shared_ptr<Token> name, std::vector<std::shared_ptr<Token>> replacement)
		: Macro(MacroType::object, name, replacement)
	{
	}
};

struct FunctionMacro : Macro
{
	FunctionMacro(std::shared_ptr<Token> name, std::vector<std::shared_ptr<Token>> parameters, std::vector<std::shared_ptr<Token>> replacement)
		: Macro(MacroType::function, name, replacement)
		, parameters(parameters)
	{
	}

	std::vector<std::shared_ptr<Token>> parameters;
};

enum class ControlDirective
{
	if_,
	ifdef,
	ifndef,
	elif,
	else_,
	endif,
};

class Preprocessor
{
public:
	Preprocessor(std::string source, std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens)
		: source(source)
		, tokens(tokens)
	{
	}

	std::vector<std::shared_ptr<Token>> process();

private:
	void expand(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens,
				int &index,
				std::vector<std::shared_ptr<Token>> &output);
	void expand_directives(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens,
						   int &index,
						   std::vector<std::shared_ptr<Token>> &output,
						   const std::vector<std::shared_ptr<Token>> &terminated_tokens = std::vector<std::shared_ptr<Token>>());
	std::vector<std::shared_ptr<Token>> &&parse_define_parameters(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index);
	std::vector<std::shared_ptr<Token>> &&parse_define_body(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index);
	std::vector<std::vector<std::shared_ptr<Token>>> &&parse_macro_arguments(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens,
																			 int &index);
	std::vector<std::shared_ptr<Token>> &&parse_macro_argument(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index);
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> substitute_object_macro(std::shared_ptr<Macro> macro,
																				 const std::unordered_map<std::string, bool> &hide_set);
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> substitute_function_macro(std::shared_ptr<Macro> macro,
																				   const std::vector<std::vector<std::shared_ptr<Token>>> &arguments,
																				   const std::unordered_map<std::string, bool> &hide_set);
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> parse_include(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index);
	void skip_control_block(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index);
	std::vector<std::shared_ptr<Token>> &&parse_constant_expression(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index);
	bool eval_constant_expression(std::vector<std::shared_ptr<Token>> expr);

	std::string source;
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens;
	std::unordered_map<std::string, std::shared_ptr<Macro>> macros;
	std::vector<std::pair<ControlDirective, bool>> control_directives;

	std::vector<std::string> include_paths;
	std::string dir_path;
};

#endif
