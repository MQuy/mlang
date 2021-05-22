#ifndef PREPROCESSOR_MACRO_H
#define PREPROCESSOR_MACRO_H 1

#include "scan/token.h"

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
	virtual std::shared_ptr<Macro> clone() = 0;

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

	std::shared_ptr<Macro> clone();
};

struct FunctionMacro : Macro
{
	FunctionMacro(std::shared_ptr<Token> name, std::vector<std::shared_ptr<Token>> parameters, std::vector<std::shared_ptr<Token>> replacement)
		: Macro(MacroType::function, name, replacement)
		, parameters(parameters)
	{
	}
	std::shared_ptr<Macro> clone();

	std::vector<std::shared_ptr<Token>> parameters;
};

#endif
