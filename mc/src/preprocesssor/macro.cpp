#include "macro.h"

std::shared_ptr<Macro> ObjectMacro::clone()
{
	std::vector<std::shared_ptr<Token>> tokens;
	for (auto token : replacement)
		tokens.push_back(token->clone());

	return std::make_shared<ObjectMacro>(name, tokens);
}

std::shared_ptr<Macro> FunctionMacro::clone()
{
	std::vector<std::shared_ptr<Token>> tokens;
	for (auto token : replacement)
		tokens.push_back(token->clone());

	std::vector<std::shared_ptr<Token>> params;
	for (auto token : parameters)
		params.push_back(token->clone());

	return std::make_shared<FunctionMacro>(name, params, tokens);
}
