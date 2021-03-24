#include "preprocessor.h";

#include <regex>

#include "utils.h"

std::vector<std::shared_ptr<Token>> Preprocessor::process()
{
	std::vector<std::shared_ptr<Token>> output;
	int index = 0;
	expand(tokens, index, output);
	return output;
}

/*
1. for each token in tokens
2. if token in terminated tokens -> return
3. if token == #include
    - get filename and read the content
    - included_tokens = tokenize its content
    - expand(included_tokens, 0, output, terminated_tokens)
4. if token == #define
    - macro name = next token
    - if next == (
        - parameters = parse_define_parameters
        - body = parse_define_body
        - add FunctionMacro(name, parameters, body) to macros
      | next != (
        - body = parse_define_body
        - add ObjectMacro(name, body) to macros
5. if token == #undef
    - macro name = next token
    - remove macro from macros
6. if token == #if
    - macro name = next token
    - if condition is true
        - expand(tokens, current, output, (#elif, #else, #endif))
     | not -> skip_if_group(tokens, current)
7. if token == #elif
    - macro name = next token
    - if condition is true
        - expand(tokens, current, output, (#else, #endif))
     | not -> skip_if_group(tokens, current)
8. if token == #else
    - macro name = next token
    - if condition is true
        - expand(tokens, current, output, (#endif))
     | not -> skip_if_group(tokens, current)
9. if token->in_hide_set(token) -> push token to output and current++
10. if token is object macro
    - token->add_to_hide_set(token)
    - macro = get_macro(token)
    - expand_object_macro(macro, output, (token->hide_set, token))
11. if token is function macro
    - token->add_to_hide_set(token)
    - macro = get_macro(token)
    - expand_function_macro(macro, output, (token->hide_set, token))
*/
void Preprocessor::expand(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens,
						  int &index,
						  std::vector<std::shared_ptr<Token>> &output,
						  const std::vector<std::shared_ptr<Token>> &terminated_tokens = std::vector<std::shared_ptr<Token>>())
{
	for (int length = tokens->size(); index < length; ++index)
	{
		auto token = tokens->at(index);

		if (std::find(terminated_tokens.begin(), terminated_tokens.end(), token) != terminated_tokens.end())
			return;
		else if (token->match(TokenName::tk_hash))
			expand_directives(tokens, ++index, output, terminated_tokens);
		else if (token->in_hide_set(token) || !macros[token])
			output.push_back(token);
		else if (auto macro = macros[token])
		{
			std::vector<std::shared_ptr<Token>> hide_set = token->hide_set;
			hide_set.push_back(token);

			std::shared_ptr<std::vector<std::shared_ptr<Token>>> expanded_macro;
			if (macro->type == MacroType::function)
			{
				auto arguments = parse_macro_arguments(tokens, index);
				expanded_macro = substitute_function_macro(macro, arguments, hide_set);
			}
			else if (macro->type == MacroType::object)
				expanded_macro = substitute_object_macro(macro, hide_set);

			int expanded_index = 0;
			expand(expanded_macro, expanded_index, output, terminated_tokens);
		}
		else
			assert_not_reached();
	}
}

void Preprocessor::expand_directives(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens,
									 int &index,
									 std::vector<std::shared_ptr<Token>> &output,
									 const std::vector<std::shared_ptr<Token>> &terminated_tokens = std::vector<std::shared_ptr<Token>>())
{
	auto token = tokens->at(index);
	if (token->match(TokenType::tk_identifier))
	{
		auto token_identifier = std::dynamic_pointer_cast<TokenIdentifier>(token);
		if (token_identifier->name == "ifdef")
		{
		}
		else if (token_identifier->name == "ifndef")
		{
		}
		else if (token_identifier->name == "elif")
		{
		}
		else if (token_identifier->name == "endif")
		{
		}
		else if (token_identifier->name == "include")
		{
		}
		else if (token_identifier->name == "define")
		{
			auto macro_token = tokens->at(++index);
			std::shared_ptr<Macro> macro;

			auto after_macro_token = tokens->at(index);
			if (after_macro_token->match(TokenName::tk_left_paren))
			{
				auto parameters = parse_define_parameters(tokens, index);
				auto body = parse_define_body(tokens, index);
				macro = std::make_shared<FunctionMacro>(macro_token, parameters, body);
			}
			else
			{
				auto body = parse_define_body(tokens, index);
				macro = std::make_shared<ObjectMacro>(macro_token, body);
			}
			macros[macro_token] = macro;
		}
		else if (token_identifier->name == "undef")
			macros.erase(token);
		else
			throw std::runtime_error("# " + token_identifier->name + " is not supported");
	}
	else if (token->match(TokenName::tk_if))
	{
	}
	else if (token->match(TokenName::tk_else))
	{
	}
	else
		assert_not_reached();
}

std::vector<std::shared_ptr<Token>> &&Preprocessor::parse_define_parameters(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index)
{
	std::vector<std::shared_ptr<Token>> parameters;

	auto token = tokens->at(index);
	token->match(TokenName::tk_left_paren, true);
	index++;

	for (int length = tokens->size(); index < length; ++index)
	{
		token = tokens->at(index);
		if (token->match(TokenName::tk_right_paren))
		{
			index++;
			break;
		}
		else if (std::regex_match(token->lexeme, std::regex("^[a-zA-Z_]\w+$")))
		{
			parameters.push_back(token);
			tokens->at(++index)->match(TokenName::tk_comma, true);
		}
		else
			throw std::runtime_error("Only support #define's identifier with variable name rule");
	}

	return std::move(parameters);
}

std::vector<std::shared_ptr<Token>> &&Preprocessor::parse_define_body(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index)
{
	std::vector<std::shared_ptr<Token>> body;
	for (int length = tokens->size(); index < length; ++index)
	{
		auto token = tokens->at(index);
		if (token->match(TokenName::tk_newline))
			break;

		body.push_back(token);
	}
	return std::move(body);
}

std::vector<std::vector<std::shared_ptr<Token>>> &&Preprocessor::parse_macro_arguments(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens,
																					   int &index)
{
	std::vector<std::vector<std::shared_ptr<Token>>> arguments;

	auto token = tokens->at(index);
	token->match(TokenName::tk_left_paren, true);
	index++;

	for (int length = tokens->size(); index < length; ++index)
	{
		token = tokens->at(index);
		if (token->match(TokenName::tk_right_paren))
			break;
		else
		{
			auto arg = parse_macro_argument(tokens, index);
			arguments.push_back(arg);

			token = tokens->at(index);
			if (token->match(TokenName::tk_comma))
				index++;
		}
	}

	return std::move(arguments);
}

/* NOTE: MQ 2021-03-24
comma is used as macro argument separation
there are 2 cases comma belongs to argument when it appears between
- [xxx,xxx]
- (xxx,xxx)
*/
std::vector<std::shared_ptr<Token>> &&Preprocessor::parse_macro_argument(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index)
{
	std::vector<std::shared_ptr<Token>> argument;

	for (int length = tokens->size(), number_of_bracket = 0; index < length; ++index)
	{
		auto token = tokens->at(index);
		if (token->match(TokenName::tk_comma) && number_of_bracket == 0)
			break;
		else if (token->match([](TokenName name) {
					 return name == TokenName::tk_left_paren || name == TokenName::tk_left_bracket;
				 }))
			number_of_bracket++;
		else if (token->match([](TokenName name) {
					 return name == TokenName::tk_right_paren || name == TokenName::tk_right_bracket;
				 }))
			number_of_bracket--;

		argument.push_back(token);
	}

	return std::move(argument);
}

std::shared_ptr<std::vector<std::shared_ptr<Token>>> Preprocessor::substitute_object_macro(std::shared_ptr<Macro> macro,
																						   const std::vector<std::shared_ptr<Token>> &hide_set)
{
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> expanded_tokens;

	for (int i = 0, length = macro->replacement.size(); i < length; ++i)
	{
		auto token = macro->replacement.at(i);
		if (token->match(TokenName::tk_hash_hash))
		{
			assert(0 < i && i + 1 < length);

			auto before_token = expanded_tokens->back();
			auto after_token = macro->replacement.at(i + 1);

			auto content = before_token->lexeme + after_token->lexeme;
			Lexer lexer(content);
			token = lexer.scan()->front();

			expanded_tokens->pop_back();
			i++;
		}

		token->hide_set.insert(token->hide_set.end(), hide_set.begin(), hide_set.end());
		expanded_tokens->push_back(token);
	}

	return expanded_tokens;
}

std::shared_ptr<std::vector<std::shared_ptr<Token>>> Preprocessor::substitute_function_macro(std::shared_ptr<Macro> macro,
																							 const std::vector<std::vector<std::shared_ptr<Token>>> &arguments,
																							 const std::vector<std::shared_ptr<Token>> &hide_set)
{
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> expanded_tokens;

	auto fmacro = std::dynamic_pointer_cast<FunctionMacro>(macro);
	for (int i = 0, length = fmacro->replacement.size(); i < length; ++i)
	{
		auto token = fmacro->replacement.at(i);

		if (token->match(TokenName::tk_hash))
			throw std::runtime_error("# in #define is not supported");
		else if (token->match(TokenName::tk_hash_hash))
		{
			assert(0 < i && i + 1 < length);

			auto before_token = expanded_tokens->back();
			auto after_token = macro->replacement.at(i + 1);

			auto iterator = std::find_if(fmacro->parameters.begin(), fmacro->parameters.end(), [&after_token](std::shared_ptr<Token> tk) {
				return tk->lexeme == after_token->lexeme;
			});

			if (iterator == fmacro->parameters.end())
			{
				auto content = before_token->lexeme + after_token->lexeme;
				Lexer lexer(content);
				token = lexer.scan()->front();

				expanded_tokens->pop_back();
				i++;
			}
			else
			{
				auto parameter_index = std::distance(fmacro->parameters.begin(), iterator);
				auto argument = arguments[parameter_index];

				for (auto tk : argument)
				{
					tk->hide_set.insert(tk->hide_set.end(), hide_set.begin(), hide_set.end());
				}

				auto farg = argument.front();
				auto content = before_token->lexeme + after_token->lexeme;
				Lexer lexer(content);
				token = lexer.scan()->front();
				expanded_tokens->push_back(token);

				for (int j = 1, length = argument.size(); j < length; ++j)
					expanded_tokens->push_back(argument[j]);

				i++;
				continue;
			}
		}

		token->hide_set.insert(token->hide_set.end(), hide_set.begin(), hide_set.end());
		expanded_tokens->push_back(token);
	}

	return expanded_tokens;
}
