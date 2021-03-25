#include "preprocessor.h";

#include <filesystem>
#include <fstream>
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
2. if token == #include
    - get filename and read the content
    - included_tokens = tokenize its content
    - expand(included_tokens, 0, output)
3. if token == #define
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
    - parse constant expression
    - result = eval
    - push (#if, result) to control_directives 
    - if condition is false -> skip_control_group(tokens, current)
7. if token == #elif
    - get last control from control_directives
    - if control's result is true -> skip_control_group(tokens, current)
      | else
        - parse constant expression
        - result = eval
        - if condition is true
            - pop the last and push (#elif, result) to control_directives
        | not -> skip_if_group(tokens, current)
8. if token == #else
    - get last control from control_directives
    - if control's result is true -> skip_control_group(tokens, current)
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
						  std::vector<std::shared_ptr<Token>> &output)
{
	for (int length = tokens->size(); index < length; ++index)
	{
		auto token = tokens->at(index);

		if (token->match(TokenName::tk_hash))
			expand_directives(tokens, ++index, output);
		else if (token->hide_set[token->lexeme] || !macros[token->lexeme])
			output.push_back(token);
		else if (auto macro = macros[token->lexeme])
		{
			token->hide_set[token->lexeme] = true;

			std::shared_ptr<std::vector<std::shared_ptr<Token>>> expanded_macro;
			if (macro->type == MacroType::function)
			{
				auto arguments = parse_macro_arguments(tokens, ++index);
				expanded_macro = substitute_function_macro(macro, arguments, token->hide_set);
			}
			else if (macro->type == MacroType::object)
				expanded_macro = substitute_object_macro(macro, token->hide_set);

			int expanded_index = 0;
			expand(expanded_macro, expanded_index, output);
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
		if (token_identifier->name == "ifdef" || token_identifier->name == "ifndef")
		{
			assert(index + 1 < tokens->size());
			auto identifier = tokens->at(++index);
			assert(std::regex_match(identifier->lexeme, std::regex("^[a-zA-Z_]\w+$")));

			auto nxt_token = std::dynamic_pointer_cast<TokenSymbol>(tokens->at(++index));
			assert(nxt_token && nxt_token->name == TokenName::tk_newline);

			auto exist = macros[identifier->lexeme];
			auto forward = token_identifier->name == "ifdef" ? exist != nullptr : exist == nullptr;
			control_directives.push_back(std::make_pair(ControlDirective::ifdef, forward));
			if (forward)
				skip_control_block(tokens, index);
		}
		else if (token_identifier->name == "elif")
		{
			auto [control_name, control_cond] = control_directives.back();
			assert(control_name == ControlDirective::if_ || control_name == ControlDirective::elif);

			if (control_cond)
				skip_control_block(tokens, index);
			else
			{
				auto expr = parse_constant_expression(tokens, ++index);
				auto result = eval_constant_expression(expr);

				if (result)
				{
					control_directives.pop_back();
					control_directives.push_back(std::make_pair(ControlDirective::elif, true));
				}
				else
					skip_control_block(tokens, index);
			}
		}
		else if (token_identifier->name == "endif")
		{
			assert(control_directives.size() > 0);
			control_directives.pop_back();
		}
		else if (token_identifier->name == "include")
		{
			auto expanded_include = parse_include(tokens, ++index);
			int expanded_index = 0;
			expand(expanded_include, expanded_index, output);
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
			macros[macro_token->lexeme] = macro;
		}
		else if (token_identifier->name == "undef")
			macros.erase(token->lexeme);
		else
			throw std::runtime_error("# " + token_identifier->name + " is not supported");
	}
	else if (token->match(TokenName::tk_if))
	{
		auto expr = parse_constant_expression(tokens, ++index);
		auto result = eval_constant_expression(expr);

		control_directives.push_back(std::make_pair(ControlDirective::if_, result));

		if (!result)
			skip_control_block(tokens, index);
	}
	else if (token->match(TokenName::tk_else))
	{
		auto [_, control_cond] = control_directives.back();
		if (control_cond)
			skip_control_block(tokens, index);
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

			auto nxt_token = tokens->at(index + 1);
			if (nxt_token->match(TokenName::tk_comma))
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
																						   const std::unordered_map<std::string, bool> &hide_set)
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

		for (auto [name, exist] : hide_set)
			token->hide_set[name] = exist;

		expanded_tokens->push_back(token);
	}

	return expanded_tokens;
}

std::shared_ptr<std::vector<std::shared_ptr<Token>>> Preprocessor::substitute_function_macro(std::shared_ptr<Macro> macro,
																							 const std::vector<std::vector<std::shared_ptr<Token>>> &arguments,
																							 const std::unordered_map<std::string, bool> &hide_set)
{
	std::shared_ptr<std::vector<std::shared_ptr<Token>>> expanded_tokens;

	auto fmacro = std::dynamic_pointer_cast<FunctionMacro>(macro);
	for (int i = 0, length = fmacro->replacement.size(); i < length; ++i)
	{
		auto token = fmacro->replacement.at(i);

		if (token->match(TokenName::tk_hash))
		{
			assert(i + 1 < length);
			auto after_token = macro->replacement.at(i + 1);

			auto iterator = std::find_if(fmacro->parameters.begin(), fmacro->parameters.end(), [&after_token](std::shared_ptr<Token> tk) {
				return tk->lexeme == after_token->lexeme;
			});
			if (iterator != fmacro->parameters.end())
				throw std::runtime_error("doesn't support #xxx if xxx is not macro agrument");

			auto parameter_index = std::distance(fmacro->parameters.begin(), iterator);
			auto argument = arguments[parameter_index];

			std::string content;
			for (auto tk : argument)
			{
				// TODO: MQ 2021-03-25 we should keep the same spacing in macro argument when stringizing
				content += tk->lexeme;
			}

			token = std::make_shared<TokenLiteral<std::string>>(TokenLiteral<std::string>(content, content));
			i++;
		}
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
					for (auto [name, exist] : hide_set)
						tk->hide_set[name] = exist;

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

		for (auto [name, exist] : hide_set)
			token->hide_set[name] = exist;

		expanded_tokens->push_back(token);
	}

	return expanded_tokens;
}

std::shared_ptr<std::vector<std::shared_ptr<Token>>> Preprocessor::parse_include(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index)
{
	auto token = tokens->at(index++);
	std::filesystem::path filepath;

	if (token->type == TokenType::tk_literal)
	{
		auto token_string = std::dynamic_pointer_cast<TokenLiteral<std::string>>(token);
		assert(token_string);

		auto tmp_path = std::filesystem::path(dir_path) / token_string->value;
		if (std::filesystem::exists(filepath))
			filepath = tmp_path;
		else
		{
			for (auto include_path : include_paths)
			{
				tmp_path = std::filesystem::path(include_path) / token_string->value;
				if (std::filesystem::exists(tmp_path))
				{
					filepath = tmp_path;
					break;
				}
			}
		}
	}
	else
	{
		token->match(TokenName::tk_less);
		index++;

		std::string path;
		for (int length = tokens->size(); index < length; ++index)
		{
			auto nxt_token = tokens->at(index);
			if (nxt_token->match(TokenName::tk_greater))
			{
				index++;
				break;
			}
			else
				path += token->lexeme;
		}

		for (auto include_path : include_paths)
		{
			auto tmp_path = std::filesystem::path(include_path) / path;
			if (std::filesystem::exists(tmp_path))
			{
				filepath = tmp_path;
				break;
			}
		}
	}

	if (filepath.empty())
		throw std::runtime_error(filepath.u8string() + " deson't exist");

	std::ifstream fs(filepath);
	const auto sz = std::filesystem::file_size(filepath);
	std::string content(sz, '\0');
	fs.read(content.data(), sz);

	Lexer lexer(content);
	return lexer.scan();
}

void Preprocessor::skip_control_block(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index)
{
	for (int length = tokens->size(), number_of_control = 0; index < length; ++index)
	{
		auto token = tokens->at(index);
		if (token->match(TokenName::tk_hash))
		{
			assert(index + 1 < length);
			auto nxt_token = tokens->at(++index);

			if (nxt_token->lexeme == "ifdef" || nxt_token->lexeme == "ifndef" || nxt_token->lexeme == "if")
				number_of_control++;
			else if (nxt_token->lexeme == "elif" || nxt_token->lexeme == "else")
				throw std::runtime_error("Standalone elif or else is not valid");
			else if (nxt_token->lexeme == "endif")
			{
				if (number_of_control > 0)
					number_of_control--;
				else
					break;
			}
			else
				throw std::runtime_error("#" + nxt_token->lexeme + " is not supported");
		}
	}
}

std::vector<std::shared_ptr<Token>> &&Preprocessor::parse_constant_expression(std::shared_ptr<std::vector<std::shared_ptr<Token>>> tokens, int &index)
{
	for (int length = tokens->size(); index < length; ++index)
	{
		auto token = tokens->at(index);
	}
}

bool Preprocessor::eval_constant_expression(std::vector<std::shared_ptr<Token>> expr)
{
}
