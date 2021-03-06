#include "preprocessor.h";

#include <filesystem>
#include <fstream>
#include <regex>

#include "ast/parser.h"
#include "const_expr.h"
#include "utils.h"

inline bool is_whitespace_token(std::shared_ptr<Token> token)
{
	return token->match([](TokenName name)
						{
							return name == TokenName::tk_space || name == TokenName::tk_tab;
						});
}

inline int skip_whitespaces_tokens(std::vector<std::shared_ptr<Token>>& tokens, int index, int length, bool include_newline = false, bool forward = true)
{
	int i = index;
	while (0 <= i && i < length)
	{
		auto token = tokens.at(i);
		if (!(is_whitespace_token(token) || (include_newline ? token->match(TokenName::tk_newline) : false)))
			break;
		i = i + (forward ? 1 : -1);
	}
	return i;
}

std::vector<std::shared_ptr<Token>> postpreprocess_tokens(std::vector<std::shared_ptr<Token>>& tokens)
{
	std::vector<std::shared_ptr<Token>> output;
	for (int length = tokens.size(), i = skip_whitespaces_tokens(tokens, 0, length, true);
		 i < length;
		 i = skip_whitespaces_tokens(tokens, i + 1, length, true))
	{
		auto token = tokens.at(i);

		auto nxt_i = skip_whitespaces_tokens(tokens, i + 1, length);
		if (nxt_i < length)
		{
			auto nxt_token = tokens.at(nxt_i);
			if (std::regex_match(token->lexeme, std::regex("^\".*\"$"))
				&& std::regex_match(nxt_token->lexeme, std::regex("^\".*\"$")))
			{
				auto token_literal = std::static_pointer_cast<TokenLiteral<std::string>>(token);
				auto nxt_token_literal = std::static_pointer_cast<TokenLiteral<std::string>>(nxt_token);
				auto content = token_literal->value + nxt_token_literal->value;

				token = std::make_shared<TokenLiteral<std::string>>(TokenLiteral<std::string>(content, "\"" + content + "\""));
				token->set_position(token->start, nxt_token->end);
				i = nxt_i;
			}
		}
		else if (token->match(TokenName::tk_eof))
			break;

		output.push_back(token);
	}
	return output;
}

// remove trailling and collapse sequence of whitespaces in the middle to a single space
std::vector<std::shared_ptr<Token>> standarize_function_macro_argument(std::vector<std::shared_ptr<Token>>& tokens)
{
	std::vector<std::shared_ptr<Token>> argument;
	auto length = tokens.size();

	int start = skip_whitespaces_tokens(tokens, 0, length);
	int end = skip_whitespaces_tokens(tokens, length - 1, length, false, false);

	for (; start <= end; ++start)
	{
		auto token = tokens.at(start);

		if (token->match(TokenName::tk_tab))
			token = std::make_shared<TokenSymbol>(TokenSymbol(TokenName::tk_space));

		if (is_whitespace_token(token) && start > 0)
		{
			auto prev_token = tokens.at(start - 1);
			if (is_whitespace_token(prev_token))
				continue;
		}

		argument.push_back(token);
	}

	return argument;
}

void skip_control_block(std::vector<std::shared_ptr<Token>>& tokens, int& index)
{
	for (int length = tokens.size(), number_of_control = 0; index < length; ++index)
	{
		auto token = tokens.at(index);
		if (token->match(TokenName::tk_hash))
		{
			index = skip_whitespaces_tokens(tokens, index + 1, length);
			assert(index < length);
			auto nxt_token = tokens.at(index);

			if (nxt_token->lexeme == "define" || nxt_token->lexeme == "undef" || nxt_token->lexeme == "include" || nxt_token->lexeme == "include_next")
				continue;
			if (nxt_token->lexeme == "ifdef" || nxt_token->lexeme == "ifndef" || nxt_token->lexeme == "if")
				number_of_control++;
			else if (nxt_token->lexeme == "elif" || nxt_token->lexeme == "else" || nxt_token->lexeme == "endif")
			{
				if (number_of_control == 0)
				{
					// back to before # position
					index = skip_whitespaces_tokens(tokens, index - 1, length, false, false);
					index--;
					break;
				}
				if (nxt_token->lexeme == "endif")
					number_of_control--;
			}
			else
				throw std::runtime_error("#" + nxt_token->lexeme + " is not supported");
		}
	}
}

/* NOTE: MQ 2021-03-24
comma is used as macro argument separation
there are 2 cases comma belongs to argument when it appears between
- [xxx,xxx]
- (xxx,xxx)
*/
std::vector<std::shared_ptr<Token>> parse_macro_argument(std::vector<std::shared_ptr<Token>>& tokens, int& index)
{
	std::vector<std::shared_ptr<Token>> argument;

	auto length = tokens.size();
	for (int number_of_bracket = 0; index < length; ++index)
	{
		auto token = tokens.at(index);
		if (token->match([](TokenName name)
						 {
							 return name == TokenName::tk_comma || name == TokenName::tk_right_paren;
						 })
			&& number_of_bracket == 0)
		{
			--index;
			break;
		}
		else if (token->match([](TokenName name)
							  {
								  return name == TokenName::tk_left_paren || name == TokenName::tk_left_bracket;
							  }))
			number_of_bracket++;
		else if (token->match([](TokenName name)
							  {
								  return name == TokenName::tk_right_paren || name == TokenName::tk_right_bracket;
							  }))
			number_of_bracket--;

		argument.push_back(token);
	}

	return argument;
}

std::vector<std::vector<std::shared_ptr<Token>>> parse_macro_arguments(std::vector<std::shared_ptr<Token>>& tokens,
																	   int& index)
{
	std::vector<std::vector<std::shared_ptr<Token>>> arguments;

	auto length = tokens.size();
	index = skip_whitespaces_tokens(tokens, index, length);

	auto token = tokens.at(index);
	token->match(TokenName::tk_left_paren, true);

	token = tokens.at(++index);
	if (token->match(TokenName::tk_right_paren))
		return arguments;

	for (; index < length; ++index)
	{
		token = tokens.at(index);
		auto arg = parse_macro_argument(tokens, index);
		arguments.push_back(arg);

		auto nxt_token = tokens.at(++index);
		if (nxt_token->match(TokenName::tk_right_paren))
			break;
		else
			nxt_token->match(TokenName::tk_comma, true);
	}

	return arguments;
}

std::vector<std::shared_ptr<Token>> parse_define_body(std::vector<std::shared_ptr<Token>>& tokens, int& index)
{
	std::vector<std::shared_ptr<Token>> body;

	auto length = tokens.size();
	index = skip_whitespaces_tokens(tokens, index, length);
	for (; index < length; ++index)
	{
		auto token = tokens.at(index);
		if (token->match(TokenName::tk_newline))
			break;

		body.push_back(token);
	}
	return body;
}

std::vector<std::shared_ptr<Token>> parse_define_parameters(std::vector<std::shared_ptr<Token>>& tokens, int& index)
{
	std::vector<std::shared_ptr<Token>> parameters;

	auto token = tokens.at(index);
	token->match(TokenName::tk_left_paren, true);

	auto length = tokens.size();
	index = skip_whitespaces_tokens(tokens, index + 1, length);

	for (; index < length; index = skip_whitespaces_tokens(tokens, index + 1, length))
	{
		token = tokens.at(index);
		if (token->match(TokenName::tk_right_paren))
			break;
		else if (std::regex_match(token->lexeme, std::regex("^[a-zA-Z_]\\w*$")))
		{
			parameters.push_back(token);

			index = skip_whitespaces_tokens(tokens, index + 1, length);
			auto nxt_token = tokens.at(index);
			if (nxt_token->match(TokenName::tk_right_paren))
				break;
			else
				nxt_token->match(TokenName::tk_comma, true);
		}
		else
			throw std::runtime_error("Only support #define's identifier with variable name rule");
	}

	return parameters;
}

std::vector<std::shared_ptr<Token>> Preprocessor::process()
{
	std::vector<std::shared_ptr<Token>> expanded_tokens;
	int index = 0;
	expand(tokens, index, expanded_tokens);

	return postpreprocess_tokens(expanded_tokens);
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
void Preprocessor::expand(std::vector<std::shared_ptr<Token>>& tokens,
						  int& index,
						  std::vector<std::shared_ptr<Token>>& output)
{
	for (int length = tokens.size(); index < length; ++index)
	{
		auto token = tokens.at(index);

		if (token->match(TokenName::tk_hash))
			expand_directives(tokens, ++index, output);
		else if (token->hide_set[token->lexeme] || !macros[token->lexeme])
			output.push_back(token);
		else if (auto macro = macros[token->lexeme])
		{
			std::vector<std::shared_ptr<Token>> expanded_tokens;
			auto copied_macro = macro->clone();
			if (macro->type == MacroType::function)
			{
				auto arguments = parse_macro_arguments(tokens, ++index);
				auto right_paren_token = tokens[index];
				auto hide_set = intersect_two_hide_sets(token->hide_set, right_paren_token->hide_set);

				hide_set[token->lexeme] = true;
				expanded_tokens = substitute_function_macro(copied_macro, arguments, hide_set);
			}
			else if (macro->type == MacroType::object)
			{
				token->hide_set[token->lexeme] = true;
				expanded_tokens = substitute_object_macro(copied_macro, token->hide_set);
			}

			for (++index; index < length; ++index)
				expanded_tokens.push_back(tokens[index]);

			int expanded_index = 0;
			expand(expanded_tokens, expanded_index, output);
		}
		else
			assert_not_reached();
	}
}

void Preprocessor::expand_directives(std::vector<std::shared_ptr<Token>>& tokens,
									 int& index,
									 std::vector<std::shared_ptr<Token>>& output,
									 const std::vector<std::shared_ptr<Token>>& terminated_tokens)
{
	auto length = tokens.size();
	index = skip_whitespaces_tokens(tokens, index, length);

	auto token = tokens.at(index);
	if (token->match(TokenType::tk_identifier))
	{
		auto token_identifier = std::static_pointer_cast<TokenIdentifier>(token);
		if (token_identifier->name == "ifdef" || token_identifier->name == "ifndef")
		{
			index = skip_whitespaces_tokens(tokens, index + 1, length);
			assert(index < length);

			auto identifier = tokens.at(index);
			assert(std::regex_match(identifier->lexeme, std::regex("^[a-zA-Z_]\\w*$")));

			index = skip_whitespaces_tokens(tokens, index + 1, length);

			auto nxt_token = tokens.at(index);
			assert(nxt_token->type == TokenType::tk_symbol && std::static_pointer_cast<TokenSymbol>(nxt_token)->name == TokenName::tk_newline);

			auto exist = macros[identifier->lexeme];
			auto forward = token_identifier->name == "ifdef" ? exist != nullptr : exist == nullptr;
			auto control_directive = token_identifier->name == "ifdef" ? ControlDirective::ifdef : ControlDirective::ifndef;
			control_directives.push_back(std::make_pair(control_directive, forward));
			if (!forward)
				skip_control_block(tokens, ++index);
		}
		else if (token_identifier->name == "elif")
		{
			auto [control_name, control_cond] = control_directives.back();
			assert(control_name == ControlDirective::if_ || control_name == ControlDirective::elif);

			if (control_cond)
				skip_control_block(tokens, ++index);
			else
			{
				auto expr = parse_constant_expression(tokens, ++index);
				auto result = PreprocessorConstExpr(std::move(expr)).eval();

				if (result)
				{
					control_directives.pop_back();
					control_directives.push_back(std::make_pair(ControlDirective::elif, true));
				}
				else
					skip_control_block(tokens, ++index);
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
			std::vector<std::shared_ptr<Token>> expanded_tokens;
			expand(expanded_include, expanded_index, expanded_tokens);

			auto cleanup_tokens = postpreprocess_tokens(expanded_tokens);
			output.insert(output.end(), cleanup_tokens.begin(), cleanup_tokens.end());
		}
		else if (token_identifier->name == "define")
		{
			index = skip_whitespaces_tokens(tokens, index + 1, length);
			auto macro_token = tokens.at(index);
			assert(std::regex_match(macro_token->lexeme, std::regex("^[a-zA-Z_]\\w*$")));

			std::shared_ptr<Macro> macro;

			// there has to be no space between identifier and left paren
			auto after_macro_token = tokens.at(++index);
			if (after_macro_token->match(TokenName::tk_left_paren))
			{
				auto parameters = parse_define_parameters(tokens, index);
				auto body = parse_define_body(tokens, ++index);
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
		{
			index = skip_whitespaces_tokens(tokens, index + 1, length);
			auto nxt_token = tokens.at(index);
			assert(std::regex_match(nxt_token->lexeme, std::regex("^[a-zA-Z_]\\w*$")));

			macros.erase(nxt_token->lexeme);
		}
		else if (token_identifier->name == "include_next")
		{
			// TODO: MQ 2021-03-29 Support include next
			while (!tokens.at(index)->match(TokenName::tk_newline))
				index++;
		}
		else
			throw std::runtime_error("# " + token_identifier->name + " is not supported");
	}
	else if (token->match(TokenName::tk_if))
	{
		auto expr = parse_constant_expression(tokens, ++index);
		auto result = PreprocessorConstExpr(std::move(expr)).eval();

		control_directives.push_back(std::make_pair(ControlDirective::if_, result));

		if (!result)
			skip_control_block(tokens, ++index);
	}
	else if (token->match(TokenName::tk_else))
	{
		auto nxt_token = tokens.at(++index);

		auto [_, control_cond] = control_directives.back();
		if (control_cond)
			skip_control_block(tokens, ++index);
	}
	else
		throw std::runtime_error("# " + token->lexeme + " is not supported");
}

std::vector<std::shared_ptr<Token>> Preprocessor::substitute_object_macro(const std::shared_ptr<Macro>& macro,
																		  const std::unordered_map<std::string, bool>& hide_set)
{
	std::vector<std::shared_ptr<Token>> expanded_tokens;

	for (int i = 0, length = macro->replacement.size(); i < length; ++i)
	{
		auto token = macro->replacement.at(i);
		if (token->match(TokenName::tk_hash_hash))
		{
			auto et_length = expanded_tokens.size();
			assert(et_length > 0);
			auto et_i = skip_whitespaces_tokens(expanded_tokens, et_length - 1, et_length, false, false);
			std::shared_ptr<Token> before_token = expanded_tokens.at(et_i);

			i = skip_whitespaces_tokens(macro->replacement, i + 1, length);
			assert(i < length);
			auto after_token = macro->replacement.at(i);

			auto content = before_token->lexeme + after_token->lexeme;
			Lexer lexer(content);
			token = lexer.scan().front();

			for (int times = et_length - et_i; times > 0; --times)
				expanded_tokens.pop_back();
		}

		for (auto [name, exist] : hide_set)
			token->hide_set[name] = exist;

		expanded_tokens.push_back(token);
	}

	return expanded_tokens;
}

std::vector<std::shared_ptr<Token>> Preprocessor::substitute_function_macro(const std::shared_ptr<Macro>& macro,
																			const std::vector<std::vector<std::shared_ptr<Token>>>& arguments,
																			const std::unordered_map<std::string, bool>& hide_set)
{
	std::vector<std::shared_ptr<Token>> expanded_tokens;

	auto fmacro = std::static_pointer_cast<FunctionMacro>(macro);
	for (int i = 0, length = fmacro->replacement.size(); i < length; ++i)
	{
		auto token = fmacro->replacement.at(i);

		if (token->match(TokenName::tk_hash))
		{
			i = skip_whitespaces_tokens(macro->replacement, i + 1, length);
			assert(i < length);
			auto after_token = macro->replacement.at(i);

			auto iterator = std::find_if(fmacro->parameters.begin(), fmacro->parameters.end(), [&after_token](std::shared_ptr<Token> tk)
										 {
											 return tk->lexeme == after_token->lexeme;
										 });
			if (iterator == fmacro->parameters.end())
				throw std::runtime_error("doesn't support #" + after_token->lexeme + " if xxx is not macro agrument");

			auto parameter_index = std::distance(fmacro->parameters.begin(), iterator);
			auto argument = arguments[parameter_index];
			std::vector<std::shared_ptr<Token>> standard_argument = standarize_function_macro_argument(argument);

			std::string content;
			for (auto tk : standard_argument)
				content += tk->lexeme;

			token = std::make_shared<TokenLiteral<std::string>>(TokenLiteral<std::string>(content, "\"" + content + "\""));
		}
		else if (token->match(TokenName::tk_hash_hash))
		{
			auto et_length = expanded_tokens.size();
			assert(et_length > 0);
			auto et_i = skip_whitespaces_tokens(expanded_tokens, et_length - 1, et_length, false, false);
			std::shared_ptr<Token> before_token = expanded_tokens.at(et_i);

			i = skip_whitespaces_tokens(macro->replacement, i + 1, length);
			assert(i < length);
			auto after_token = macro->replacement.at(i);

			auto iterator = std::find_if(fmacro->parameters.begin(), fmacro->parameters.end(), [&after_token](std::shared_ptr<Token> tk)
										 {
											 return tk->lexeme == after_token->lexeme;
										 });

			if (iterator == fmacro->parameters.end())
			{
				auto content = before_token->lexeme + after_token->lexeme;
				Lexer lexer(content);
				token = lexer.scan().front();

				for (int times = et_length - et_i; times > 0; --times)
					expanded_tokens.pop_back();
				expanded_tokens.push_back(token);
			}
			else
			{
				auto parameter_index = std::distance(fmacro->parameters.begin(), iterator);
				auto argument = arguments[parameter_index];
				std::vector<std::shared_ptr<Token>> standard_argument = standarize_function_macro_argument(argument);

				if (standard_argument.size() > 0)
				{
					for (auto tk : standard_argument)
						for (auto [name, exist] : hide_set)
							tk->hide_set[name] = exist;

					auto farg = standard_argument.front();
					auto content = before_token->lexeme + farg->lexeme;
					Lexer lexer(content);
					token = lexer.scan().front();

					for (int times = et_length - et_i; times > 0; --times)
						expanded_tokens.pop_back();

					for (auto [name, exist] : hide_set)
						token->hide_set[name] = exist;
					expanded_tokens.push_back(token);

					for (int j = 1, length = standard_argument.size(); j < length; ++j)
						expanded_tokens.push_back(standard_argument[j]);
				}

				continue;
			}
		}
		else
		{
			auto iterator = std::find_if(fmacro->parameters.begin(), fmacro->parameters.end(), [&token](std::shared_ptr<Token> tk)
										 {
											 return tk->lexeme == token->lexeme;
										 });
			if (iterator != fmacro->parameters.end())
			{
				auto parameter_index = std::distance(fmacro->parameters.begin(), iterator);
				auto argument = arguments[parameter_index];
				std::vector<std::shared_ptr<Token>> standard_argument = standarize_function_macro_argument(argument);

				for (auto tk : standard_argument)
				{
					for (auto [name, exist] : hide_set)
						tk->hide_set[name] = exist;
					expanded_tokens.push_back(tk);
				}

				if (standard_argument.size() == 0)
					if (auto nxt_i = skip_whitespaces_tokens(macro->replacement, i + 1, length); nxt_i < length)
					{
						auto nxt_token = macro->replacement.at(nxt_i);
						if (nxt_token->match(TokenName::tk_hash_hash))
							i = nxt_i;
					}

				continue;
			}
		}

		for (auto [name, exist] : hide_set)
			token->hide_set[name] = exist;
		expanded_tokens.push_back(token);
	}

	return expanded_tokens;
}

std::vector<std::shared_ptr<Token>> Preprocessor::parse_include(std::vector<std::shared_ptr<Token>>& tokens, int& index)
{
	index = skip_whitespaces_tokens(tokens, index, tokens.size());
	auto token = tokens.at(index);
	std::filesystem::path filepath;

	if (token->type == TokenType::tk_literal)
	{
		auto token_string = std::dynamic_pointer_cast<TokenLiteral<std::string>>(token);
		assert(token_string);

		auto tmp_path = std::filesystem::path(config->current_path) / token_string->value;
		if (std::filesystem::exists(tmp_path))
			filepath = tmp_path;
		else
		{
			for (auto include_path : config->include_paths)
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
		for (int length = tokens.size(); index < length; ++index)
		{
			token = tokens.at(index);
			if (token->match(TokenName::tk_greater))
			{
				index++;
				break;
			}
			else
				path += token->lexeme;
		}

		for (auto include_path : config->include_paths)
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
		throw std::runtime_error(filepath.u8string() + " doesn't exist");

	std::ifstream fs(filepath);
	const auto sz = std::filesystem::file_size(filepath);
	std::string content(sz, '\0');
	fs.read(content.data(), sz);

	Lexer lexer(content);
	return lexer.scan();
}

/*
- get tokens from current position till the end of line
- replace macros with their definition
- replace defined identifier with 1 and undefined identifier with 0
*/
std::vector<std::shared_ptr<Token>> Preprocessor::parse_constant_expression(std::vector<std::shared_ptr<Token>>& tokens, int& index)
{
	std::vector<std::shared_ptr<Token>> body;
	for (int length = tokens.size(); index < length; ++index)
	{
		auto token = tokens.at(index);
		if (token->match(TokenName::tk_newline))
			break;
		else if (!is_whitespace_token(token))
			body.push_back(token);
	}

	std::vector<std::shared_ptr<Token>> replaced_body;
	for (int i = 0, length = body.size(); i < length; ++i)
	{
		auto token = body.at(i);
		if (token->lexeme == "defined")
		{
			i = skip_whitespaces_tokens(body, i + 1, length);
			auto nxt_token = body.at(i);
			if (nxt_token->match(TokenName::tk_left_paren))
			{
				i = skip_whitespaces_tokens(body, i + 1, length);
				nxt_token = body.at(i);
				i = skip_whitespaces_tokens(body, i + 1, length);
				body.at(i)->match(TokenName::tk_right_paren, true);
			}
			assert(std::regex_match(nxt_token->lexeme, std::regex("^[a-zA-Z_]\\w*$")));

			int result = macros[nxt_token->lexeme] != nullptr;
			replaced_body.push_back(std::make_shared<TokenLiteral<int>>(TokenLiteral<int>(result, std::to_string(result))));
		}
		else
			replaced_body.push_back(token);
	}

	int ei = 0;
	std::vector<std::shared_ptr<Token>> expanded_body;
	expand(replaced_body, ei, expanded_body);

	std::vector<std::shared_ptr<Token>> output;
	for (int i = 0, length = expanded_body.size(); i < length; ++i)
	{
		auto token = expanded_body.at(i);
		if (std::regex_match(token->lexeme, std::regex("^[a-zA-Z_]\\w*$")))
		{
			auto value = macros[token->lexeme] ? 1 : 0;
			token = std::make_shared<TokenLiteral<int>>(TokenLiteral<int>(value, std::to_string(value)));
		}
		output.push_back(token);
	}

	return output;
}

std::unordered_map<std::string, bool> Preprocessor::intersect_two_hide_sets(std::unordered_map<std::string, bool> hide_set1, std::unordered_map<std::string, bool> hide_set2)
{
	std::unordered_map<std::string, bool> hide_set;
	for (auto [hs1_name, hs1_exist] : hide_set1)
		if (hs1_exist && hide_set2[hs1_name])
			hide_set[hs1_name] = true;

	return hide_set;
}
