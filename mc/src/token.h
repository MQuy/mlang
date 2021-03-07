#include <optional>
#include <set>
#include <string>
#include <vector>

enum class TokenName
{
	// literal constant
	tk_integer,
	tk_character,
	tk_float,
	tk_enum,
	tk_string,
	// operator
	tk_plus,
	tk_minus,
	tk_star,
	tk_slash,
	// reversed words
	tk_auto,
	tk_register,
	tk_static,
	tk_extern,
	tk_typedef,
};

enum class TokenType
{
	tk_keyword,
	tk_operator,
	tk_special_symbol,
	tk_identifier,
	tk_constant,
	tk_string,
	tk_eof,
};

struct SourcePosition
{
	int line;
	int column;
};

class Token
{
protected:
	enum TokenType type;
	struct SourcePosition start;
	struct SourcePosition end;
};

class TokenSymbol : Token
{
private:
	enum TokenName name;  // keyword, opeartor and special symbol
};

class TokenIdentifier : Token
{
private:
	std::string name;  // Identifier
};

template <class T>
class TokenLiteral : Token
{
private:
	T value;  // constant, string
};
