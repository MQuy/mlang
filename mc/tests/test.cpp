#include "gtest/gtest.h"
#include "src/scan/lexer.h"

TEST(TestCaseName, TestName) {
	init_keywords();
	Lexer lexer = Lexer("double d = .1;");
	lexer.scan();
}
