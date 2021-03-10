#include "gtest/gtest.h"
#include "src/scan/lexer.h"

TEST(TestCaseName, TestName) {
	init_keywords();
	Lexer lexer = Lexer("char *d = \"\\1802\"");
	lexer.scan();
}
