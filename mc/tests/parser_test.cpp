#include "ast/parser.h"

#include <filesystem>
#include <regex>

#include "ast/stmt.h"
#include "ast/type.h"
#include "gtest/gtest.h"
#include "preprocesssor/preprocessor.h"
#include "scan/lexer.h"

std::shared_ptr<Program> parse(std::string content)
{
	init_keywords();
	Lexer lexer(content);

	std::filesystem::path current_path = __FILE__;
	std::filesystem::path library_path = "C:\\Program Files\\mingw-w64\\x86_64-8.1.0-posix-seh-rt_v6-rev0\\mingw64\\lib\\gcc\\x86_64-w64-mingw32\\8.1.0\\include-fixed";
	std::vector<std::string> libraries_path = {library_path.string()};
	Config config(libraries_path, current_path.parent_path().string());
	Preprocessor preprocess(content, lexer.scan(), std::make_shared<Config>(config));

	Parser parser(preprocess.process());
	return parser.parse();
}

TEST(ASTExtern, IntDeclaration_OnlyInt)
{
	auto program = parse("int;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	ASSERT_EQ(stmt->declarators.size(), 0);

	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::int_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
}

TEST(ASTExtern, UnsignedIntDeclaration_WithStaticStorage)
{
	auto program = parse("static unsigned int foo;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::unsigned_int);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::static_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "foo");
	ASSERT_EQ(std::get<2>(declarator), nullptr);

	auto declarator_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(declarator));
	ASSERT_EQ(declarator_type, type);
}

TEST(ASTExtern, SignedCharDeclaration_WithConstQualiferAndPointerDeclarator)
{
	auto program = parse("const signed char *foo;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::signed_char);
	ASSERT_EQ(*type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "foo");
	ASSERT_EQ(std::get<2>(declarator), nullptr);

	auto declarator_type = std::static_pointer_cast<PointerTypeAST>(std::get<1>(declarator));
	ASSERT_EQ(declarator_type->kind, TypeKind::pointer);
	ASSERT_EQ(declarator_type->underlay, type);
	ASSERT_EQ(declarator_type->qualifiers.size(), 0);
}

TEST(ASTExtern, DoubleDeclaration_WithStaticStorageAndConstQualiferAndFunctionDeclarator)
{
	auto program = parse("const static double (*foo)(int baz, void *);");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::double_);
	ASSERT_EQ(*type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(type->storage, StorageSpecifier::static_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "foo");
	ASSERT_EQ(std::get<2>(declarator), nullptr);

	auto declarator_type = std::static_pointer_cast<PointerTypeAST>(std::get<1>(declarator));
	ASSERT_EQ(declarator_type->kind, TypeKind::pointer);
	ASSERT_EQ(declarator_type->qualifiers.size(), 0);

	auto underlay_type = std::static_pointer_cast<FunctionTypeAST>(declarator_type->underlay);
	ASSERT_EQ(underlay_type->kind, TypeKind::function);
	ASSERT_EQ(underlay_type->returning, type);

	auto first_parameter = underlay_type->parameters[0];
	ASSERT_EQ(first_parameter.first->lexeme, "baz");

	auto first_parameter_type = std::static_pointer_cast<BuiltinTypeAST>(first_parameter.second);
	ASSERT_EQ(first_parameter_type->kind, TypeKind::builtin);
	ASSERT_EQ(first_parameter_type->qualifiers.size(), 0);
	ASSERT_EQ(first_parameter_type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(first_parameter_type->name, BuiltinTypeName::int_);

	auto second_parameter = underlay_type->parameters[1];
	ASSERT_EQ(second_parameter.first, nullptr);

	auto second_parameter_type = std::static_pointer_cast<PointerTypeAST>(second_parameter.second);
	ASSERT_EQ(second_parameter_type->kind, TypeKind::pointer);
	ASSERT_EQ(second_parameter_type->qualifiers.size(), 0);
	auto second_parameter_type_underlay = std::static_pointer_cast<BuiltinTypeAST>(second_parameter_type->underlay);
	ASSERT_EQ(second_parameter_type_underlay->kind, TypeKind::builtin);
	ASSERT_EQ(second_parameter_type_underlay->qualifiers.size(), 0);
	ASSERT_EQ(second_parameter_type_underlay->storage, StorageSpecifier::auto_);
	ASSERT_EQ(second_parameter_type_underlay->name, BuiltinTypeName::void_);
}

TEST(ASTExtern, LongDeclaration_WithInitializer)
{
	auto program = parse("extern long x = 10;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::long_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "x");
	ASSERT_EQ(std::get<1>(declarator), type);

	auto declarator_init = std::static_pointer_cast<LiteralExprAST<int>>(std::get<2>(declarator));
	ASSERT_EQ(declarator_init->type, nullptr);
	ASSERT_EQ(declarator_init->value->value, 10);
}

TEST(ASTExtern, FloatDeclaration_WithMultiDeclarators)
{
	auto program = parse("float x, y;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::float_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);

	auto declarator1 = stmt->declarators[0];
	ASSERT_EQ(std::get<0>(declarator1)->lexeme, "x");
	ASSERT_EQ(std::get<1>(declarator1), type);
	ASSERT_EQ(std::get<2>(declarator1), nullptr);

	auto declarator2 = stmt->declarators[1];
	ASSERT_EQ(std::get<0>(declarator2)->lexeme, "y");
	ASSERT_EQ(std::get<1>(declarator2), type);
	ASSERT_EQ(std::get<2>(declarator2), nullptr);
}

TEST(ASTExtern, VoidDeclaration_WithConstQualifierAndConstPointer)
{
	auto program = parse("void const * const baz;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::void_);
	ASSERT_EQ(*type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "baz");
	ASSERT_EQ(std::get<2>(declarator), nullptr);

	auto declarator_type = std::static_pointer_cast<PointerTypeAST>(std::get<1>(declarator));
	ASSERT_EQ(declarator_type->kind, TypeKind::pointer);
	ASSERT_EQ(*declarator_type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(declarator_type->underlay, type);
}

TEST(ASTExtern, EnumDeclaration_Forward)
{
	auto program = parse("enum foo;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);
}

TEST(ASTExtern, EnumDeclaration_WithEmptyBody)
{
	auto program = parse("enum foo {};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);
}

TEST(ASTExtern, EnumDeclaration_WithConstantInitialForIdentifer)
{
	auto program = parse("enum foo { RED, GREEN = 1, BLUE = 2};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->name->lexeme, "foo");

	auto declarator1 = type->members[0];
	ASSERT_EQ(declarator1.first->lexeme, "RED");
	ASSERT_EQ(declarator1.second, nullptr);

	auto declarator2 = type->members[1];
	ASSERT_EQ(declarator2.first->lexeme, "GREEN");
	auto declarator2_expr = std::static_pointer_cast<LiteralExprAST<int>>(declarator2.second);
	ASSERT_EQ(declarator2_expr->value->value, 1);

	auto declarator3 = type->members[2];
	ASSERT_EQ(declarator3.first->lexeme, "BLUE");
	auto declarator3_expr = std::static_pointer_cast<LiteralExprAST<int>>(declarator3.second);
	ASSERT_EQ(declarator3_expr->value->value, 2);
}

TEST(ASTExtern, EnumDeclaration_WithDeclarator)
{
	auto program = parse("enum foo { red, green } baz;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->name->lexeme, "foo");

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "baz");
	ASSERT_EQ(std::get<1>(declarator), type);
	ASSERT_EQ(std::get<2>(declarator), nullptr);
}

TEST(ASTExtern, EnumDeclaration_WithDeclaratorAndInitializer)
{
	auto program = parse("enum foo { red, green } baz = red;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program->declarations.front());
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->name->lexeme, "foo");

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "baz");
	ASSERT_EQ(std::get<1>(declarator), type);
	auto declarator_value = std::static_pointer_cast<IdentifierExprAST>(std::get<2>(declarator));
	ASSERT_EQ(declarator_value->name->lexeme, "red");
}

TEST(ASTExtern, StructDeclaration_Forward)
{
}

TEST(ASTExtern, StructDeclaration_WithEmptyBody)
{
}

TEST(ASTExtern, StructDefinition_WithFlatMembers)
{
}

TEST(ASTExtern, StructDefinition_WithNestedStruct)
{
}

TEST(ASTExtern, StructDefinition_WithDeclarator)
{
}

TEST(ASTExtern, StructDefinition_WithDeclaratorAndInit)
{
}

TEST(ASTExtern, Typedef_MapIntToIdentifier)
{
}

TEST(ASTExtern, Typedef_MapArrayOfIntToIdentifier)
{
}

TEST(ASTExtern, FunctionPrototype_VoidReturnAndEmptyParameters)
{
}

TEST(ASTExtern, FunctionPrototype_IntReturnAndOneIntParameter)
{
}

TEST(ASTExtern, FunctionPrototype_StructReturnAndPointerStructParameterWithoutName)
{
}

TEST(ASTExtern, FunctionPrototype_VoidPointerReturnAndRegisterInt)
{
}

TEST(ASTExtern, FunctionPrototype_StructDefinitionAsReturnAndEnumDeclaration)
{
}

TEST(ASTExtern, FunctionDefinition_WithEmptyBody)
{
}

TEST(ASTExtern, FunctionDefinition_ContainIntDeclaration)
{
}

TEST(ASTExtern, FunctionDefinition_ContainStructDefinition)
{
}

TEST(ASTExtern, FunctionDefinition_ContainReturnStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainIfStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainIfElseStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainSwitchStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainSwitchStatementWithDefaultCase)
{
}

TEST(ASTExtern, FunctionDefinition_ContainForStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainForStatementWithEmptyInitCondIncr)
{
}

TEST(ASTExtern, FunctionDefinition_ContainForStatementWithBreak)
{
}

TEST(ASTExtern, FunctionDefinition_ContainForStatementWithContinue)
{
}

TEST(ASTExtern, FunctionDefinition_ContainWhileStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainDoWhileStatement)
{
}

TEST(ASTExtern, FunctionDefinition_ContainGotoStatement)
{
}
