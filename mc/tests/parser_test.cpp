#include "ast/parser.h"

#include <filesystem>
#include <regex>

#include "ast/parser.h"
#include "ast/stmt.h"
#include "ast/type.h"
#include "gtest/gtest.h"
#include "preprocesssor/preprocessor.h"
#include "scan/lexer.h"

std::vector<std::shared_ptr<ExternAST>> parse(std::string content)
{
	init_keywords();
	init_operators();
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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);

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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);

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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);

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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);

	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::long_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "x");
	ASSERT_EQ(std::get<1>(declarator), type);

	auto declarator_init = std::static_pointer_cast<LiteralExprAST<int>>(std::get<2>(declarator));
	ASSERT_EQ(declarator_init->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(declarator_init->type, nullptr);
	ASSERT_EQ(declarator_init->value->value, 10);
}

TEST(ASTExtern, FloatDeclaration_WithMultiDeclarators)
{
	auto program = parse("float x, y;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);

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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
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
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);
}

TEST(ASTExtern, EnumDeclaration_WithEmptyBodyAndAutoSpecifier)
{
	auto program = parse("auto enum foo {};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);
}

TEST(ASTExtern, EnumDeclaration_WithExternSpecifierConstantInitialForIdentifer)
{
	auto program = parse("extern enum foo { RED, GREEN = 1, BLUE = 2};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->name->lexeme, "foo");

	auto declarator1 = type->members[0];
	ASSERT_EQ(declarator1.first->lexeme, "RED");
	ASSERT_EQ(declarator1.second, nullptr);

	auto declarator2 = type->members[1];
	ASSERT_EQ(declarator2.first->lexeme, "GREEN");
	auto declarator2_expr = std::static_pointer_cast<LiteralExprAST<int>>(declarator2.second);
	ASSERT_EQ(declarator2_expr->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(declarator2_expr->value->value, 1);

	auto declarator3 = type->members[2];
	ASSERT_EQ(declarator3.first->lexeme, "BLUE");
	auto declarator3_expr = std::static_pointer_cast<LiteralExprAST<int>>(declarator3.second);
	ASSERT_EQ(declarator3_expr->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(declarator3_expr->value->value, 2);
}

TEST(ASTExtern, EnumDeclaration_WithConstQualifierAndDeclarator)
{
	auto program = parse("const enum foo { red, green } baz;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(*type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(type->name->lexeme, "foo");

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "baz");
	ASSERT_EQ(std::get<1>(declarator), type);
	ASSERT_EQ(std::get<2>(declarator), nullptr);
}

TEST(ASTExtern, EnumDeclaration_WithDeclaratorAndInitializer)
{
	auto program = parse("enum foo { red, green } baz = red;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<EnumTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::enum_);
	ASSERT_EQ(type->name->lexeme, "foo");

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "baz");
	ASSERT_EQ(std::get<1>(declarator), type);
	auto declarator_value = std::static_pointer_cast<IdentifierExprAST>(std::get<2>(declarator));
	ASSERT_EQ(declarator_value->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(declarator_value->name->lexeme, "red");
}

TEST(ASTExtern, StructDeclaration_Forward)
{
	auto program = parse("struct foo;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	ASSERT_EQ(stmt->declarators.size(), 0);

	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);
}

TEST(ASTExtern, StructDeclaration_WithVolatileQualifierAndEmptyBody)
{
	auto program = parse("volatile struct foo {};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	ASSERT_EQ(stmt->declarators.size(), 0);

	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(*type->qualifiers.begin(), TypeQualifier::volatile_);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);
}

TEST(ASTExtern, StructDefinition_WithConstQualifierAndFlatMembers)
{
	auto program = parse(
		"const struct foo {\n"
		"	int x, y;\n"
		"	float z;\n"
		"};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	ASSERT_EQ(stmt->declarators.size(), 0);

	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(*type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 3);

	auto member1 = type->members[0];
	ASSERT_EQ(std::get<0>(member1)->lexeme, "x");
	auto member1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(member1));
	ASSERT_EQ(member1_type->kind, TypeKind::builtin);
	ASSERT_EQ(member1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(member1_type->qualifiers.size(), 0);
	ASSERT_EQ(member1_type->storage, StorageSpecifier::auto_);

	auto member2 = type->members[1];
	ASSERT_EQ(std::get<0>(member2)->lexeme, "y");
	auto member2_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(member2));
	ASSERT_EQ(member2_type->kind, TypeKind::builtin);
	ASSERT_EQ(member2_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(member2_type->qualifiers.size(), 0);
	ASSERT_EQ(member2_type->storage, StorageSpecifier::auto_);

	auto member3 = type->members[2];
	ASSERT_EQ(std::get<0>(member3)->lexeme, "z");
	auto member3_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(member3));
	ASSERT_EQ(member3_type->kind, TypeKind::builtin);
	ASSERT_EQ(member3_type->name, BuiltinTypeName::float_);
	ASSERT_EQ(member3_type->qualifiers.size(), 0);
	ASSERT_EQ(member3_type->storage, StorageSpecifier::auto_);
}

TEST(ASTExtern, StructDefinition_WithNestedNameAndUnNameStruct)
{
	auto program = parse(
		"struct foo {\n"
		"	const int x;\n"
		"	volatile struct baz y;\n"
		"	struct { char qux; } *z;\n"
		"};");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	ASSERT_EQ(stmt->declarators.size(), 0);

	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 3);

	auto member1 = type->members[0];
	ASSERT_EQ(std::get<0>(member1)->lexeme, "x");
	auto member1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(member1));
	ASSERT_EQ(member1_type->kind, TypeKind::builtin);
	ASSERT_EQ(member1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(*member1_type->qualifiers.begin(), TypeQualifier::const_);
	ASSERT_EQ(member1_type->storage, StorageSpecifier::auto_);

	auto member2 = type->members[1];
	ASSERT_EQ(std::get<0>(member2)->lexeme, "y");
	auto member2_type = std::static_pointer_cast<AggregateTypeAST>(std::get<1>(member2));
	ASSERT_EQ(member2_type->kind, TypeKind::aggregate);
	ASSERT_EQ(member2_type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(*member2_type->qualifiers.begin(), TypeQualifier::volatile_);
	ASSERT_EQ(member2_type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(member2_type->name->lexeme, "baz");
	ASSERT_EQ(member2_type->members.size(), 0);

	auto member3 = type->members[2];
	ASSERT_EQ(std::get<0>(member3)->lexeme, "z");
	auto member3_type = std::static_pointer_cast<PointerTypeAST>(std::get<1>(member3));
	ASSERT_EQ(member3_type->kind, TypeKind::pointer);
	ASSERT_EQ(member3_type->qualifiers.size(), 0);

	auto member3_underlay_type = std::static_pointer_cast<AggregateTypeAST>(member3_type->underlay);
	ASSERT_EQ(member3_underlay_type->kind, TypeKind::aggregate);
	ASSERT_EQ(member3_underlay_type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(member3_underlay_type->qualifiers.size(), 0);
	ASSERT_EQ(member3_underlay_type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(member3_underlay_type->name, nullptr);
	ASSERT_EQ(member3_underlay_type->members.size(), 1);

	auto submember1 = member3_underlay_type->members[0];
	ASSERT_EQ(std::get<0>(submember1)->lexeme, "qux");
	auto submember1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(submember1));
	ASSERT_EQ(submember1_type->kind, TypeKind::builtin);
	ASSERT_EQ(submember1_type->name, BuiltinTypeName::char_);
	ASSERT_EQ(submember1_type->qualifiers.size(), 0);
	ASSERT_EQ(submember1_type->storage, StorageSpecifier::auto_);
}

TEST(ASTExtern, StructDefinition_WithDeclarator)
{
	auto program = parse("struct foo {} x;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 0);

	auto declarator = stmt->declarators[0];
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "x");
	ASSERT_EQ(std::get<1>(declarator), type);
	ASSERT_EQ(std::get<2>(declarator), nullptr);
}

TEST(ASTExtern, StructDefinition_WithDeclaratorAndInit)
{
	auto program = parse(
		"struct foo {\n"
		"	int x;\n"
		"} xxx = { 1, { 2 } };");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->name->lexeme, "foo");
	ASSERT_EQ(type->members.size(), 1);

	auto declarator = stmt->declarators[0];
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "xxx");
	ASSERT_EQ(std::get<1>(declarator), type);

	auto declarator_init = std::static_pointer_cast<InitializerExprAST>(std::get<2>(declarator));
	ASSERT_EQ(declarator_init->node_type, ASTNodeType::expr_initializer);
	ASSERT_EQ(declarator_init->exprs.size(), 2);

	auto member1 = std::static_pointer_cast<LiteralExprAST<int>>(declarator_init->exprs[0]);
	ASSERT_EQ(member1->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(member1->type, nullptr);
	ASSERT_EQ(member1->value->value, 1);

	auto member2 = std::static_pointer_cast<InitializerExprAST>(declarator_init->exprs[1]);
	ASSERT_EQ(member2->node_type, ASTNodeType::expr_initializer);
	ASSERT_EQ(member2->exprs.size(), 1);

	auto member2_member1 = std::static_pointer_cast<LiteralExprAST<int>>(member2->exprs[0]);
	ASSERT_EQ(member2_member1->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(member2_member1->type, nullptr);
	ASSERT_EQ(member2_member1->value->value, 2);
}

TEST(ASTExtern, Typedef_IntToIdentifier)
{
	auto program = parse("typedef int int32_t;");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::int_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::typedef_);

	auto alias = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(alias)->lexeme, "int32_t");
	ASSERT_EQ(std::get<1>(alias), type);
	ASSERT_EQ(std::get<2>(alias), nullptr);
}

TEST(ASTExtern, Typedef_ArrayOfIntToIdentifier)
{
	auto program = parse("typedef int A[];");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::int_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::typedef_);

	auto alias = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(alias)->lexeme, "A");
	ASSERT_EQ(std::get<2>(alias), nullptr);

	auto alias_type = std::static_pointer_cast<ArrayTypeAST>(std::get<1>(alias));
	ASSERT_EQ(alias_type->kind, TypeKind::array);
	ASSERT_EQ(alias_type->expr, nullptr);

	auto underlay_type = std::static_pointer_cast<BuiltinTypeAST>(alias_type->underlay);
	ASSERT_EQ(underlay_type, type);
}

TEST(ASTExtern, Typedef_MultiDelaratorsHavingPointerAndFunction)
{
	auto program = parse("typedef char char_t, *char_p, (*fp)(void);");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::char_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::typedef_);

	auto alias1 = stmt->declarators[0];
	ASSERT_EQ(std::get<0>(alias1)->lexeme, "char_t");
	ASSERT_EQ(std::get<1>(alias1), type);
	ASSERT_EQ(std::get<2>(alias1), nullptr);

	auto alias2 = stmt->declarators[1];
	ASSERT_EQ(std::get<0>(alias2)->lexeme, "char_p");
	ASSERT_EQ(std::get<2>(alias2), nullptr);

	auto alias2_type = std::static_pointer_cast<PointerTypeAST>(std::get<1>(alias2));
	ASSERT_EQ(alias2_type->kind, TypeKind::pointer);
	ASSERT_EQ(alias2_type->qualifiers.size(), 0);
	auto alias2_underlay_type = std::static_pointer_cast<BuiltinTypeAST>(alias2_type->underlay);
	ASSERT_EQ(alias2_underlay_type, type);

	auto alias3 = stmt->declarators[2];
	ASSERT_EQ(std::get<0>(alias3)->lexeme, "fp");
	ASSERT_EQ(std::get<2>(alias3), nullptr);

	auto alias3_type = std::static_pointer_cast<PointerTypeAST>(std::get<1>(alias3));
	ASSERT_EQ(alias3_type->kind, TypeKind::pointer);
	ASSERT_EQ(alias3_type->qualifiers.size(), 0);
	auto alias3_underlay_type = std::static_pointer_cast<FunctionTypeAST>(alias3_type->underlay);
	ASSERT_EQ(alias3_underlay_type->kind, TypeKind::function);
	ASSERT_EQ(alias3_underlay_type->parameters.size(), 0);
	ASSERT_EQ(alias3_underlay_type->returning, type);
}

TEST(ASTExtern, FunctionPrototype_VoidReturnAndEmptyParameters)
{
	auto program = parse("void foo();");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->name, BuiltinTypeName::void_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "foo");
	ASSERT_EQ(std::get<2>(declarator), nullptr);

	auto declarator_type = std::static_pointer_cast<FunctionTypeAST>(std::get<1>(declarator));
	ASSERT_EQ(declarator_type->kind, TypeKind::function);
	ASSERT_EQ(declarator_type->parameters.size(), 0);
	ASSERT_EQ(declarator_type->returning, type);
}

TEST(ASTExtern, FunctionPrototype_StaticWithIntReturnAndOneDoubleParameter)
{
	auto program = parse("static int LongFunctionName(double x);");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::static_);
	ASSERT_EQ(type->name, BuiltinTypeName::int_);

	auto declarator = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator)->lexeme, "LongFunctionName");
	ASSERT_EQ(std::get<2>(declarator), nullptr);

	auto declarator_type = std::static_pointer_cast<FunctionTypeAST>(std::get<1>(declarator));
	ASSERT_EQ(declarator_type->kind, TypeKind::function);
	ASSERT_EQ(declarator_type->returning, type);

	auto parameter1 = declarator_type->parameters.front();
	ASSERT_EQ(std::get<0>(parameter1)->lexeme, "x");

	auto paramter1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(parameter1));
	ASSERT_EQ(paramter1_type->kind, TypeKind::builtin);
	ASSERT_EQ(paramter1_type->qualifiers.size(), 0);
	ASSERT_EQ(paramter1_type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(paramter1_type->name, BuiltinTypeName::double_);
}

TEST(ASTExtern, FunctionPrototype_UnionReturnAndPointerStructParameterWithoutName)
{
	auto program = parse("union point LongFunctionName(struct foo);");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::union_);
	ASSERT_EQ(type->members.size(), 0);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->name->lexeme, "point");

	auto declarator1 = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator1)->lexeme, "LongFunctionName");
	ASSERT_EQ(std::get<2>(declarator1), nullptr);

	auto declarator1_type = std::static_pointer_cast<FunctionTypeAST>(std::get<1>(declarator1));
	ASSERT_EQ(declarator1_type->kind, TypeKind::function);
	ASSERT_EQ(declarator1_type->parameters.size(), 1);
	ASSERT_EQ(declarator1_type->returning, type);

	auto parameter1 = declarator1_type->parameters.front();
	ASSERT_EQ(std::get<0>(parameter1), nullptr);

	auto parameter1_type = std::static_pointer_cast<AggregateTypeAST>(std::get<1>(parameter1));
	ASSERT_EQ(parameter1_type->kind, TypeKind::aggregate);
	ASSERT_EQ(parameter1_type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(parameter1_type->members.size(), 0);
	ASSERT_EQ(parameter1_type->qualifiers.size(), 0);
	ASSERT_EQ(parameter1_type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(parameter1_type->name->lexeme, "foo");
}

TEST(ASTExtern, FunctionPrototype_VoidPointerReturnAndRegisterInt)
{
	auto program = parse("void *foo(int register x);");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<BuiltinTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::builtin);
	ASSERT_EQ(type->name, BuiltinTypeName::void_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);

	auto declarator1 = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator1)->lexeme, "foo");
	ASSERT_EQ(std::get<2>(declarator1), nullptr);

	auto declarator1_type = std::static_pointer_cast<FunctionTypeAST>(std::get<1>(declarator1));
	ASSERT_EQ(declarator1_type->kind, TypeKind::function);
	ASSERT_EQ(declarator1_type->parameters.size(), 1);

	auto declarator1_underlay_type = std::static_pointer_cast<PointerTypeAST>(declarator1_type->returning);
	ASSERT_EQ(declarator1_underlay_type->kind, TypeKind::pointer);
	ASSERT_EQ(declarator1_underlay_type->qualifiers.size(), 0);

	auto parameter1 = declarator1_type->parameters.front();
	ASSERT_EQ(std::get<0>(parameter1)->lexeme, "x");

	auto parameter1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(parameter1));
	ASSERT_EQ(parameter1_type->kind, TypeKind::builtin);
	ASSERT_EQ(parameter1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(parameter1_type->qualifiers.size(), 0);
	ASSERT_EQ(parameter1_type->storage, StorageSpecifier::register_);
}

TEST(ASTExtern, FunctionPrototype_StructDefinitionAsReturnAndEnumDeclaration)
{
	auto program = parse("struct foo { int x; } baz(enum qux { RED });");
	auto stmt = std::static_pointer_cast<DeclarationAST>(program.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::extern_declaration);
	auto type = std::static_pointer_cast<AggregateTypeAST>(stmt->type);
	ASSERT_EQ(type->kind, TypeKind::aggregate);
	ASSERT_EQ(type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(type->qualifiers.size(), 0);
	ASSERT_EQ(type->storage, StorageSpecifier::extern_);
	ASSERT_EQ(type->members.size(), 1);

	auto struct_member1 = type->members.front();
	ASSERT_EQ(std::get<0>(struct_member1)->lexeme, "x");

	auto struct_member1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(struct_member1));
	ASSERT_EQ(struct_member1_type->kind, TypeKind::builtin);
	ASSERT_EQ(struct_member1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(struct_member1_type->qualifiers.size(), 0);
	ASSERT_EQ(struct_member1_type->storage, StorageSpecifier::auto_);

	auto declarator1 = stmt->declarators.front();
	ASSERT_EQ(std::get<0>(declarator1)->lexeme, "baz");
	ASSERT_EQ(std::get<2>(declarator1), nullptr);

	auto declarator1_type = std::static_pointer_cast<FunctionTypeAST>(std::get<1>(declarator1));
	ASSERT_EQ(declarator1_type->kind, TypeKind::function);
	ASSERT_EQ(declarator1_type->parameters.size(), 1);
	ASSERT_EQ(declarator1_type->returning, type);

	auto declarator1_parameter1 = declarator1_type->parameters.front();
	ASSERT_EQ(std::get<0>(declarator1_parameter1), nullptr);

	auto declarator1_parameter1_type = std::static_pointer_cast<EnumTypeAST>(std::get<1>(declarator1_parameter1));
	ASSERT_EQ(declarator1_parameter1_type->kind, TypeKind::enum_);
	ASSERT_EQ(declarator1_parameter1_type->qualifiers.size(), 0);
	ASSERT_EQ(declarator1_parameter1_type->storage, StorageSpecifier::auto_);
	ASSERT_EQ(declarator1_parameter1_type->members.size(), 1);
	ASSERT_EQ(declarator1_parameter1_type->name->lexeme, "qux");

	auto parameter1_member1 = declarator1_parameter1_type->members.front();
	ASSERT_EQ(std::get<0>(parameter1_member1)->lexeme, "RED");
	ASSERT_EQ(std::get<1>(parameter1_member1), nullptr);
}

TEST(ASTExtern, FunctionDefinition_WithEmptyBody)
{
	auto program = parse("int foo() {}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	ASSERT_EQ(func->name->lexeme, "foo");
	ASSERT_EQ(func->body->stmts.size(), 0);

	auto type = std::static_pointer_cast<FunctionTypeAST>(func->type);
	ASSERT_EQ(type->kind, TypeKind::function);
	ASSERT_EQ(type->parameters.size(), 0);

	auto return_type = std::static_pointer_cast<BuiltinTypeAST>(type->returning);
	ASSERT_EQ(return_type->kind, TypeKind::builtin);
	ASSERT_EQ(return_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(return_type->qualifiers.size(), 0);
	ASSERT_EQ(return_type->storage, StorageSpecifier::extern_);
}

TEST(ASTExtern, FunctionDefinition_ContainIntDeclaration)
{
	auto program = parse(
		"void foo() {\n"
		"	int x;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	ASSERT_EQ(func->body->stmts.size(), 1);

	auto stmt1 = std::static_pointer_cast<DeclarationAST>(func->body->stmts.front());
	auto stmt1_type = std::static_pointer_cast<BuiltinTypeAST>(stmt1->type);
	ASSERT_EQ(stmt1_type->kind, TypeKind::builtin);
	ASSERT_EQ(stmt1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(stmt1_type->qualifiers.size(), 0);
	ASSERT_EQ(stmt1_type->storage, StorageSpecifier::auto_);

	auto stmt1_declarator1 = stmt1->declarators.front();
	ASSERT_EQ(std::get<0>(stmt1_declarator1)->lexeme, "x");
	ASSERT_EQ(std::get<1>(stmt1_declarator1), stmt1_type);
	ASSERT_EQ(std::get<2>(stmt1_declarator1), nullptr);
}

TEST(ASTExtern, FunctionDefinition_ContainStructDefinition)
{
	auto program = parse(
		"void foo() {\n"
		"	struct baz { int x; } qux = {};\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	ASSERT_EQ(func->body->stmts.size(), 1);

	auto stmt1 = std::static_pointer_cast<DeclarationAST>(func->body->stmts.front());
	auto stmt1_type = std::static_pointer_cast<AggregateTypeAST>(stmt1->type);
	ASSERT_EQ(stmt1_type->kind, TypeKind::aggregate);
	ASSERT_EQ(stmt1_type->aggregate_kind, AggregateKind::struct_);
	ASSERT_EQ(stmt1_type->name->lexeme, "baz");
	ASSERT_EQ(stmt1_type->members.size(), 1);
	ASSERT_EQ(stmt1_type->storage, StorageSpecifier::auto_);

	auto member1 = stmt1_type->members.front();
	ASSERT_EQ(std::get<0>(member1)->lexeme, "x");
	auto member1_type = std::static_pointer_cast<BuiltinTypeAST>(std::get<1>(member1));
	ASSERT_EQ(member1_type->kind, TypeKind::builtin);
	ASSERT_EQ(member1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(member1_type->qualifiers.size(), 0);
	ASSERT_EQ(member1_type->storage, StorageSpecifier::auto_);

	auto stmt1_declarator1 = stmt1->declarators.front();
	ASSERT_EQ(std::get<0>(stmt1_declarator1)->lexeme, "qux");
	ASSERT_EQ(std::get<1>(stmt1_declarator1), stmt1_type);

	auto stmt1_declarator1_value = std::static_pointer_cast<InitializerExprAST>(std::get<2>(stmt1_declarator1));
	ASSERT_EQ(stmt1_declarator1_value->node_type, ASTNodeType::expr_initializer);
	ASSERT_EQ(stmt1_declarator1_value->exprs.size(), 0);
}

TEST(ASTExtern, FunctionDefinition_ContainReturnStatement)
{
	auto program = parse(
		"int foo() {\n"
		"	return 1;"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ReturnStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_return);

	auto expr1 = std::static_pointer_cast<LiteralExprAST<int>>(stmt1->expr);
	ASSERT_EQ(expr1->value->value, 1);
}

TEST(ASTExtern, FunctionDefinition_ContainIfStatement)
{
	auto program = parse(
		"void foo() {\n"
		"	if (1)\n"
		"		;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<IfStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_if);
	ASSERT_EQ(stmt1->else_stmt, nullptr);

	auto cond1 = std::static_pointer_cast<LiteralExprAST<int>>(stmt1->cond);
	ASSERT_EQ(cond1->value->value, 1);

	auto ifbody1 = std::static_pointer_cast<ExprStmtAST>(stmt1->if_stmt);
	ASSERT_EQ(ifbody1->node_type, ASTNodeType::stmt_expr);
	ASSERT_EQ(ifbody1->expr, nullptr);
}

TEST(ASTExtern, FunctionDefinition_ContainIfElseStatement)
{
	auto program = parse(
		"void foo() {\n"
		"	if (1)\n"
		"		return;\n"
		"	else if (2)\n"
		"		return;\n"
		"	else\n"
		"		return;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<IfStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_if);
	auto cond1 = std::static_pointer_cast<LiteralExprAST<int>>(stmt1->cond);
	ASSERT_EQ(cond1->value->value, 1);

	auto ifbody1 = std::static_pointer_cast<ReturnStmtAST>(stmt1->if_stmt);
	ASSERT_EQ(ifbody1->node_type, ASTNodeType::stmt_return);
	ASSERT_EQ(ifbody1->expr, nullptr);

	auto elsebody1 = std::static_pointer_cast<IfStmtAST>(stmt1->else_stmt);
	ASSERT_EQ(elsebody1->node_type, ASTNodeType::stmt_if);
	auto cond2 = std::static_pointer_cast<LiteralExprAST<int>>(elsebody1->cond);
	ASSERT_EQ(cond2->value->value, 2);

	auto ifbody2 = std::static_pointer_cast<ReturnStmtAST>(elsebody1->if_stmt);
	ASSERT_EQ(ifbody2->node_type, ASTNodeType::stmt_return);
	ASSERT_EQ(ifbody2->expr, nullptr);

	auto elsebody2 = std::static_pointer_cast<ReturnStmtAST>(elsebody1->else_stmt);
	ASSERT_EQ(elsebody2->node_type, ASTNodeType::stmt_return);
	ASSERT_EQ(elsebody2->expr, nullptr);
}

TEST(ASTExtern, FunctionDefinition_ContainSwitchStatement)
{
	auto program = parse(
		"void foo() {\n"
		"	switch(x) { }\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<SwitchStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_switch);
	auto expr = std::static_pointer_cast<IdentifierExprAST>(stmt1->expr);
	ASSERT_EQ(expr->name->name, "x");

	auto body = std::static_pointer_cast<CompoundStmtAST>(stmt1->stmt);
	ASSERT_EQ(body->node_type, ASTNodeType::stmt_compound);
	ASSERT_EQ(body->stmts.size(), 0);
}

TEST(ASTExtern, FunctionDefinition_ContainSwitchStatementWithCaseAndDefault)
{
	auto program = parse(
		"void foo() {\n"
		"	switch(baz) {\n"
		"		case 1:\n"
		"			break;\n"
		"		default:\n"
		"			break;\n"
		"	}\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<SwitchStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_switch);
	auto expr = std::static_pointer_cast<IdentifierExprAST>(stmt1->expr);
	ASSERT_EQ(expr->name->name, "baz");

	auto body = std::static_pointer_cast<CompoundStmtAST>(stmt1->stmt);
	ASSERT_EQ(body->node_type, ASTNodeType::stmt_compound);
	ASSERT_EQ(body->stmts.size(), 2);

	auto stmt2 = std::static_pointer_cast<CaseStmtAST>(body->stmts[0]);
	ASSERT_EQ(stmt2->node_type, ASTNodeType::stmt_case);
	auto cond2 = std::static_pointer_cast<LiteralExprAST<int>>(stmt2->constant);
	ASSERT_EQ(cond2->value->value, 1);

	auto stmt3 = std::static_pointer_cast<DefaultStmtAST>(body->stmts[1]);
	ASSERT_EQ(stmt3->node_type, ASTNodeType::stmt_default_);
}

TEST(ASTExtern, FunctionDefinition_ContainForStatementWithEmptyInitCondIncr)
{
	auto program = parse(
		"void foo() {\n"
		"	for(;;)\n"
		"		;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ForStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_for);
	ASSERT_EQ(stmt1->cond, nullptr);
	ASSERT_EQ(stmt1->init, nullptr);
	ASSERT_EQ(stmt1->inc, nullptr);

	auto forbody = std::static_pointer_cast<ExprStmtAST>(stmt1->stmt);
	ASSERT_EQ(forbody->node_type, ASTNodeType::stmt_expr);
	ASSERT_EQ(forbody->expr, nullptr);
}

TEST(ASTExtern, FunctionDefinition_ContainForStatementWithBreak)
{
	auto program = parse(
		"void foo() {\n"
		"	for(int x = 10; x > 0 ; ++x)\n"
		"		break;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ForStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_for);

	auto init1 = std::static_pointer_cast<DeclarationAST>(stmt1->init);
	ASSERT_EQ(init1->node_type, ASTNodeType::extern_declaration);

	auto init1_type = std::static_pointer_cast<BuiltinTypeAST>(init1->type);
	ASSERT_EQ(init1_type->kind, TypeKind::builtin);
	ASSERT_EQ(init1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(init1_type->qualifiers.size(), 0);
	ASSERT_EQ(init1_type->storage, StorageSpecifier::auto_);

	auto init1_declarator1 = init1->declarators.front();
	ASSERT_EQ(std::get<0>(init1_declarator1)->lexeme, "x");
	ASSERT_EQ(std::get<1>(init1_declarator1), init1_type);

	auto init1_declarator1_value = std::static_pointer_cast<LiteralExprAST<int>>(std::get<2>(init1_declarator1));
	ASSERT_EQ(init1_declarator1_value->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(init1_declarator1_value->value->value, 10);

	auto cond1 = std::static_pointer_cast<BinaryExprAST>(stmt1->cond);
	ASSERT_EQ(cond1->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(cond1->op, BinaryOperator::greater_than);

	auto cond1_left = std::static_pointer_cast<IdentifierExprAST>(cond1->left);
	ASSERT_EQ(cond1_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(cond1_left->name->lexeme, "x");

	auto cond1_right = std::static_pointer_cast<LiteralExprAST<int>>(cond1->right);
	ASSERT_EQ(cond1_right->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(cond1_right->value->value, 0);

	auto inc1 = std::static_pointer_cast<UnaryExprAST>(stmt1->inc);
	ASSERT_EQ(inc1->node_type, ASTNodeType::expr_unary);
	ASSERT_EQ(inc1->op, UnaryOperator::prefix_increment);

	auto inc1_expr = std::static_pointer_cast<IdentifierExprAST>(inc1->expr);
	ASSERT_EQ(inc1_expr->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(inc1_expr->name->lexeme, "x");

	auto stmt2 = std::static_pointer_cast<BreakStmtAST>(stmt1->stmt);
	ASSERT_EQ(stmt2->node_type, ASTNodeType::stmt_break);
}

TEST(ASTExtern, FunctionDefinition_ContainWhileStatementWithContinue)
{
	auto program = parse(
		"void foo() {\n"
		"	while(1)\n"
		"		continue;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<WhileStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_while);

	auto cond1 = std::static_pointer_cast<LiteralExprAST<int>>(stmt1->cond);
	ASSERT_EQ(cond1->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(cond1->value->value, 1);

	auto stmt2 = std::static_pointer_cast<ContinueStmtAST>(stmt1->stmt);
	ASSERT_EQ(stmt2->node_type, ASTNodeType::stmt_continue);
}

TEST(ASTExtern, FunctionDefinition_ContainDoWhileStatement)
{
	auto program = parse(
		"void foo() {\n"
		"	do {\n"
		"		return;\n"
		"	} while(x);\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<DoWhileStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_dowhile);

	auto cond1 = std::static_pointer_cast<IdentifierExprAST>(stmt1->cond);
	ASSERT_EQ(cond1->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(cond1->name->lexeme, "x");

	auto stmt2 = std::static_pointer_cast<CompoundStmtAST>(stmt1->stmt);
	ASSERT_EQ(stmt2->node_type, ASTNodeType::stmt_compound);
	ASSERT_EQ(stmt2->stmts.size(), 1);

	auto stmt2_sub = std::static_pointer_cast<ReturnStmtAST>(stmt2->stmts[0]);
	ASSERT_EQ(stmt2_sub->node_type, ASTNodeType::stmt_return);
	ASSERT_EQ(stmt2_sub->expr, nullptr);
}

TEST(ASTExtern, FunctionDefinition_ContainLabelStatement)
{
	auto program = parse(
		"void foo() {\n"
		"	anchor:\n"
		"		return;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<LabelStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_label);
	ASSERT_EQ(stmt1->name->lexeme, "anchor");

	auto stmt2 = std::static_pointer_cast<ReturnStmtAST>(stmt1->stmt);
	ASSERT_EQ(stmt2->node_type, ASTNodeType::stmt_return);
	ASSERT_EQ(stmt2->expr, nullptr);
}

TEST(ASTExtern, FunctionDefinition_ContainGotoStatement)
{
	auto program = parse(
		"void foo() {\n"
		"	goto anchor;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<JumpStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_jump);
	ASSERT_EQ(stmt1->name->lexeme, "anchor");
}

TEST(ASTExtern, FunctionDefinition_BinaryExpr)
{
	std::vector<std::pair<std::string, BinaryOperator>> binops = {
		std::make_pair("=", BinaryOperator::assignment),
		std::make_pair("+=", BinaryOperator::addition_assigment),
		std::make_pair("-=", BinaryOperator::subtraction_assignment),
		std::make_pair("*=", BinaryOperator::multiplication_assigment),
		std::make_pair("/=", BinaryOperator::division_assignment),
		std::make_pair("%=", BinaryOperator::remainder_assignment),
		std::make_pair("&=", BinaryOperator::bitwise_and_assigment),
		std::make_pair("|=", BinaryOperator::bitwise_or_assigment),
		std::make_pair("^=", BinaryOperator::bitwise_xor_assigment),
		std::make_pair("<<=", BinaryOperator::shift_left_assignment),
		std::make_pair(">>=", BinaryOperator::shift_right_assignment),

		std::make_pair("+", BinaryOperator::addition),
		std::make_pair("-", BinaryOperator::subtraction),
		std::make_pair("*", BinaryOperator::multiplication),
		std::make_pair("/", BinaryOperator::division),
		std::make_pair("%", BinaryOperator::remainder),
		std::make_pair("&", BinaryOperator::bitwise_and),
		std::make_pair("|", BinaryOperator::bitwise_or),
		std::make_pair("^", BinaryOperator::bitwise_xor),
		std::make_pair("<<", BinaryOperator::shift_left),
		std::make_pair(">>", BinaryOperator::shift_right),

		std::make_pair("&&", BinaryOperator::and_),
		std::make_pair("||", BinaryOperator::or_),

		std::make_pair("==", BinaryOperator::equal),
		std::make_pair("!=", BinaryOperator::not_equal),
		std::make_pair("<", BinaryOperator::less),
		std::make_pair(">", BinaryOperator::greater_than),
		std::make_pair("<=", BinaryOperator::less_or_equal),
		std::make_pair(">=", BinaryOperator::greater_or_equal),

		std::make_pair(",", BinaryOperator::comma),
	};

	for (auto [name, op] : binops)
	{
		auto program = parse(
			"void foo() {\n"
			"	x "
			+ name +
			" 1;\n"
			"}");
		auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
		ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
		auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
		ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

		auto expr1 = std::static_pointer_cast<BinaryExprAST>(stmt1->expr);
		ASSERT_EQ(expr1->node_type, ASTNodeType::expr_binary);
		ASSERT_EQ(expr1->op, op);

		auto expr1_left = std::static_pointer_cast<IdentifierExprAST>(expr1->left);
		ASSERT_EQ(expr1_left->node_type, ASTNodeType::expr_identifier);
		ASSERT_EQ(expr1_left->name->lexeme, "x");

		auto expr1_right = std::static_pointer_cast<LiteralExprAST<int>>(expr1->right);
		ASSERT_EQ(expr1_right->node_type, ASTNodeType::expr_literal);
		ASSERT_EQ(expr1_right->value->value, 1);
	}
}

TEST(ASTExtern, FunctionDefinition_TenaryExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	1 ? 2 : 3;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<TenaryExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_tenary);

	auto cond1 = std::static_pointer_cast<LiteralExprAST<int>>(expr1->cond);
	ASSERT_EQ(cond1->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(cond1->value->value, 1);

	auto subexpr1 = std::static_pointer_cast<LiteralExprAST<int>>(expr1->expr1);
	ASSERT_EQ(subexpr1->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(subexpr1->value->value, 2);

	auto subexpr2 = std::static_pointer_cast<LiteralExprAST<int>>(expr1->expr2);
	ASSERT_EQ(subexpr2->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(subexpr2->value->value, 3);
}

TEST(ASTExtern, FunctionDefinition_UnaryExpr)
{
	std::vector<std::pair<std::string, UnaryOperator>> binops = {
		std::make_pair("++", UnaryOperator::prefix_increment),
		std::make_pair("--", UnaryOperator::prefix_decrement),

		std::make_pair("-", UnaryOperator::minus),
		std::make_pair("+", UnaryOperator::plus),
		std::make_pair("~", UnaryOperator::complement),

		std::make_pair("!", UnaryOperator::not_),

		std::make_pair("*", UnaryOperator::dereference),
		std::make_pair("&", UnaryOperator::address_of),
	};

	for (auto [name, op] : binops)
	{
		auto program = parse(
			"void foo() {\n"
			"	"
			+ name +
			"x;\n"
			"}");
		auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
		ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
		auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
		ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

		auto expr1 = std::static_pointer_cast<UnaryExprAST>(stmt1->expr);
		ASSERT_EQ(expr1->node_type, ASTNodeType::expr_unary);
		ASSERT_EQ(expr1->op, op);

		auto expr1_left = std::static_pointer_cast<IdentifierExprAST>(expr1->expr);
		ASSERT_EQ(expr1_left->node_type, ASTNodeType::expr_identifier);
		ASSERT_EQ(expr1_left->name->lexeme, "x");
	}
}

TEST(ASTExtern, FunctionDefinition_FunctionCallExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	baz();\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<FunctionCallExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_function_call);
	ASSERT_EQ(expr1->arguments.size(), 0);

	auto identifier = std::static_pointer_cast<IdentifierExprAST>(expr1->callee);
	ASSERT_EQ(identifier->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(identifier->name->lexeme, "baz");
}

TEST(ASTExtern, FunctionDefinition_CastExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	(int)x;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<TypeCastExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_typecast);

	auto expr1_type = std::static_pointer_cast<BuiltinTypeAST>(expr1->type);
	ASSERT_EQ(expr1_type->kind, TypeKind::builtin);
	ASSERT_EQ(expr1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(expr1_type->qualifiers.size(), 0);
	ASSERT_EQ(expr1_type->storage, StorageSpecifier::auto_);

	auto identifier = std::static_pointer_cast<IdentifierExprAST>(expr1->expr);
	ASSERT_EQ(identifier->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(identifier->name->lexeme, "x");
}

TEST(ASTExtern, FunctionDefinition_MemberAccessExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	x.y;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<MemberAccessExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_member_access);
	ASSERT_EQ(expr1->access_type, MemberAccessType::dot);
	ASSERT_EQ(expr1->member->lexeme, "y");

	auto object = std::static_pointer_cast<IdentifierExprAST>(expr1->object);
	ASSERT_EQ(object->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(object->name->lexeme, "x");
}

TEST(ASTExtern, FunctionDefinition_ArraySubscriptExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	x[0];\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<BinaryExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(expr1->op, BinaryOperator::array_subscript);

	auto expr1_left = std::static_pointer_cast<IdentifierExprAST>(expr1->left);
	ASSERT_EQ(expr1_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(expr1_left->name->lexeme, "x");

	auto expr1_right = std::static_pointer_cast<LiteralExprAST<int>>(expr1->right);
	ASSERT_EQ(expr1_right->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(expr1_right->value->value, 0);
}

TEST(ASTExtern, FunctionDefinition_SizeOfTypeExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	sizeof(int);\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<UnaryExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_unary);
	ASSERT_EQ(expr1->op, UnaryOperator::sizeof_);
	ASSERT_EQ(expr1->expr, nullptr);

	auto expr1_type = std::static_pointer_cast<BuiltinTypeAST>(expr1->type);
	ASSERT_EQ(expr1_type->kind, TypeKind::builtin);
	ASSERT_EQ(expr1_type->name, BuiltinTypeName::int_);
	ASSERT_EQ(expr1_type->qualifiers.size(), 0);
	ASSERT_EQ(expr1_type->storage, StorageSpecifier::auto_);
}

TEST(ASTExtern, FunctionDefinition_SizeOfIdentifierGroupExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	sizeof(x);\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<UnaryExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_unary);
	ASSERT_EQ(expr1->op, UnaryOperator::sizeof_);
	ASSERT_EQ(expr1->expr, nullptr);

	auto expr1_type = std::static_pointer_cast<AliasTypeAST>(expr1->type);
	ASSERT_EQ(expr1_type->kind, TypeKind::alias);
	ASSERT_EQ(expr1_type->name->lexeme, "x");
}

TEST(ASTExtern, FunctionDefinition_SizeOfIdentifierExpr)
{
	auto program = parse(
		"void foo() {\n"
		"	sizeof x;\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt1 = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt1->node_type, ASTNodeType::stmt_expr);

	auto expr1 = std::static_pointer_cast<UnaryExprAST>(stmt1->expr);
	ASSERT_EQ(expr1->node_type, ASTNodeType::expr_unary);
	ASSERT_EQ(expr1->op, UnaryOperator::sizeof_);
	ASSERT_EQ(expr1->type, nullptr);

	auto identifier = std::static_pointer_cast<IdentifierExprAST>(expr1->expr);
	ASSERT_EQ(identifier->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(identifier->name->lexeme, "x");
}

TEST(ASTExtern, FunctionDefinition_OperatorPrecedence)
{
	auto program = parse(
		"void foo() {\n"
		"	x = (x > y ? x1 + y2 * z : x1 - ++y2) && (x2[0] >> y3(10) == z1->z);\n"
		"}");
	auto func = std::static_pointer_cast<FunctionDefinitionAST>(program.front());
	ASSERT_EQ(func->node_type, ASTNodeType::extern_function);
	auto stmt = std::static_pointer_cast<ExprStmtAST>(func->body->stmts.front());
	ASSERT_EQ(stmt->node_type, ASTNodeType::stmt_expr);

	auto expr = std::static_pointer_cast<BinaryExprAST>(stmt->expr);
	ASSERT_EQ(expr->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(expr->op, BinaryOperator::assignment);

	auto expr_left = std::static_pointer_cast<IdentifierExprAST>(expr->left);
	ASSERT_EQ(expr_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(expr_left->name->lexeme, "x");

	auto expr_right = std::static_pointer_cast<BinaryExprAST>(expr->right);
	ASSERT_EQ(expr_right->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(expr_right->op, BinaryOperator::and_);

	auto left = std::static_pointer_cast<TenaryExprAST>(expr_right->left);
	ASSERT_EQ(left->node_type, ASTNodeType::expr_tenary);

	auto left_cond = std::static_pointer_cast<BinaryExprAST>(left->cond);
	ASSERT_EQ(left_cond->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(left_cond->op, BinaryOperator::greater_than);

	auto left_cond_left = std::static_pointer_cast<IdentifierExprAST>(left_cond->left);
	ASSERT_EQ(left_cond_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_cond_left->name->lexeme, "x");

	auto left_cond_right = std::static_pointer_cast<IdentifierExprAST>(left_cond->right);
	ASSERT_EQ(left_cond_right->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_cond_right->name->lexeme, "y");

	auto left_left = std::static_pointer_cast<BinaryExprAST>(left->expr1);
	ASSERT_EQ(left_left->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(left_left->op, BinaryOperator::addition);

	auto left_left_left = std::static_pointer_cast<IdentifierExprAST>(left_left->left);
	ASSERT_EQ(left_left_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_left_left->name->lexeme, "x1");

	auto left_left_right = std::static_pointer_cast<BinaryExprAST>(left_left->right);
	ASSERT_EQ(left_left_right->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(left_left_right->op, BinaryOperator::multiplication);

	auto left_left_right_left = std::static_pointer_cast<IdentifierExprAST>(left_left_right->left);
	ASSERT_EQ(left_left_right_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_left_right_left->name->lexeme, "y2");

	auto left_left_right_right = std::static_pointer_cast<IdentifierExprAST>(left_left_right->right);
	ASSERT_EQ(left_left_right_right->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_left_right_right->name->lexeme, "z");

	auto left_right = std::static_pointer_cast<BinaryExprAST>(left->expr2);
	ASSERT_EQ(left_right->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(left_right->op, BinaryOperator::subtraction);

	auto left_right_left = std::static_pointer_cast<IdentifierExprAST>(left_right->left);
	ASSERT_EQ(left_right_left->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_right_left->name->lexeme, "x1");

	auto left_right_right = std::static_pointer_cast<UnaryExprAST>(left_right->right);
	ASSERT_EQ(left_right_right->node_type, ASTNodeType::expr_unary);
	ASSERT_EQ(left_right_right->op, UnaryOperator::prefix_increment);

	auto left_right_right_expr = std::static_pointer_cast<IdentifierExprAST>(left_right_right->expr);
	ASSERT_EQ(left_right_right_expr->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(left_right_right_expr->name->lexeme, "y2");

	auto right = std::static_pointer_cast<BinaryExprAST>(expr_right->right);
	ASSERT_EQ(right->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(right->op, BinaryOperator::equal);

	auto right_left = std::static_pointer_cast<BinaryExprAST>(right->left);
	ASSERT_EQ(right_left->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(right_left->op, BinaryOperator::shift_right);

	auto right_left_left = std::static_pointer_cast<BinaryExprAST>(right_left->left);
	ASSERT_EQ(right_left_left->node_type, ASTNodeType::expr_binary);
	ASSERT_EQ(right_left_left->op, BinaryOperator::array_subscript);

	auto right_left_left_object = std::static_pointer_cast<IdentifierExprAST>(right_left_left->left);
	ASSERT_EQ(right_left_left_object->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(right_left_left_object->name->lexeme, "x2");

	auto right_left_left_sub = std::static_pointer_cast<LiteralExprAST<int>>(right_left_left->right);
	ASSERT_EQ(right_left_left_sub->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(right_left_left_sub->value->value, 0);

	auto right_left_right = std::static_pointer_cast<FunctionCallExprAST>(right_left->right);
	ASSERT_EQ(right_left_right->node_type, ASTNodeType::expr_function_call);
	ASSERT_EQ(right_left_right->arguments.size(), 1);

	auto right_left_right_callee = std::static_pointer_cast<IdentifierExprAST>(right_left_right->callee);
	ASSERT_EQ(right_left_right_callee->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(right_left_right_callee->name->lexeme, "y3");

	auto right_left_right_argument1 = std::static_pointer_cast<LiteralExprAST<int>>(right_left_right->arguments[0]);
	ASSERT_EQ(right_left_right_argument1->node_type, ASTNodeType::expr_literal);
	ASSERT_EQ(right_left_right_argument1->value->value, 10);

	auto right_right = std::static_pointer_cast<MemberAccessExprAST>(right->right);
	ASSERT_EQ(right_right->node_type, ASTNodeType::expr_member_access);
	ASSERT_EQ(right_right->access_type, MemberAccessType::arrow);
	ASSERT_EQ(right_right->member->lexeme, "z");

	auto right_right_object = std::static_pointer_cast<IdentifierExprAST>(right_right->object);
	ASSERT_EQ(right_right_object->node_type, ASTNodeType::expr_identifier);
	ASSERT_EQ(right_right_object->name->lexeme, "z1");
}
