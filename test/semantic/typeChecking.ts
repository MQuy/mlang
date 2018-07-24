import { Lexer, Parser, TypeChecking } from "../../src";

it("var statement", () => {
  const source = `
  class A {}
  class B extends A {}
  class C extends A {}
  var x: B = new C();
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  new TypeChecking(ast).run();
});
