import { Lexer, Parser, TypeChecking } from "../../src";
import { error } from "../helpers";

it("string to number", () => {
  const source = `var x: Number = "minh";`;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  expect(() => new TypeChecking(ast).run()).toThrow(
    error(1, 5, "String", "Number"),
  );
});

it("string", () => {
  const source = `var x: Boolean = true;`;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  new TypeChecking(ast).run();
});

it("function return wrong type", () => {
  const source = `
  def hello(x: Number): Number {
    return "world";
  }
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  expect(() => new TypeChecking(ast).run()).toThrow(
    error(3, 5, "String", "Number"),
  );
});

it("call with wrong type", () => {
  const source = `
  def hello(x: Number): Number {
    return x;
  }
  hello("world");
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  expect(() => new TypeChecking(ast).run()).toThrow(
    error(5, 3, "String", "Number"),
  );
});

it("lambda return wrong type", () => {
  const source = `var x = def(x: String): Boolean => { return 1; };`;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  expect(() => new TypeChecking(ast).run()).toThrow(
    error(1, 38, "Number", "Boolean"),
  );
});

it("inherit not same", () => {
  const source = `
  class A {}
  class B extends A {}
  class C extends A {}
  var x: B = new C();
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  expect(() => new TypeChecking(ast).run()).toThrow(error(5, 7, "C", "B"));
});

it("program", () => {
  const source = `
  class A {
    def hi(name: String): String {
      return name;
    }
  }
  class B extends A {
    def hello(name: String): String {
      return super.hi(name);
    }
  }
  var b = new B();
  b.hello("world");
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();

  new TypeChecking(ast).run();
});
