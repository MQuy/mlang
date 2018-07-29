import { Lexer, Parser, Interpreter } from "../../src";

it("class", () => {
  const source = `
  class Foo {
    var name = "mqlang";

    def hello(message: String): String {
      return this.name + ": " + message;
    }
  }

  var baz = new Foo();
  baz.name = "bar";
  var output = baz.hello("hello world");
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();
  const interpreter = new Interpreter(ast);

  interpreter.interpret();
  expect(interpreter.scope.symbols["output"]).toEqual("bar: hello world");
});

it("superclass", () => {
  const source = `
  class Foo {
    var foo = "foo";

    def hello(name: String): String {
      return this.foo + ": " + name;
    }
  }

  class Bar extends Foo {
    var bar = "bar";

    def world(time: Number): String {
      if (time > 10) {
        return this.bar;
      } else {
        return super.hello("world");
      }
    }
  }

  var baz = new Bar();
  baz.foo = "foofoo";
  var o1 = baz.world(5);
  var o2 = baz.world(20);
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();
  const interpreter = new Interpreter(ast);

  interpreter.interpret();
  expect(interpreter.scope.symbols["o1"]).toEqual("foofoo: world");
  expect(interpreter.scope.symbols["o2"]).toEqual("bar");
});
