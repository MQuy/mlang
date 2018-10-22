import { Lexer, Parser, Interpreter } from "../../src";

it("function", () => {
  const source = `
  def hello(name: string, time: Number): String {
    var result = "";

    for(var i = 0 ; i < time ; ++i) {
      result = result + name;
    }
    return result;
  }
  var output = hello("world", 2);
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();
  const interpreter = new Interpreter(ast);

  interpreter.interpret();
  expect(interpreter.scope.symbols["output"]).toEqual("worldworld");
});

it("lambda", () => {
  const source = `
  var foo = "world";
  var hello = def (): String => {
    return foo;
  };
  var output = hello();
  `;
  const tokens = new Lexer(source).scan();
  const ast = new Parser(tokens).parse();
  const interpreter = new Interpreter(ast);

  interpreter.interpret();
  expect(interpreter.scope.symbols["output"]).toEqual("world");
});
