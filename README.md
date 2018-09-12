### How to setup

```
$ git clone https://github.com/MQuy/mqlang && cd mqlang
$ yarn install
```

**To run**
```
$ node ./build/repl.ts
mqlang, version 0.1.0
>
```

### Syntax

#### Class

```
class Foo extends Baz {
  var name = "mqlang";
  var counter: Number;

  def hello(name: String): String {
    if (this.counter > 10) {
      return name;
    } else {
      return this.name;
    }
  }

  def setCounter(number: Number): Number {
    return this.counter = number;
  }
}
```

#### Function & Lambda

```
def hello(name: String): String {
  return name;
}

var baz = def (counter: Number): Boolean {
  return counter > 10;
}
```

### Resources

#### Tutorial

- https://ruslanspivak.com/
- http://craftinginterpreters.com/
- https://lagunita.stanford.edu/courses/Engineering/Compilers/Fall2014/info

#### Video

- https://channel9.msdn.com/Blogs/Seth-Juarez/Anders-Hejlsberg-on-Modern-Compiler-Construction

#### Semantic Analysis

- Type checking:
  - http://www.cs.sfu.ca/~msiahban/personal/teaching/CMPT-379-Spring-2016/typecheck.pdf
  - http://web.cs.iastate.edu/~weile/cs440.540/5.SemanticAnalysis.types.pdfType
  - https://www.coursera.org/learn/programming-languages/lecture/zHyUU/what-is-type-inference

#### Parser
- https://www.youtube.com/watch?v=RDalzi7mhdY
