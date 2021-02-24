### The Basics

- to make long literals readable, we can use single quote as a digit separator
  ```cpp
  3.14159'26535
  ```
- the order of evaluation (similar to C)
  - left-right for `x.y`, `x->y`, `x[y]`, `x<<y`, `x>>y`, `x&&y` and `x||y`
  - right-left for assignments (for example `x=y`)
  - unspecified for other expressions (for example `f(x) + g(x)`) and of function arguments (for example `h(f(x), g(x))`)
- two ways to initialize
  ```cpp
  double d1 = 2.3;
  double d2 {2.3};    // = {2.3}, = is optional here
  ```
  the latter one is modern and is preferable because of strict conversion rule
  ```cpp
  int i1 = 7.8;       // i1 becomes 7
  int i2 {7.8};       // error
  ```
- value of `const` can be calculated at run time, while `constexpr` must be calculated by the compiler
  ```cpp
  const double s1 = sqrt(10);
  constexpr double s2 = 10;
  constexpr double s3 = s1; // error
  ```
  if function (pure) is defined with `constexpr` so it might be usable in constant expression. When calling that function with non-constant arguments, return value is not constant expression
  ```cpp
  constexpr double square(double x) { return x * x;}
  ----
  constexpr double max1 = 1.4 * square(17);       // ok
  constexpr double max2 = 1.4 * square(var);      // error: var is not constant expression
  const double max3 = 1.4 * square(var);          // ok, is evaluated at runtime
  ```
  |     | default linkage (static) | initialization | use                           |
  | --- | ------------------------ | -------------- | ----------------------------- |
  | C   | external                 | optional       | restrict                      |
  | C++ | internal                 | mandatory      | same as preprocessor constant |
- a _reference_ (_lvalue reference_ which binds to [_lvalue_](https://ncona.com/2019/11/cpp-value-categories/)) is similar to a _pointer_ except that we don't need to use a prefix `*` to access a refered value, cannot be modified after initialization and must refer to a valid object. A reference and a point both refer/point to an object and both are represented in memory as a machine address
  ```cpp
  void swap(int &x, int &y) { };
  void foo(const int&x) { };
  int main() {
    int a = 1;
    int b = 2;
    swap(a, b);   // a and b are swapped after this point since reference makes sure we use that variable itself (not copy)
    foo(1);       // is allowed only if that parameter has const qualifier, compiler creates a temporary variable, assigns 7 to it and passes to that foo
  }
  ```
- if-statement can introduce a variable
  ```cpp
  if (auto n = v.size()) {      // n is in scope on both branches if/else
  }
  ```

### User-Defined Types

- use `.` (dot) to access member through name and reference and `->` through a pointer
  ```cpp
  void f(Vector v, Vector& rv, Vector* pv) {
    v.xxx;             // access through name
    rv.xxx;            // access through reference
    pv->xxx;           // access through pointer
  }
  ```
- no fundamental difference between _struct_ (simply _class_ with members public by default) and _class_
- overall, prefer `variant` over `union`. one of `union` usage is type-punning (interpret type in a different way like [socket](https://en.wikipedia.org/wiki/Type_punning#Sockets_example))
- `enum` can be defined with `class/struct` to make it scoped and cannot implicity mix with integer values (how enumerator value works is the same as C)
  ```cpp
  enum class Color { Red, Blue, Green };
  Color c1 = Color::Red;        // ok
  int c2 = Color::Red;          // error
  Color c3 = 2;                 // error
  Color c4 = Color {1};         // explicit, undefined behaviour if value is out of enum ranges
  Color c5 {1};                 // same above
  ```

### Modularity

- similar to C (a `.c` file), a `.cpp` file is called a _translation unit_
- from c++20 (not ready yet), prefer using `module` over `#include` (old-fashioned preprocessor text replacement). Benefits of modules
  - a module is compile only once
  - two modules can be imported in any order without changing the meaning
  - importing something into a module, users of that module doesn't implicitly gain access to what we imported
- `noexcept` promises that function should never throw an exception (if not, program is terminted)
- class invariant constraints the state stored in the object (established in construction and constantly maintain between calls via public methods)
- avoid overuse of `try/catch` (usually design error-handing strategy beforehand) by using RAII technique
- put a bit care when choosing an suitable approach either throwing an exception, returning a error code or terminating the program to indicate is wrong
- `assert` for runtime error checking while `static_assert` for compile error checking
- default behaviour for both argument passing and value return is "copy" (can be implicitly optimized to "moves" or "shared")
- structured binding is mechanism for giving local names to members of a class/struct object
  ```cpp
  map<string, int> m;
  for(auto & [key, value] : m)
    ++value;
  ```

### Classes

- the idea of concrete classes is behaving like built-in types, its representation is part of its definition <- allows implementation to optitmize efficient in time and space
- compiler converts operators into appropriate function calls (which can be inlined)
  ```cpp
  complex a {2.3};
  complex b {1 / a}; // -> operator/(complex{1}, a)
  a + b;              // -> operator+(a, b)
  ```
- prefer _Resource Acquisition Is Initialization (RAII)_ (acquiring resources in a constructor and releasing them in a destructor) over "naked `new/delete` operator" since releasing resources is automatic when execution is out of its scope
  ```cpp
  void foo(int n) {
    Vector v(n);
  } // v is destroyed here
  ```
- for classes in class hierarchies, we tend to allocate them via `new` and then deallocate via `delete`. There are few reasons to avoid them like forget or fail to `delete`, in that case, prefer using `unique_ptr`
  ```cpp
  void foo() {
    vector<unique_ptr<Shape>> v;
  } // all Shapes implicitly destroyed
  ```
- [virtual method table](https://en.wikipedia.org/wiki/Virtual_method_table#Example) is used to support dynamic dispatch (override virtual method calls). In short, each derived object has a hidden pointer (object's member) which points to array of pointers (class's vpointer), each pointer points to a class function address
  ![virtual method table](https://i.imgur.com/v3RgnSL.jpg)

### Essential Operations

- Constructor, destructor, copy and move operators will be generated as needed (though we can specify `=default` or `=delete` to force)
- To prevent implicit conversion, we add prefix to constructor like below
  ```cpp
  class Vector {
  public:
    explicit Vector(int s);    // no implicit conversion from int to Vector
  };
  ----
  Vector v1(7);               // ok
  Vector v2 = 7;              // error
  ```
- move is the concept of "transfer" content of an object which is rvalue to a target and "empty" that object
- move operator is applied when an rvalue reference is used as an initializer or as the right-hand side of an assignment
- user-defined literal is the way to construct user-defined object from literal
