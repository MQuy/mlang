### The Basics

- to make long literals readable, we can use single quote as a digit separator
  ```c++
  3.14159'26535
  ```
- the order of evaluation (similar to C)
  - left-right for `x.y`, `x->y`, `x[y]`, `x<<y`, `x>>y`, `x&&y` and `x||y`
  - right-left for assignments (for example `x=y`)
  - unspecified for other expressions (for example `f(x) + g(x)`) and of function arguments (for example `h(f(x), g(x))`)
- two ways to initialize
  ```c++
  double d1 = 2.3;
  double d2 {2.3};    // = {2.3}, = is optional here
  ```
  the latter one is modern and is preferable because of strict conversion rule
  ```c++
  int i1 = 7.8;       // i1 becomes 7
  int i2 {7.8};       // error
  ```
- value of `const` can be calculated at run time, while `constexpr` must be calculated by the compiler
  ```c++
  const double s1 = sqrt(10);
  constexpr double s2 = 10;
  constexpr double s3 = s1; // error
  ```
  if function (pure) is defined with `constexpr` so it might be usable in constant expression. When calling that function with non-constant arguments, return value is not constant expression
  ```c++
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
- a _reference_ is similar to a _pointer_ except that you don't need to use a prefix `*` to access a refered value, cannot be modified after initialization and must refer to a valid object. A reference and a point both refer/point to an object and both are represented in memory as a machine address
  ```c++
  void swap(int &x, int &y) { ... };
  int main() {
    int a = 1;
    int b = 2;
    swap(a, b);       // a and b are swapped after this point since reference makes sure we use that variable itself (not copy)
  }
  ```
- if-statement can introduce a variable
  ```c++
  if (auto n = v.size()) {      // n is in scope on both branches if/else
  }
  ```
