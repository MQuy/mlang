## Inference rules

### Type inference

#### String

```haskell
  s is a string constant
─────────────────────────
      ⊢ s: string
```

```haskell
  ⊢ s1: string ^ ⊢ s2: string
───────────────────────────────
      ⊢ s1 + s2: string
```

#### Number

```haskell
  i is an number constant
───────────────────────────
        ⊢ i: number
```

```haskell
  ⊢ e1: number ^ ⊢ e2: number
───────────────────────────────
    ⊢ e1 +-*/** e2: number
```

#### Bool / Null

```haskell
───────────────────────────────────────────────
  ⊢ true: bool | ⊢ false: bool | ⊢ null: null
```

#### Complex types

```haskell
          x is an identifier
  x is variable in scope S with type T
────────────────────────────────────────
              S ⊢ x: T
```

```haskell
  S ⊢ e: T[] ^ S ⊢ i: number
──────────────────────────────
        S ⊢ e[i]: T
```

```haskell
          S ⊢ e1: T ^ S ⊢ e2: T
───────────────────────────────────────────
  S ⊢ e1 == e2: bool | S ⊢ e1 != e2: bool
```

#### Class

```haskell
  S is in scope of class T
────────────────────────────
        S ⊢ this: T
```

```haskell
  T is a class type T
───────────────────────
      S ⊢ new T: T
```

```haskell
  S is in scope of class T
          T <= T0
────────────────────────────
        S ⊢ super: T0
```

### Type checking

```haskell
  S ⊢ expression: T
─────────────────────
  S ⊢ WF(expression)
```

#### Assgiment

```haskell
  S ⊢ e1: T1 ^ S ⊢ e2: T2
         T1 <= T2
────────────────────────────
        S ⊢ e1 = e2: T2
```

#### Control

```haskell
        S ⊢ expression: bool
    S1 is the scope inside the loop
         S1 ⊢ WF(statement)
──────────────────────────────────────
  S ⊢ WF(while(expression) statement)
```

```haskell
    S1 is the block scope inside S
         S1 ⊢ WF(statement)
──────────────────────────────────────
        S ⊢ WF({ statement })
```

### Function

```haskell
   f is an identifer in scope S
  S ⊢ f: (T1, T2, ..., Tn) -> U
    S ⊢ e1: Ai, 1 <= i <= n
          Ai <= Ti
─────────────────────────────────
    S ⊢ f(e1, e2, ..., en): U
```

```haskell
       S ⊢ f: (...) => T
      S1 ⊢ expression: T1
          T1 <= T
─────────────────────────────
  S ⊢ WF(return expression)
```

```haskell
  S ⊢ f: (...) => void
────────────────────────
     S ⊢ WF(return)
```
