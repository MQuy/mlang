### Inference rules

#### String

```haskell
s is a string constant  
──────────────────────
     ⊢ s: string
```

```haskell
⊢ s1: string ^ ⊢ s2: string
───────────────────────────
     ⊢ s1 + s2: string
```

#### Number

```haskell
i is an number constant  
───────────────────────
      ⊢ i: number
```

```haskell
⊢ e1: number ^ ⊢ e2: number
───────────────────────────
  ⊢ e1 +-*/** e2: number
```

#### Primitive types

```haskell
───────────────────────────────────────────
⊢ true: bool | ⊢ false: bool | ⊢ null: null
```

#### Complex types

```haskell
          x is an identifier
  x is variable in scope S with type T
───────────────────────────────────────
              S ⊢ x: T
```

```haskell
        S ⊢ e1: T ^ S ⊢ e2: T
───────────────────────────────────────
S ⊢ e1 == e2: bool | S ⊢ e1 != e2: bool
```

```haskell
  S ⊢ e: T[] ^ S ⊢ i: number
──────────────────────────────
        S ⊢ e[i]: T
```

```haskell
  S ⊢ e1: T1 ^ S ⊢ e2: T2
         T1 <= T2
────────────────────────────
        S ⊢ e1 = e2: T2
```

### Type checking rules
