# GADTs

GADTs provide the ability to refine types based on terms. By pattern matching on a constructor we can gain information about the type signature of that particular GADT.

```haskell
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool

negate :: Expr a -> a
negate (LitInt i)  = negate i -- a ~ Int
negate (LitBool b) = not b    -- a ~ Bool
```

When we construct a term with `LitInt` or `LitBool` we encode a proof about the type of the term that inhabits those constructors. This means that when we pattern match on those constructors we can use that proof to recover information about the type. This is witnessed by the function `negate` which can effectively discharge a type equality constraint in each branch

Therefore GADTs can be used to constrain type parameters. GADTs are really just syntactic sugar around type equalities. `Expr` can be re-written.

```haskell
data Expr a
  = (a ~ Int)  => LitInt Int
  | (a ~ Bool) => LitBool Bool
```
