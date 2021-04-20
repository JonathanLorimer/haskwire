# Constraints

There are 3 kinds of constraints:
  - Fully saturated typeclasses `Show a`
  - Tuples of constraints `(Applicative f, Functor f)`
  - Type equalities `(Int ~ a)` (enabled via `-XGADTs`

Type equalities are transitive and therefore `a ~ b` and `b ~ c` implies `a ~ c` to GHC

With `-XConstraintKinds` enabled we can even talk about constraints like types.

```haskell
type Codec cst1 cst2 a = (cst1 a, cst2 a)
type StringCodec a = Codec Show Read a
type JSONCodec a = Codec ToJSON FromJSON a
type SerializeCodec a = Codec Put Get a
```

We can use constraint types and associated type families to create constrained monads. This allows us to do things like write a `Monad` instance for `Set` which was previously impossible because we couldn't communicate the `Ord` constraint to `return`
> Example taken from http://blog.omega-prime.co.uk/2011/09/10/constraint-kinds-for-ghc/
```haskell
class ConstrainedMonad m where
    type MonadCtxt m a :: Constraint
    type MonadCtxt m a = ()
    return :: MonadCtxt m a => a -> m a
    (>>=) :: (MonadCtxt m a, MonadCtxt m b) => m a -> (a -> m b) -> m b

instance ConstrainedMonad Set where
  type MonadCtxt Set a = Ord a
  return = S.singleton
  mx >>= fxmy = S.fromList [y | x <- S.toList mx, y <- S.toList (fxmy x)]
```

