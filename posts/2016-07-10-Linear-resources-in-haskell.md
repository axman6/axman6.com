---
title: Linear resources in Haskell
---

A few months ago, I was reading through [Polarised Data Parallel Data Flow](http://benl.ouroborus.net/papers/2016-polarized/dpdf-FHPC2016-sub.pdf), and noticed there were some invariants which needed to be kept in mind when using the library, mainly that streams must only be consumed once. It seemed to me that with all the power of Haskell's type system, we could do something about this.

I'd recently seen Gabriele Keller's [Bringing Down the Cost of Verification](https://www.youtube.com/watch?v=DSOOZyukILI&list=PLIpl4GKFQR6dFB0W9IzF_gcIt1VMRnuco&index=5) talk at LambdaJam '16, and thought that maybe we could use the type system to ensure that

  * Resources are only consumed once
  * all resources are consumed

It turns out you can, but making it nice to work with it harder than I'd hoped. Let's start with some imports, there's a lot because we're using some mildly advanced features in GHC.

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
```

and some imports

```haskell
import GHC.TypeLits
import GHC.Prim
import Data.Type.List
import Data.Proxy
import Control.Monad.Indexed
```

The most interesting import is `Control.Monad.Indexed` from the [indexed](https://hackage.haskell.org/package/indexed) package, which implements notion of indexed monads (and functors and comonads), which has been much more clearly explained [elsewhere](http://stackoverflow.com/a/28696299/19872) than I could have - thanks Conor!

The primary idea I wanted express in the type system was that of a counter which is incremented each time a resource is allocated, and a list of resources which are yet to be consumed. To achieve this, we'll start with the `L` type, basically a type level tuple of a `Nat` (read type level `Natural`) and a list of `Nat`s

```haskell
data L (n :: Nat) (is :: [Nat])
```

The `L` type forms the input and output of our indexed monad. The input `Nat` tells you how many says how many resources have been allocated before a particular action was invoked, and the output `Nat` minus the input tells you how many resources were allocated within an action. Similarly, the input and output `[Nat]`'s tell you which resources haven't been consumed before and after a particular action. This will make more sense once we've introduced the `Linear` monad.

`Linear` is essentially a monad transformer defined by a newtype wrapping a monad `p`, with some extra type parameters

```haskell
newtype Linear p i o a = Linear (p a)
```

Now for the indexed monad classes we'll need:

```haskell
-- ireturn is similar to return or pure, but guarantees
-- through its type that no effects which could change
-- the indices occur.
class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a

-- imap can only change the last type parameter, just like our
-- old friend fmap. Anything that changed in the indexed
-- parameters will still change after being imapped
class IxFunctor f where
	imap :: (a -> b) -> f j k a -> f j k b

-- iap is where things get more interesting - it works
-- just like ap or <*>, but also composes the effects
-- expressed in the indices.
--
-- Notice that the final indices are i and k, and the
-- two actions share j.
class IxPointed m => IxApplicative m where
	iap :: m i j (a -> b) -> m j k a -> m i k b

-- ibind is just like >>= but again composing the indices
-- as above.
class IxApplicative m => IxMonad m where
	ibind :: (a -> m j k b) -> m i j a -> m i k b
```

The implementations for `Linear` are trivial, just using non-indexed functions we're used to in everyday Haskell, with the necessary changes in `IxMonad` to work with the `newtype`.

```haskell
instance Functor p => IxFunctor (Linear p) where
  imap f (Linear x) = Linear (fmap f x)

instance Applicative p => IxPointed (Linear p) where
  ireturn x = Linear (pure x)

instance Applicative p => IxApplicative (Linear p) where
  iap (Linear pf) (Linear px) = Linear (pf <*> px)

instance Monad p => IxMonad (Linear p) where
  ibind f (Linear a) = Linear (a >>= \x -> let (Linear y) = f x in y)
```

With this sorted, we can get into the details of managing resources. First, we'll define a `newtype` which wraps a Resource, and whose type includes the index it was assigned when it was allocated.

```haskell
newtype Resource (n :: Nat) a = Res a
```

So how do we allocate a Resource? with `allocate`

```haskell
allocate :: (m ~ (n + 1)          -- (3)
            , os ~ (Insert n is)  -- (4)
            , Functor p)
         => p a                   -- (1)
         -> Linear p
                  (L n is)        -- (2)
                  (L m os)        -- (5)
                  (Resource n a)  -- (6)
allocate x = Linear $ fmap Res x
```

There's a lot going on in this type, so let's walk through it.

1. The action from the underlying monad is is used to allocate the resoure is passed in. this could be, for example `openFile` which allocate a file `Handle`
2. the `Linear` type is passed `n`, the current resource allocation count and `is`, the in scope resources.
3. Since we're allocating a new resource, we need to increment the outgoing count
4. the output has the current index `n` inserted into the inout - we've consumed nothing which was previously in scope, and now we've got one more, `n`
5. the previous two values are used in the output, to be passed to any following actions.
6. the wrapped resource is returned, marhed with its index.




```haskell
consume :: (Find q is ~ 'True, os ~ Remove q is)
        => Resource q a
        -> (a -> p b)
        -> Linear p (L n is) (L n os) b
consume (Res x) f = Linear (f x)

runLinear :: KnownNat m => Linear p (L 0 '[]) (L m '[]) a -> p a
runLinear (Linear x) = x

starting :: (KnownNat m) => Linear p (L 0 '[]) (L m os) a -> Linear p (L 0 '[]) (L m os) a
starting x = x

-- test1 :: Linear IO (L n '[]) (L (n+3) '[]) ()
-- test1 :: Linear IO (L n '[]) (L (((n+1)+1)+1) '[]) ()
-- test1 :: Linear IO (L 0 '[]) (L (3) '[]) ()
test1 =
    allocate getLine >>>= \l0 ->
    allocate getLine >>>= \l1 ->
    consume l0 print >>>= \_ ->
    allocate getLine >>>= \l2 ->
    consume l2 print >>>= \_ ->
    consume l1 print
--
-- runLinear' :: (KnownNat m, Functor p) => Linear p (L 0 '[]) (L m '[]) a -> p (a,Integer)
-- runLinear' (Linear x) = fmap (,natVal' (proxy# :: Proxy# m)) x

main :: IO ()
main = do
  runLinear $
    allocate getLine >>>= \l0 ->
    allocate getLine >>>= \l1 ->
    consume l0 print >>>= \_ ->
    allocate getLine >>>= \l2 ->
    consume l2 print >>>= \_ ->
    consume l1 print
```
