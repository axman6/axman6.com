---
title: Linear resources in Haskell
snippet: Exploring the implementation of linear types in Haskell.
author: Alex Mason
ident: 24b218cf-c395-4a06-92f8-99d7a17655ab
image: fractal_golden_ratio.jpg
image-attr: fracme / stockarch.com (http://stockarch.com/images/abstract/patterns/green-shell-lines-2119)
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


The other side of this is resource consumption. To consume a resource, we need to ensure that the given resource hasn't been consumed elsewhere, and that once it is consumed, we remove its index from the list of in scope resources.

```haskell
consume :: (Find i is ~ 'True, os ~ Remove i is)
        => Resource i a
        -> (a -> p b)
        -> Linear p (L n is) (L n os) b
consume (Res x) f = Linear (f x)
```

Here we first check that index `i` is present in the input list of resources `is`. We also remove set the output list `os` to the input list with `i` `Remove`d. Then we pass in the wrapped resource and the function in the underlying monad which will deallocate the resource.

Along similar lines, we could implement a function for using the resource while it's in scope but which doesn't deallocate it - if you wanted to ensure that resources were only ever used once, as in the original problem, then we would omit this function.

```haskell
utilise :: (Find i is ~ 'True)
        => Resource i a
        -> (a -> p b)
        -> Linear p (L n is) (L n is) b
utilise (Res x) f = Linear (f x)
```
Note that the only change is that `i` is not removed from the output, and everything else is identical to `consume`.

Finally we can provide a function to run a computation, which ensures that the resource index starts at zero, and ensures that all resources are consumed.

```haskell
runLinear :: KnownNat m => Linear p (L 0 '[]) (L m '[]) a -> p a
runLinear (Linear x) = x
```

which can be used like so

```haskell
main :: IO ()
main = runLinear $
  allocate openFile >>>= \f0 ->
  allocate openFile >>>= \f1 ->
  consume f0 closeFile >>>= \_ ->
  allocate openFile >>>= \f2 ->
  consume f2 closeFile >>>= \_ ->
  consume f1 closeFile
```

Notice that the resources are consumed in a different order from the order they were allocated. This is something that differentiates this technique from something like Golang's `defer`.

Now for the problem. Using the library works well when used like this, but it doesn't allow you to define composable actions. The problem is that GHC isn't s enough to smart enough when composing actions to realise that `Find i (Insert i is)` is always true when it doesn't know what `is` is. I had tried just cons'ing `i` onto `is` but this only helps in the situation where a resource being consumed is the most recently allocated one. For example, I can define `test1` with the type shown, but I can't define it with either of the other two, which are the types GHC tried to infer

```haskell
-- test1 :: Linear IO (L n '[]) (L (n+3) '[]) ()
-- test1 :: Linear IO (L n '[]) (L (((n+1)+1)+1) '[]) ()
test1 :: Linear IO (L 0 '[]) (L 3 '[]) ()
test1 =
  allocate openFile >>>= \f0 ->
  allocate openFile >>>= \f1 ->
  consume f0 closeFile >>>= \_ ->
  allocate openFile >>>= \f2 ->
  consume f2 closeFile >>>= \_ ->
  consume f1 closeFile
```


To solve this, I'd love to see type level Sets which allow for tests like `Find i (Add i s)` to be true for all `s`.

I'd love to hear what others have to say about this idea, and alternative methods which can be used to implement similar ideas.
