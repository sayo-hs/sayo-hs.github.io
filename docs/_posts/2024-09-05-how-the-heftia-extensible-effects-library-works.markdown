---
layout: post
title:  "Higher-Order Effects Done Right: How the Heftia Extensible Effects Library Works"
date:   2024-09-05 03:33:30 +0900
categories: jekyll update
---

This article is a proofread version of a machine translation of the original Japanese article:
<a href="https://zenn.dev/lanexpr/articles/dd8110bbdd707c">Higher-Order Effects Done Right: Heftia Extensible Effectsライブラリの仕組み</a>

---

# Introduction

This article is intended for the Haskell Extensible Effects (EE) community, with a particular focus on "higher-order effects" and "delimited continuations." It explains the workings of the EE library "Heftia," which I implemented. I hope this article reaches the EE community and generates some buzz.

[https://github.com/sayo-hs/heftia](https://github.com/sayo-hs/heftia)

# Background

As is well-known, the handling of higher-order effects in existing EE libraries is far from perfect.

First, [fused-effects](https://hackage.haskell.org/package/fused-effects) fuses effects, as its name suggests. Strictly speaking, it’s not clear if this can even be called EE (at the very least, it’s not Freer-based, so I don’t think it’s Freer Effects). It probably can’t. The fusion of effects means that there is a fundamental performance advantage, but the trade-off is that dynamic transformation of effects (so-called `interpose` operations) becomes difficult. Also, managing interpreters is a hassle (there’s a lot of `<$ ctx`, and dealing with `thread` feels quite challenging).

[polysemy](https://hackage.haskell.org/package/polysemy) gets close but still falls short. First-order effects are handled easily and conveniently. However, higher-order effects remain difficult. Concepts like `Strategy` and `Final` can be quite mind-boggling. Additionally, it cannot correctly handle nondeterministic computations like `NonDet` or coroutines[^1]. This stems from the inability to manage delimited continuations.

[^1]: https://github.com/polysemy-research/polysemy/issues/246

This is where [eff](https://github.com/lexi-lambda/eff) comes in. By adding primitive operators for managing delimited continuations at the IO Monad level to GHC, it aims to provide a *continuation-based semantics* for higher-order effects. If this starts working, it may become a de facto superior alternative to [effectful](https://hackage.haskell.org/package/effectful), which is currently popular due to its simplicity on the IO Monad base. However, it seems that this does not work on the current GHC.

There's also [in-other-words](https://github.com/KingoftheHomeless/in-other-words). Like *fused-effects*, it's probably not an EE. The issues I mentioned with *fused-effects* have been resolved. The interpreter code is concise, and it seems to handle *NonDet* as well. However, the behavior involving higher-order effects can become transactional or not depending on the order of interpretation, which is a characteristic carried over from *mtl*. If you expect higher-order effects to emulate the semantics of *Algebraic Effects and Handlers*, this might be a quite tricky to deal with. It would likely meet the demand for a modernized version of *mtl*.

# The Problem with Higher-Order Effects

Why is this such a complicated issue? Is there a simple, elegant solution that makes everything work?

First, let’s forget about performance and focus on semantics. Good semantics first, performance second.

## A Hint Towards the Answer

Here are two important hints.

The first is [freer-simple](https://hackage.haskell.org/package/freer-simple). This library completely ignores higher-order effects and only supports first-order effects. In exchange for the inability to use higher-order effects, it provides perfect semantics. Specifically, it achieves predictable, well-structured behavior through *continuation-based semantics*, which is essentially equivalent to Algebraic Effects and Handlers. For more details on these semantics, check Alexis King’s [The effect semantics zoo](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md). With delimited continuations at your disposal, coroutines and nondeterministic computations are a breeze.

What we learn here is that higher-order effects are the troublemakers. If higher-order effects didn’t exist, everything would simply be solved by composing `Free` with `Coyoneda`.

The second hint comes from an early idea in Polysemy’s development. Below is an excerpt from developer Sandy Maguire’s blog, written around March 2019:

> ...
> ```haskell
> newtype Freer f a = Freer
>   { runFreer
>         :: forall m
>          . Monad m
>         => (forall x. f (Freer f) x -> m x)
>         -> m a
>   }
> ```
> I have no idea if this is *right*, but at least it gives a `Monad` instance for free. One limitation you’ll notice is that the continuation in `runFreer` is a natural transformation, and thus it’s unable to change its return type.
>
> That means interpretations like `runError :: Eff (Error e ': r) a -> Eff r (Either e a)` are surprisingly difficult to implement. More on this later—I just wanted to point out this flaw.
>
> ...

From [Freer, yet Too Costly Higher-order Effects - Freeing the Higher Orders - REASONABLY POLYMORPHIC](https://reasonablypolymorphic.com/blog/freer-yet-too-costly/index.html#freeing-the-higher-orders).

Afterward, the blog discusses attempts to compose a structure called `Yo`, a `Coyoneda`-like structure for weaving in Polysemy. The current internal implementation of Polysemy is likely based on this or something very close to it.

So, what if we don’t use a weaving method and simply compose `Coyoneda`? Will we still be able to write `runError`?

Here lies the crux of the matter.

**Yes, this higher-order version of Freer, defined as `Freer f a`, was indeed *right*!**

But how should we write `runError`?

## Separating First-Order and Higher-Order Effects

Now, let me introduce a novel idea:
**separating first-order effects from higher-order effects at the type level**.

For example, the `Error` effect is split into two data types:

```haskell
data Throw e (a :: Type) where
    Throw :: e -> Throw e a
```

```haskell
data Catch e f (a :: Type) where
    Catch :: f a -> (e -> f a) -> Catch e f a
```

Then, in the monad representing effectful programs, we pass the list of higher-order effects and the list of first-order effects as two separate type arguments:

```haskell
program :: Eff '[Catch Int] '[Throw Int] Int
program = catch (throw 42) \a -> pure a
```

Internally, `Eff` is defined as a higher-order version of Polysemy's `Freer`, composed with `Coyoneda`. I’ll explain this later.

Instead of interpreting both effects at once, as with something like `runError`, we split the interpreters into one for first-order effects and one for higher-order effects:

```haskell
runThrow :: Eff '[] (Throw e ': ef) a -> Eff '[] ef a
runCatch :: Eff (Catch e ': eh) ef a -> Eff eh ef a
```

Note that in `runThrow`, the list of higher-order effects is empty. The key point is that **when interpreting delimited continuations, higher-order effects must already be interpreted and removed from the list**. In `runThrow`, when a `Throw` is raised, we need to discard the continuation and globally exit.

-- TODO: runCatchの話が抜けてる

[^2]: In fact, you need an `HFunctor` constraint (similar to `MFunctor` in older versions of Polysemy) for `eh`. There’s a lot more I could explain about this, but I’ll omit that for now.

# The Essential Structure

The takeaway is this: **The root cause of the difficulties surrounding higher-order effects in Polysemy and existing EE implementations is that they mix and treat fundamentally different entities—first-order effects and higher-order effects—with different structures and capabilities**. This is the key point I want to emphasize in this article.

By separating the effect types and lists into first-order and higher-order at the type level, we can more accurately represent the constraints of what is and isn’t possible in different scenarios. This leads to situations where operations previously thought impossible can be written, provided they are conditionally separated.

This is the method Heftia uses to handle both first-order and higher-order effects. As mentioned earlier, this comes with the restriction that when handling delimited continuations or global exits, all higher-order effects must have been interpreted and removed from the list.

I believe this restriction reflects an inherent mathematical structure that appears when extending algebraic effects to higher-order effects—a fundamental limitation of the extension. The reasoning is that this restriction serves to protect the correctness of the semantics.

For example, consider the following program using `throw` for a first-order effect and a higher-order effect like `finally` for resource management:
```haskell
prog :: ... => Eff (Finally ': eh) (Throw Int ': ef) ()
prog =
    ( do
        allocateResource
        throw 42
    ) `finally` freeResource
```

Now, if we assume `runThrow` could work with higher-order effects still remaining in the list, we could imagine a hypothetical `runThrow'`:
```haskell
runThrow' :: Eff eh (Throw e ': ef) a -> Eff eh ef a
```

This would allow us to run:
```haskell
runThrow' prog :: Eff (Finally ': eh) ef (Either Int ())
```

But what happens next with `Finally`? Since `Eff` is internally just `Freer`, we no longer have any information about `Throw`, which has already disappeared from the list. The `throw 42` bypasses `finally` and directly results in `Either Int`. At this point, the `Finally` interpreter would no longer be able to determine whether an error occurred within its scope.

The restriction on handling higher-order effects when interpreting delimited continuations acts as a type-level "guardrail" to prevent such unsafety. The types ensure that higher-order effects must be interpreted before any delimited continuations are handled, enforcing the correct order and preventing unpredictable behavior.

This also ties into the idea that higher-order effects like `Resource` (i.e., `bracket` and `finally`) and `UnliftIO` conflict with continuation-based effects like `NonDet`, `Throw`, and `State`[^3]. In such cases, only interpretations dependent on `IO` (e.g., `runNonDetByThread`, `runThrowByIO`, or `runStateByIORef`) are possible. This demonstrates the structural soundness of the restrictions Heftia imposes, reinforcing its fundamental alignment with essential structures.

[^3]: -- TODO

### Continuation-based Semantics
When defining interpreters and effects in this way, *continuation-based semantics* are naturally realized. For details, refer to the [Getting Started](https://github.com/sayo-hs/heftia?tab=readme-ov-file#getting-started) section of the library’s README. In short, the result is equivalent to Algebraic Effects and Handlers or [Alexis King’s eff](https://github.com/lexi-lambda/eff).

## The Actual Path, and Hefty Algebra

Of course, I didn’t arrive at these ideas from scratch. About a year and a half ago, while I was struggling with the design of the early version of Heftia, I came across the following paper:

[Casper Bach Poulsen and Cas van der Rest. 2023. Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects. Proc. ACM Program. Lang. 7, POPL, Article 62 (January 2023), 31 pages.](https://dl.acm.org/doi/10.1145/3571255)

It seemed to offer some answers to my questions, so I tinkered with the types based on the Agda definitions and Haskell code from the artifact repository. Eventually, I discovered that `Hefty` is the following data structure[^4]:

[^4]: A scattered note I wrote at the time in Japanese: https://gist.github.com/ymdryo/e8af81d454371c719c289eba1418e573

```haskell
data Hefty h a =
      Pure a
    | forall x. Impure (h (Hefty h) x) (x -> Hefty h a)
```

This resembles `HCoyoneda`, a higher-order version of `Coyoneda`. When decomposed, `HCoyoneda` looks like this:

```haskell
type Hefty h = Hefty' (HCoyoneda h)

data Hefty' h a =
      Pure a
    | Impure (h (Hefty h) (Hefty h a))

data HCoyoneda h f a
    = forall x. HCoyoneda (h f x) (x -> a)
    = HCoyoneda (Coyoneda (h f) a)
```

Further decomposing `Hefty'` reveals that it’s essentially a higher-order version of `Free`:
```haskell
data Hefty' h a = Hefty' (Free (h (Hefty' h)) a)
```

Thus, `Hefty` is a combination of the higher-order versions of `Free` and `Coyoneda`, making it a higher-order Freer.

In Hefty Algebra, an elaboration mechanism is used to handle higher-order effects on the Hefty data structure. This mechanism imposes an even stricter constraint than what I mentioned earlier: regardless of whether delimited continuations are involved, all higher-order effects must be interpreted before any first-order effects are interpreted. In other words, the elaboration stage for higher-order effects is completely separated from the interpretation stage for first-order effects.

It felt like I was onto something, so I redesigned the library based on these ideas. Eventually, I realized that by separating the effect lists, higher-order and first-order effects could coexist without fully separating interpretation stages. Thus, the idea of separating the lists of first-order and higher-order effects was inspired by the elaboration approach.

-- TODO: ここ怪しい？
A little later, I came across that Polysemy article. Let’s revisit the definition of the higher-order version of Freer:
```haskell
newtype Freer f a = Freer
  { runFreer
        :: forall m
         . Monad m
        => (forall x. f (Freer f) x -> m x)
        -> m a
  }
```

Let’s rework this. To avoid confusion with the traditional first-order `Freer`, I’ll rename this higher-order version to `FreerH` and rename `f` to `h`. Now, let’s rewrite `FreerH` using the traditional first-order `Freer1`. We can express `Freer1` as `Free (Coyoneda f)`[^5]:
```haskell
data FreerH h a = FreerH (Freer1 (h (FreerH h)) a)

data Freer1 f a
    = Freer1 (forall m. Monad m => (forall x. f x -> m x) -> m a)
    = Freer1 (Free (Coyoneda f) a)
```

[^5]: https://stackoverflow.com/questions/71562355/did-this-construction-of-freefreer-monad-works

Substituting `Freer1` with `Free (Coyoneda f)` into `FreerH` gives:
```haskell
data FreerH h a
    = FreerH (Free (Coyoneda (h (FreerH h))) a)
    = FreerH (Free (HCoyoneda h (FreerH h)) a)
```

Decomposing `HCoyoneda` results in:

```haskell
type FreerH h = FreerH' (HCoyoneda h)
data FreerH' h a = FreerH' (Free (h (FreerH' h)) a)
```

Wait... doesn’t this look like:

```haskell
type Hefty h = Hefty' (HCoyoneda h)
data Hefty' h a = Hefty' (Free (h (Hefty' h)) a)
```

They’re the same!

So, the equivalent of `Hefty` already existed in Polysemy as early as 2019. In a way, Polysemy had come very close. To fully unlock the potential of this higher-order Freer, what was needed was the elaboration framework, where higher-order effects are interpreted separately from first-order effects.

In summary, although Heftia has deviated somewhat from the original paper, it is fundamentally based on these ideas.

# Future Directions

This library is not just an early version of [Alexis King’s eff](https://github.com/lexi-lambda/eff), made functional. -- TODO: functionalの表現変えたい

If we were to classify Haskell’s effect system libraries, we could divide them into non-IO-based (pure) and IO-based categories:
* Non-IO-based (pure)
    * [mtl](https://hackage.haskell.org/package/mtl)
    * [fused-effects](https://hackage.haskell.org/package/fused-effects)
    * [polysemy](https://hackage.haskell.org/package/polysemy)
    * [freer-simple](https://hackage.haskell.org/package/freer-simple)
* IO-based (Reader + IO)
    * [effectful](https://hackage.haskell.org/package/effectful)
    * [eff](https://github.com/lexi-lambda/eff)
    * [cleff](https://hackage.haskell.org/package/cleff)

This library belongs to the non-IO-based, pure side. This means that effects can operate without depending on the IO Monad. You can use a Monad other than IO as the base, or even handle applicative effectful programs that don’t require a monad.

The reason I emphasize purity is due to a consideration of the Safe Haskell language extension. **If you don’t depend on the IO Monad and avoid using any `unsafe` functions internally, you can combine it with Safe Haskell to use the effect system as a static capability management and access control security feature**. While it probably doesn’t work with Safe Haskell at the moment, I plan to support it in the future. The emphasis on purity is laying the groundwork for this.

