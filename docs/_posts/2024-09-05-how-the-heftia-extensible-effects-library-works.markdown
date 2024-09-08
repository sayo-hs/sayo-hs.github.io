---
layout: post
title:  "Higher-Order Effects Done Right: How the Heftia Extensible Effects Library Works"
date:   2024-09-05 03:33:30 +0900
categories: jekyll update
---

# Introduction

This article is intended for the Haskell Extensible Effects (EE) community, focusing on "higher-order effects" and "delimited continuations." It explains the workings of the EE library "Heftia" that I implemented. I hope this article reaches the EE community and generates some buzz.

[https://github.com/sayo-hs/heftia](https://github.com/sayo-hs/heftia)

# Background
As is widely known, the handling of higher-order effects in existing EE libraries is not perfect.

First, as its name suggests, [fused-effects](https://hackage.haskell.org/package/fused-effects) fuses effects. Strictly speaking, it’s not clear whether it qualifies as an EE (and it's definitely not Freer Effects since it’s not based on Freer monads). Probably, it doesn't. Fusing the effects means it offers a fundamental performance advantage, but at the cost of making dynamic effect transformations (such as `interpose`) difficult. Moreover, writing and reading interpreters is tedious (you end up surrounded by `<$ ctx`, and handling `thread` feels extremely difficult).

[polysemy](https://hackage.haskell.org/package/polysemy) almost gets it right. It’s very convenient for handling first-order effects, but higher-order effects remain challenging. Constructs like `Strategy` and `Final` can be mind-boggling, and it can't correctly handle non-deterministic computations or coroutines such as `NonDet`[^1]. This ultimately stems from the inability to manipulate delimited continuations.

[^1]: [https://github.com/polysemy-research/polysemy/issues/246](https://github.com/polysemy-research/polysemy/issues/246)

Then there's [eff](https://github.com/lexi-lambda/eff), which seems to provide *continuation-based semantics* for higher-order effects by adding a primitive operator to GHC that handles delimited continuations at the IO monad level. If this works, it might effectively surpass [effectful](https://hackage.haskell.org/package/effectful), which is currently mainstream for its simplicity and reliance on the IO monad. However, it doesn't seem to work with the current version of GHC.

There’s also [in-other-words](https://hackage.haskell.org/package/in-other-words), which, like fused-effects, likely isn’t an EE library. The problems I mentioned with fused-effects are resolved here, and the interpreter code is concise. It also handles `NonDet`, but the behavior of higher-order effects can change depending on the order of interpretation, sometimes becoming transactional and other times not. This is a carryover from the characteristics of *mtl*. If you’re looking for an emulation of *Algebraic Effects and Handlers* semantics for higher-order effects, this might be a bit tricky to handle. However, if you're just after a modernized version of *mtl*, this fits the bill.

# Higher-order Effects are a Problem

Why are things so complicated? Isn’t there a better, more elegant solution where everything just works?

Let’s set aside performance for a moment and focus on semantics. First, we need good semantics; performance comes second.

## A Hint to the Solution

There are two crucial hints here.

First, there's [freer-simple](https://hackage.haskell.org/package/freer-simple), which completely ignores higher-order effects and only supports first-order effects. By sacrificing higher-order effects, it achieves perfect semantics. Specifically, it offers *continuation-based semantics*, which equates to Algebraic Effects and Handlers—predictable, well-structured, and straightforward behavior. For more on these semantics, refer to Alexis King's [The effect semantics zoo](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md). Since delimited continuations are freely available, it can handle coroutines and non-deterministic computations with ease.

What this suggests is that higher-order effects are the real troublemakers. Without them, all we need is to combine `Free` with `Coyoneda`, and that’s it.

The second hint comes from an early idea in Polysemy's development. Below is an excerpt from a blog post written by the developer, Sandy Maguire, around March 2019:

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

After this, the post explores using a structure called `Yo`, which resembles a weaving-style `Coyoneda` in Polysemy. It seems that Polysemy’s current internal implementation either consists of this combination or something very close to it.

Now, what if we don’t adopt the weaving style and instead use this as the internal data structure for `Eff`? Would `runError` still be difficult to write?

Here lies the turning point.

**Yes, this higher-order version of Freer, defined as `Freer f a`, was indeed *right*!**

So, how should we write `runError`?

## Separating First-order and Higher-order Effects

Here, let’s introduce a novel idea:
**Separating first-order effects and higher-order effects into distinct types.**

For example, the `Error` effect can be split and defined into the following two data types:

```haskell
data Throw e (a :: Type) where
    Throw :: e -> Throw e a
```

```haskell
data Catch e f (a :: Type) where
    Catch :: f a -> (e -> f a) -> Catch e f a
```

Then, on the monad representing an effectful program, the type-level lists for higher-order effects and first-order effects will be handled separately by taking two type arguments:

```haskell
program :: Eff '[Catch Int] '[Throw Int] Int
program = catch (throw 42) \a -> pure a
```

Here, `Eff` is internally defined using the same `Freer` structure that was discussed earlier in Polysemy (details on this will be explained later).

Instead of interpreting both kinds of effects at once with something like `runError`, we split them into two interpreters: one for first-order effects and one for higher-order effects:

```haskell
runThrow :: Eff '[] (Throw e ': ef) a -> Eff '[] ef (Either e a)
runCatch :: Throw e <| ef => Eff (Catch e ': eh) ef a -> Eff eh ef a
```

Notice that in `runThrow`, the list of higher-order effects is empty. In fact, this allows both `runThrow` and **even `runCatch` to be written**[^2]. This makes it possible to handle higher-order effects.

As for `runThrow`, at least in the current method we’re discussing, **when interpreting effects that handle delimited continuations, higher-order effects cannot remain in the list**. In `runThrow`, when a `Throw` is invoked, the remaining delimited continuations must be discarded to achieve a global exit.

[^2]: In fact, the constraint `eh` must satisfy `HFunctor` (which corresponds to the old `MFunctor` in Polysemy). Although there’s much more to explain here, I’ll skip the details for now.

# Essential Structure

The takeaway from this discussion is that **the difficulties with higher-order effects in Polysemy and existing EE libraries stem from treating fundamentally different entities—first-order and higher-order effects, whose structure, nature, and possible operations are inherently different—as if they were the same.** This is the main point I want to emphasize in this article.

By separating both the types and lists of effects into first-order and higher-order categories, it becomes possible to distinguish, at the type level, which operations are valid in which situations. This stricter expression of constraints allows us to realize that operations previously thought to be unfeasible can actually be written, provided certain conditions are met.

This is how Heftia manages both first-order and higher-order effects. As mentioned earlier, there is a restriction that when dealing with delimited continuations, higher-order effects must be fully interpreted and removed from the list.

In my view, this restriction inherently emerges from the mathematical structure when algebraic effects are extended to higher-order effects, and I currently believe that it might represent the theoretical limit of such an extension at this point in time. The reason I think so is that this restriction can be interpreted as a mechanism for protecting the soundness of the semantics.

Consider the following program, which uses a higher-order effect, `onException`, to safely release resources during an error, along with a first-order effect `throw`:

```haskell
prog :: ... => Eff (OnException ': eh) (Throw Int ': ef) ()
prog =
    ( do
        allocateResource
        throw 42
        freeResourceOnSuccess
    ) `onException` freeResourceOnError
```

Now, suppose `runThrow` could handle higher-order effects still in the list, like this:

```haskell
runThrow' :: Eff eh (Throw e ': ef) a -> Eff eh ef (Either e a)
```

If this were possible, the following could happen:
```haskell
runThrow' prog :: Eff (OnException ': eh) ef (Either Int ())
```

Now, what happens to `OnException` in this case? Since `Eff` is internally just a `Freer`, there is no information about the `Throw` effect stored anywhere once it has been removed from the list. The `throw 42` bypasses `onException` and becomes an `Either Int`. At this point, there is no way to obtain information about whether an error occurred within the scope of `onException` during its interpretation. In other words, it is impossible to determine whether to call `freeResourceOnError`. If we don't call it, resources won’t be released in case of a failure, but if we do call it, both `freeResourceOnSuccess` and `freeResourceOnError` would run, causing the resource to be freed twice. This is obviously undesirable.

The restriction that higher-order effects must be fully interpreted before handling delimited continuations can be seen as a "type-level guardrail" that prevents unsafe behavior where things jump out of scope unexpectedly. If the types are structured this way, the type system ensures that before interpreting delimited continuations, all higher-order effects must be interpreted first. This order is guaranteed by the type system, allowing us to say goodbye to unpredictable, strange behavior.

This discussion leads to the fact that higher-order effects like `Resource` (used for `bracket` or `onException`) and `UnliftIO` may conflict with effects like `NonDet`, `Throw`, or `State`, which involve delimited continuation operations[^3]. In these cases, interpretations that rely on the IO monad, such as `runNonDetByThread`, `runThrowByIO`, or `runStateByIORef`, become the only feasible solutions. In other words, by following the guidance of the type system, it becomes impossible for exceptions to escape from `bracket`. By adhering to the types derived from the structure, everything remains consistent and safe. This demonstrates how well-designed Heftia’s structure and its restrictions are, and *how close it comes to the essential structure*. At least, that is my belief.

[^3]: As is familiar in libraries like Effectful, only Reader, with its favorable properties, can be interpreted without relying on IO and without conflict.

### Continuation-based Semantics

By defining various first-order and higher-order effects and interpreters in this way, the previously mentioned *continuation-based semantics* are naturally realized. For more details, please refer to the [Getting Started section of the library’s README](https://github.com/sayo-hs/heftia?tab=readme-ov-file#getting-started). In short, it achieves results similar to those in Algebraic Effects and Handlers or [Alexis King’s eff](https://github.com/lexi-lambda/eff).

## The Thought Process and Hefty Algebra

Of course, I didn’t arrive at this idea from nothing. About a year and a half ago, when I was struggling with the design of Heftia’s early version, I came across the following paper:

[Casper Bach Poulsen and Cas van der Rest. 2023. Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects. Proc. ACM Program. Lang. 7, POPL, Article 62 (January 2023), 31 pages.](https://dl.acm.org/doi/10.1145/3571255)

*It seemed like something that offered an answer to the questions I was wrestling with.* By playing around with the types while referencing the Agda definitions in the paper and the Haskell code in the artifact repository, I figured out that `Hefty` seems to be defined by the following data structure[^4]:

[^4]: Here's a gist (in Japanese) of my scattered thoughts at the time: [https://gist.github.com/ymdryo/e8af81d454371c719c289eba1418e573](https://gist.github.com/ymdryo/e8af81d454371c719c289eba1418e573)

```haskell
data Hefty h a =
      Pure a
    | forall x. Impure (h (Hefty h) x) (x -> Hefty h a)
```

It seems that this is composed of something like a higher-order version of `Coyoneda`, called `HCoyoneda`. When broken down, it looks like this:

```haskell
type Hefty h = Hefty' (HCoyoneda h)

data Hefty' h a =
      Pure a
    | Impure (h (Hefty h) (Hefty h a))

data HCoyoneda h f a
    = forall x. HCoyoneda (h f x) (x -> a)
    = HCoyoneda (Coyoneda (h f) a)
```

Additionally, `Hefty'` appears to be a higher-order version of `Free`:

```haskell
data Hefty' h a = Hefty' ( Free (h (Hefty' h)) a )
```

So, `Hefty` is essentially a combination of the higher-order versions of `Free` and `Coyoneda`, or a higher-order Freer.

Furthermore, in Hefty Algebra, a mechanism called Elaboration is used to handle higher-order effects on the Hefty data structure. This is a stronger version of the aforementioned restriction, where all higher-order effects must be interpreted before any first-order effects, regardless of whether or not delimited continuation operations are involved. In other words, the interpretation stage for higher-order effects (referred to as Elaboration) and the interpretation stage for first-order effects are completely separated.

It seemed like it might work, so I redesigned the library based on these ideas. During the process, I realized that it isn’t always necessary to fully separate the interpretation stages; by simply separating the effect lists, first-order and higher-order effects can coexist to a certain degree. So, this idea of separating the lists of first-order and higher-order effects was inspired by the Elaboration method.

And then, some time later, I came across that article on Polysemy.
Let me go over the higher-order version of `Freer` again:

> ```haskell
> newtype Freer f a = Freer
>   { runFreer
>         :: forall m
>          . Monad m
>         => (forall x. f (Freer f) x -> m x)
>         -> m a
>   }
> ```

Now let’s transform this a bit. To avoid confusion with the traditional first-order `Freer`, let’s call this higher-order version `FreerH`. We’ll rename `f` to `h`, and rewrite `FreerH` using the traditional first-order `Freer`, which we will now refer to as `Freer1`. Here, `Freer1` can be equivalently encoded as `Free (Coyoneda f)` [^5].

```haskell
data FreerH h a = FreerH (Freer1 (h (FreerH h)) a)

data Freer1 f a
    = Freer1 (forall m. Monad m => (forall x. f x -> m x) -> m a)
    = Freer1 (Free (Coyoneda f) a)
```

[^5]: [https://stackoverflow.com/questions/71562355/did-this-construction-of-freefreer-monad-works](https://stackoverflow.com/questions/71562355/did-this-construction-of-freefreer-monad-works)

If we replace the encoding of `Freer1` with `Free (Coyoneda f)` and substitute it into `FreerH`, we get:

```haskell
data FreerH h a
    = FreerH (Free (Coyoneda (h (FreerH h))) a)
    = FreerH (Free (HCoyoneda h (FreerH h)) a)
```

Breaking down `HCoyoneda` gives us:

```haskell
type FreerH h = FreerH' (HCoyoneda h)
data FreerH' h a = FreerH' ( Free (h (FreerH' h)) a )
```

Wait, isn't this the same as...?

```haskell
type Hefty h = Hefty' (HCoyoneda h)
data Hefty' h a = Hefty' ( Free (h (Hefty' h)) a )
```

They’re identical.

So, this `Hefty` equivalent had already appeared in Polysemy by 2019. In that sense, Polysemy had gotten very close. The framework necessary to fully unlock the potential of this higher-order version of `Freer` was the introduction of the Elaboration method, which separates the interpretation of first-order and higher-order effects.

Thus, although Heftia has evolved somewhat from the original paper, it is fundamentally based on the ideas presented in that paper.

## Looking Ahead

This library is not merely an attempt to preempt [Alexis King’s eff](https://github.com/lexi-lambda/eff).

If we were to categorize Haskell’s effect system libraries, they would likely be divided into two groups: non-IO-based and IO-based.

* Non-IO-based (pure):
    * [mtl](https://hackage.haskell.org/package/mtl)
    * [fused-effects](https://hackage.haskell.org/package/fused-effects)
    * [polysemy](https://hackage.haskell.org/package/polysemy)
    * [freer-simple](https://hackage.haskell.org/package/freer-simple)
* IO-based (Reader + IO):
    * [effectful](https://hackage.haskell.org/package/effectful)
    * [eff](https://github.com/lexi-lambda/eff)
    * [cleff](https://hackage.haskell.org/package/cleff)

This library falls into the non-IO-based, pure category. In other words, it allows effects to be handled without relying on the IO monad. It can use monads other than IO as the base monad, and even handle effectful programs in an applicative context, rather than a monadic one.

The reason for emphasizing purity is to align with the Safe Haskell language extension. **If we can handle effects without relying on the IO monad and avoid using any internal `unsafe` functions, we can combine this with Safe Haskell to use the effect system as a static security feature for managing permissions and access control.** Currently, the library may not yet work with Safe Haskell, but I plan to support it in the future. This emphasis on purity is a foundation for that goal.

---

This article is a translation of the original Japanese article:
<a href="https://zenn.dev/lanexpr/articles/dd8110bbdd707c">Higher-Order Effects Done Right: Heftia Extensible Effectsライブラリの仕組み</a>
