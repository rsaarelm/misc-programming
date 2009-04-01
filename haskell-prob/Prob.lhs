<div class="slide">

Probabilistic Haskell
=====================

- Discrete distributions, Bayes' rule, Monte Carlo sampling...
- Use monads for bookkeeping of the computation
- Build up things from a small toolkit

From Eric Kidd's [Refactoring probability
distributions](http://www.randomhacks.net/articles/2007/02/21/refactoring-probability-distributions)
series and [Build your own probability
monads](http://www.randomhacks.net/darcs/probability-monads/probability-monads.pdf)
paper.

</div>

<div class="slide">

Raw probabilities
=================

> import Monad
> import Control.Monad.Trans
> import Maybe

We start with some library stuff we'll end up needing, and a simple numerical
type for basic probabilities.

> newtype Prob = Prob Rational
>     deriving (Eq, Ord, Num, Fractional)

These should always be within [0, 1], which isn't enforced here, so the user
shouldn't be allowed to feed raw `Prob` values into the probabilistic
machinery.

</div>

<div class="slide">

> instance Show Prob where
>   show (Prob p) = show intPart ++ "." ++ show fracPart ++ " %"
>       where digits = round (1000 * p)
>             intPart = digits `div` 10
>             fracPart = digits `mod` 10

A nice human-readable rounded percentage show function.

</div>

<div class="slide">

`Perhaps`: Values with a probability
====================================

Next, a type that's like `Maybe`, but with probabilities instead of binary
values.

> data Perhaps a = Perhaps a Prob
>     deriving (Show)

When `Prob` is 0, this can be thought of as `Nothing`, and the carried value
should be ignored. A function to help with this:

> neverHappens :: Perhaps a -> Bool
> neverHappens (Perhaps _ 0) = True
> neverHappens _             = False

</div>

<div class="slide">

The `Perhaps` monad
===================

`Maybe` is a monad, so let's make `Perhaps` into one as well.

Some extra interface: `perhaps` constructs the instances, and `never`
represents values with zero probability.

> class Monad m => MonadPerhaps m where
>     perhaps :: a -> Prob -> m a
>     never :: m a
>
> instance MonadPerhaps Perhaps where
>     perhaps = Perhaps
>     never = Perhaps undefined 0

</div>

<div class="slide">

Add a handy functor thing for messing with the value inside the `Perhaps`.

> instance Functor Perhaps where
>     fmap f (Perhaps x p) = Perhaps (f x) p

And now we can define the actual monad:

> instance Monad Perhaps where
>     return x = Perhaps x 1
>     ph >>= f | neverHappens ph = never
>              | otherwise       = Perhaps x (p1 * p2)
>         where (Perhaps (Perhaps x p1) p2) = fmap f ph

The monad works like the `Maybe` monad in that the event which never happens
causes the rest of the computation to also end up in the never happens value.
Otherwise the result of a sequece of `Perhaps`es has the product of their
probabilities as its probability.

</div>

<div class="slide">

Monad transformer for `Perhaps`
===============================

Pretty much going to assume this works by magic and leave it at that for now.

> newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }
>
> instance MonadTrans PerhapsT where
>     lift x = PerhapsT (liftM return x)
>
> instance Monad m => Functor (PerhapsT m) where
>     fmap = liftM
>
> instance Monad m => Monad (PerhapsT m) where
>     return = lift . return
>     m >>= f = PerhapsT bound
>         where bound = do
>                 ph <- runPerhapsT m
>                 case ph of
>                   (Perhaps x1 p1)  | p1 == 0   -> return never
>                                    | otherwise -> do
>                      (Perhaps x2 p2) <- runPerhapsT (f x1)
>                      return (Perhaps x2 (p1 * p2))
>

</div>

<div class="slide">

Distributions
=============

There might be multiple ways to reperesent a distribution. Let's define a
common interface.

> type Weight = Rational
>
> class (Functor d, Monad d) => Dist d where
>     weighted :: [(a, Weight)] -> d a
>
> uniform :: Dist d => [a] -> d a
> uniform = weighted . map (\x -> (x, 1))

Distributions are built with `weighted`. This takes a sequence of values
coupled with relative weights. The distribution is expected to normalize its
interior probabilities to sum to 1

</div>

<div class="slide">

Finite distributions
====================

A finite distribution is just a bunch of values and accompanying weights. We
already got a thing for doing those:

> type FDist = PerhapsT ([])
>
> instance Dist FDist where
>     weighted [] = error "Empty probability distribution."
>     weighted xws = PerhapsT (map weight xws)
>         where weight (x, w) = Perhaps x (Prob (w / totalW))
>               totalW = sum $ map snd xws

We also define a helper function to give us the inner list of `Perhaps`
values.

> exact :: FDist a -> [Perhaps a]
> exact = runPerhapsT

</div>

<div class="slide">

Sampling distributions
======================

Maybe we want values from a set that's too large to be exhaustively listed,
such as all the real numbers between 0 and 1? Finite distributions won't do,
but we can take random samples from a function.

More about this later. Maybe.

</div>

<div class="slide">

Some utilities
==============

> value :: Perhaps a -> a
> value (Perhaps x _) = x
>
> prob :: Perhaps a -> Prob
> prob (Perhaps _ p) = p

We're going to end up using `Maybe` to remove some values from a distribution.
To help working with there, function `onlyJust` drops all `Nothing` values
from a distribution and renormalizes it:

> catPMaybes :: [Perhaps (Maybe a)] -> [Perhaps a]
> catPMaybes = map (fmap (fromMaybe undefined)) . (filter (isJust . value))
>
> onlyJust :: FDist (Maybe a) -> FDist a
> onlyJust dist
>     | total > 0 = PerhapsT (map adjust filtered)
>     | otherwise = PerhapsT []
>     where filtered = catPMaybes (runPerhapsT dist)
>           total = sum (map prob filtered)
>           adjust (Perhaps x p) = Perhaps x (p / total)


</div>

<div class="slide">

Example 1: Fun with permutations
================================

Let's take the old puzzle: An acquaintance says "One of my children is a boy."
What's the probability she has two sons?

Produce all families with two children with a bit of `List` monad black magic:

> data Child = Boy | Girl
>              deriving (Show, Eq, Ord)
>
> child :: Dist d => d Child
> child = uniform [Boy, Girl]
>
> family :: Dist d => d [Child]
> family = do
>   child1 <- child
>   child2 <- child
>   return [child1, child2]

</div>

<div class="slide">

Now, cull the families with no sons and look up the case with two sons.

> sons :: Dist d => d (Maybe [Child])
> sons = do
>   kids <- family
>   return $ if elem Boy kids then Just kids else Nothing

    > exact family
    [Perhaps [Boy,Boy] 25.0 %,
     Perhaps [Boy,Girl] 25.0 %,
     Perhaps [Girl,Boy] 25.0 %,
     Perhaps [Girl,Girl] 25.0 %]

    > exact $ onlyJust sons
    [Perhaps [Boy,Boy] 33.3 %,     <---- 33.3 %
     Perhaps [Boy,Girl] 33.3 %,
     Perhaps [Girl,Boy] 33.3 %]

</div>

<div class="slide">

Example 2: Bayesian reasoning
=============================

A problem where people tend to have poor intuition before applying Bayesian
reasoning is trying to detect rare events with a noisy test.

             P(B|A) P(A)
    P(A|B) = -----------
                 P(B)

We're going to want probability dyads for this one.

> dyad :: (Dist d) => Rational -> a -> a -> d a
> dyad p x1 x2 = weighted [(x1, p), (x2, 1 - p)]

</div>

<div class="slide">

The Voight-Kampff test is used to expose rogue replicants, who are then
retired. The test is 99 % accurate.

It is estimated that there is one replicant hiding among every 1000 people.

> data Test = Pos | Neg
>             deriving (Show, Eq)
>
> data SubjectStatus = Human | Replicant
>                      deriving (Show, Eq)

</div>

<div class="slide">

> voightKampff :: Dist d => d (SubjectStatus, Test)
> voightKampff = do
>   subjectStatus <- dyad (1 / 1000) Replicant Human
>   testResult <-
>       if subjectStatus == Replicant
>          then dyad 0.99 Pos Neg
>          else dyad 0.01 Pos Neg
>   return (subjectStatus, testResult)

We're only interested in cases which lead to retirement of the subject.

> retirementStatistics :: Dist d => d (Maybe SubjectStatus)
> retirementStatistics = do
>   (subjectStatus, testResult) <- voightKampff
>   return (if testResult == Pos then Just subjectStatus else Nothing)

</div>

<div class="slide">

    > exact $ onlyJust retirementStatistics
    [Perhaps Replicant 9.0 %,
     Perhaps Human 91.0 %]

It is estimated that the Weyland-Yutani corporation expends over 10 000 000
nuyen annually on lawsuits resulting from mistaken retirement of humans.

</div>

<div class="slide">

Potential future stuff: Particle filters
========================================

Also known as sequetial Monte Carlo methods.

Intuitive idea: We have a robot in a building. We know the floor plan, but not
the location of the robot. Start with a cloud of particles filling the entire
floor space, each representing a possible location for the robot.

Whenever the robot moves, the entire cloud moves along with it. Any particle
that ends up in an inconsistent position like inside a wall, gets replaced
with a new, valid particle.

As the robot moves around, the cloud converges to the most probable location
of the robot.

Coming up next, hopefully.

</div>


<div class="slide">

Technical stuff
===============

- Haskell extensions needed: `GeneralizedNewtypeDeriving` `TypeSynonymInstances`

- Package dependencies: `base`, `haskell98`, `mtl`

- Source code for slides and program in Literate Haskell / Markdown

- HTML document generated using [Pandoc](http://johnmacfarlane.net/pandoc/)
  1.2 with syntax higlighting extension.

- HTML slide style using [HTML Slidy](http://www.w3.org/Talks/Tools/Slidy/)

</div>

<div class="slide">

> main :: IO ()
> main = do
>   print $ exact family
>   print $ exact $ onlyJust sons
>   print $ exact $ onlyJust retirementStatistics

</div>
