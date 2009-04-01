<div class="slide">

Probabilistic Haskell
=====================

- Discrete distributions, Bayes' rule, Monte Carlo sampling...
- Use monads for bookkeeping of the computation
- Build up things from a small toolkit

</div>

<div class="slide">

Basic types
===========

From Eric Kidd's [Refactoring probability
distributions](http://www.randomhacks.net/articles/2007/02/21/refactoring-probability-distributions)
series and [Build your own probability
monads](http://www.randomhacks.net/darcs/probability-monads/probability-monads.pdf)
paper.

A probability type with a nice human-readable rounded percentage show
function.

> newtype Prob = Prob Rational
>     deriving (Eq, Ord,  Num, Fractional)
>
> instance Show Prob where
>   show (Prob p) = show intPart ++ "." ++ show fracPart ++ "%"
>       where digits = round (1000 * p)
>             intPart = digits `div` 10
>             fracPart = digits `mod` 10

</div>

<div class="slide">

Start building the `Perhaps` type, like `Maybe` but with probabilities.

> data Perhaps a = Perhaps a Prob
>   deriving (Show)

When the probability is 0, it's equivaelnt to `NONE`. We can use `undefined`
for the value then:

...

</div>

<div class="slide">

Particle filters
================

Also known as sequetial Monte Carlo methods.

Intuitive idea: We have a robot in a building. We know the floor plan, but not
the location of the robot. Start with a cloud of particles filling the entire
floor space, each representing a possible location for the robot.

Whenever the robot moves, the entire cloud moves along with it. Any particle
that ends up in an inconsistent position like inside a wall, gets replaced
with a new, valid particle.

As the robot moves around, the cloud converges to the most probable location
of the robot.

</div>

<div class="slide">

Demo time!
==========

> main :: IO ()
> main = do
>        putStrLn "Probability stuff!"
>        putStrLn "TODO: Some actual stuff..."

</div>
