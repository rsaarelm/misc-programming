<div class="slide">

Probabilistic Haskell
=====================

- Discrete distributions, Bayes' rule, Monte Carlo sampling...
- Use monads for bookkeeping of the computation
- Build up things from a small toolkit

</div>

<div class="slide">

> newtype Prob = Prob Rational
>     deriving (Eq, Ord, Show, Num, Fractional)
>

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
