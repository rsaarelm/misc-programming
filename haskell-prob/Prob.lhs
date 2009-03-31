<div class="slide">

Probabilistic Haskell
=====================

- Bayes' rule for mastery of the physical universe
- Use monads for bookkeeping of the computation
- Build up things from a small toolkit

</div>

<div class="slide">

> newtype Prob = Prob Rational
>     deriving (Eq, Ord, Show, Num, Fractional)
>
> main :: IO ()
> main = do
>        putStrLn "Probability stuff!"

</div>