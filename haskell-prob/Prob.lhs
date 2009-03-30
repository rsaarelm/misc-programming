> newtype Prob = Prob Rational
>     deriving (Eq, Ord, Show, Num, Fractional)
>
> main :: IO ()
> main = do
>        putStrLn "Probability stuff!"