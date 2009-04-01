-- Eric Kidd's probability monad stuff.

-- Probability type. Should always be in [0, 1], better not allow the user to
-- build unchecked instances.

import Monad
import Control.Monad.Trans

newtype Prob = Prob Rational
    deriving (Eq, Ord, Num, Fractional)

instance Show Prob where
  show (Prob p) = show intPart ++ "." ++ show fracPart ++ "%"
      where digits = round (1000 * p)
            intPart = digits `div` 10
            fracPart = digits `mod` 10

-- Have a type that's like Maybe with probabilities added.

data Perhaps a = Perhaps a Prob
    deriving (Show)

-- Helper for avoiding evaluation of values in zero prob Perhaps values.

neverHappens :: Perhaps a -> Bool
neverHappens (Perhaps _ 0) = True
neverHappens _             = False

-- Make Perhaps into a monad.

-- Never is a special value corresponding to Maybe's Nothing.

class Monad m => MonadPerhaps m where
    perhaps :: a -> Prob -> m a
    never :: m a

instance MonadPerhaps Perhaps where
    perhaps = Perhaps
    never = Perhaps undefined 0

-- Monad helper.

instance Functor Perhaps where
    fmap f (Perhaps x p) = Perhaps (f x) p

-- This is like the Maybe monad, Nothing is sticky, but chaining yields a
-- product of the probabilities of nonzero values.

instance Monad Perhaps where
    return x = Perhaps x 1
    ph >>= f | neverHappens ph = never
             | otherwise       = Perhaps x (p1 * p2)
        where (Perhaps (Perhaps x p1) p2) = fmap f ph

-- Monad transformer for Perhaps. Scary magic stuff.

newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }

instance MonadTrans PerhapsT where
    lift x = PerhapsT (liftM return x)

instance Monad m => Functor (PerhapsT m) where
    fmap = liftM

instance Monad m => Monad (PerhapsT m) where
    return = lift . return
    m >>= f = PerhapsT bound
        where bound = do
                ph <- runPerhapsT m
                case ph of
                  (Perhaps x1 p1)  | p1 == 0   -> return never
                                   | otherwise -> do
                     (Perhaps x2 p2) <- runPerhapsT (f x1)
                     return (Perhaps x2 (p1 * p2))

-- Distribution type: Perhaps over a list

type Dist = PerhapsT ([])

-- Uniform distributions.
uniform = weighted . map (\x -> (x, 1))

-- Weighted distribution, normalizing sum of weights to 1.
weighted :: [(a, Rational)] -> Dist a
weighted [] = error "Empty probability distribution."
weighted xws = PerhapsT (map weight xws)
    where weight (x, w) = Perhaps x (Prob (w / totalW))
          totalW = sum $ map snd xws

main :: IO ()
main = do
  print $ Prob 0.2