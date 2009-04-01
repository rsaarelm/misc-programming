-- Eric Kidd's probability monad stuff.

-- Probability type. Should always be in [0, 1], better not allow the user to
-- build unchecked instances.

import Monad
import Control.Monad.Trans
import Maybe

type Weight = Rational

newtype Prob = Prob Weight
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

-- Distributions

-- Common interface for distributions

class (Functor d, Monad d) => Dist d where
    weighted :: [(a, Weight)] -> d a

uniform :: Dist d => [a] -> d a
uniform = weighted . map (\x -> (x, 1))

-- Finite distributions

type FDist = PerhapsT ([])

-- Weighted distribution, normalizing sum of weights to 1.

instance Dist FDist where
    weighted [] = error "Empty probability distribution."
    weighted xws = PerhapsT (map weight xws)
        where weight (x, w) = Perhaps x (Prob (w / totalW))
              totalW = sum $ map snd xws

-- Make the dist expose its inner Perhaps list.

exact :: FDist a -> [Perhaps a]
exact = runPerhapsT

-- Sampling functions

-- TODO

-- Bayes' rule

-- Some helper stuff for working with dists

value (Perhaps x _) = x
prob (Perhaps _ p) = p

mapValue f (Perhaps x p) = Perhaps (f x) p

-- We're going to start dropping some elements from a list by using Maybe.
-- Make a catMaybe-equivalent for Perhaps lists of Maybe values. Drop Nothings
-- from the list and unMaybe Justs.

catPMaybes :: [Perhaps (Maybe a)] -> [Perhaps a]
catPMaybes = map (mapValue (fromMaybe undefined)) . (filter (isJust . value))

-- The same for actual dists, with normalization thrown in.

onlyJust :: FDist (Maybe a) -> FDist a
onlyJust dist
    | total > 0 = PerhapsT (map adjust filtered)
    | otherwise = PerhapsT []
    where filtered = catPMaybes (runPerhapsT dist)
          total = sum (map prob filtered)
          adjust (Perhaps x p) = Perhaps x (p / total)

-- Example: Distributions of boys and girls in families.

data Child = Boy | Girl
             deriving (Show, Eq, Ord)

child = uniform [Boy, Girl]

-- Do some list monad magic and generate all families of 2 children.

family2 = do
  child1 <- child
  child2 <- child
  return [child1, child2]

-- Let's take the old puzzle: An acquaintance says "One of my children is a
-- boy." What's the probability she has two sons?

sons = do
  kids <- family2
          -- XXX: Wrong...
  return $ if elem Boy kids then Just kids else Nothing


main :: IO ()
main = do
  print $ exact $ onlyJust sons