module NumWithBase
    (
    -- * Convertion from Integral
      toBase
    , fromBase
    , NumWithBase (..)
    ) where

import qualified Data.Maybe as M
import qualified Data.List  as L

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------

data NumWithBase a = NumWithBase a [a]
  deriving (Show, Eq, Ord)

-- Apply a function to a list, starting from the last elements of both lists
sumL :: Integral a => [a] -> [a] -> [a]
sumL [] l  = l
sumL l  [] = l
sumL (l1:r1) (l2:r2) = (l1 + l2) : sumL r1 r2

-- normalize a number with its base
normalize :: Integral a => a -> [a] -> [a]
normalize _ [] = []
normalize n (x:xs)
    | x > n     = let (next,rest) = case xs of [] -> (0, []); (x':r) -> (x', r)
                  in normalize n (x `mod` n : next + x `div` n : rest)
    | otherwise = x : normalize n xs

-- Convert a number to NumWithBase
toBase :: Integral a => a -> a -> NumWithBase a
toBase n i 
    | n <  0 = negate $ NumWithBase n (normalize (-n) [i])
    | n >= 0 = NumWithBase n (normalize n [i])

-- Convert a NumWithBase back to a "normal" number
fromBase :: Integral a => NumWithBase a -> a
fromBase (NumWithBase n [])     = error "Empty list"
fromBase (NumWithBase n (x:[])) = x
fromBase (NumWithBase n (x:xs)) =
    x + n * fromBase (NumWithBase n xs)

--------------------------------------------------------------------------------
-- Num instance
--------------------------------------------------------------------------------

-- Add two NumWithBase
(+++) :: Integral a => NumWithBase a -> NumWithBase a -> NumWithBase a
(NumWithBase n1 l1) +++ (NumWithBase n2 l2)
    | n1 /= n2  = error "Wrong base"
    | otherwise = NumWithBase n1 . normalize n1 $ sumL l1 l2

negateNWB :: Integral a => NumWithBase a -> NumWithBase a
negateNWB (NumWithBase b l) = NumWithBase b $ map negate l

absNWB :: Integral a => NumWithBase a -> NumWithBase a
absNWB n@(NumWithBase b _) = toBase b . abs $ fromBase n

instance Integral a => Num (NumWithBase a) where
    (+)         = (+++)
    abs         = absNWB
    negate      = negateNWB
    fromInteger = toBase 10 . fromInteger
    n@(NumWithBase b1 l1) * m@(NumWithBase b2 l2)
        | b1 /= b2  = error "Wrong base"
        | otherwise = toBase b1 $ fromBase n * fromBase m
    signum n@(NumWithBase b _) = toBase b . signum $ fromBase n
