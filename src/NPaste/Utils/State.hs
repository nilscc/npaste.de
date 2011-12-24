module NPaste.Utils.State
  ( choice
  , optional
  , (.=)
  ) where

import NPaste.Types

-- | Sensible version of `msum` that resets the state after a failed attempt
choice :: [NPaste a] -> NPaste a
choice val = do
  t <- get
  msum [ msum [ v, setNP t >> mzero ] | v <- val ]

optional :: NPaste a -> NPaste (Maybe a)
optional np = choice [ Just `fmap` np, return Nothing ]

-- | Convenient `setNP` alias
infixr 0 .=
(.=) :: ModifyNPasteState t
     => (a -> t)
     -> a
     -> NPaste ()
con .= val = setNP $ con val
