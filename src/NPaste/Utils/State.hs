module NPaste.Utils.State
  ( optional
  , (.=)
  ) where

import NPaste.Types

optional :: NPaste a -> NPaste (Maybe a)
optional np = msum [ Just `fmap` np, return Nothing ]

-- | Convenient `setNP` alias
infixr 0 .=
(.=) :: ModifyNPasteState t
     => (a -> t)
     -> a
     -> NPaste ()
con .= val = setNP $ con val
