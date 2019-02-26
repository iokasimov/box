module Box.Characters.Characterize (Characterize (..)) where

import "ghc-prim" GHC.Types (Char)

class Characterize a where
	char :: a -> Char
