module Box.Characters.Characterize (Characterize (..)) where

import "pandora" Pandora.Core.Morphism (identity)

import Box.Literals (Char)

class Characterize a where
	char :: a -> Char

instance Characterize Char where
	char = identity
