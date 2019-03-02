module Box.Characters.Decharacterize (Decharacterize (..)) where

import "pandora" Pandora.Core.Morphism (identity)
import "pandora" Pandora.Paradigm.Basis.Maybe (Maybe (Just))

import Box.Literals (Char)

class Decharacterize a where
	dechar :: Char -> Maybe a

instance Decharacterize Char where
	dechar = Just
