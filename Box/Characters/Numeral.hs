module Box.Characters.Numeral (Arabic (..)) where

import "pandora" Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))

import Box.Characters.Characterize (Characterize (char))
import Box.Characters.Decharacterize (Decharacterize (dechar))

data Arabic = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9

instance Characterize Arabic where
	char A0 = '0'
	char A1 = '1'
	char A2 = '2'
	char A3 = '3'
	char A4 = '4'
	char A5 = '5'
	char A6 = '6'
	char A7 = '7'
	char A8 = '8'
	char A9 = '9'

instance Decharacterize Arabic where
	dechar '0' = Just A0
	dechar '1' = Just A1
	dechar '2' = Just A2
	dechar '3' = Just A3
	dechar '4' = Just A4
	dechar '5' = Just A5
	dechar '6' = Just A6
	dechar '7' = Just A7
	dechar '8' = Just A8
	dechar '9' = Just A9
	dechar _ = Nothing
