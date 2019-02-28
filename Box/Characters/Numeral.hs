module Box.Characters.Numeral (Arabic (..)) where

import Box.Characters.Characterize (Characterize (char))

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
