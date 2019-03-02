module Box.Characters.Set.ASCII (Control (..), Mark (..), Case (..), Style (..), Position (..), Textual (..), ASCII (..)) where

import "base" Data.Char (chr, ord)
import "pandora" Pandora.Core.Morphism (($))
import "pandora" Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))

import Box.Characters.Characterize (Characterize (char))
import Box.Characters.Decharacterize (Decharacterize (dechar))
import Box.Characters.Alphabet (Latin (A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T, V, X, Y, Z))
import Box.Characters.Numeral (Arabic (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9))

data Control = NULL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS | HT | LF | VT | FF | CR | SO | SI
	| DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM | SUB | ESC | FS | GS | RS | US | DEL

instance Characterize Control where
	char NULL = chr 0
	char SOH = chr 1
	char STX = chr 2
	char ETX = chr 3
	char EOT = chr 4
	char ENQ = chr 5
	char ACK = chr 6
	char BEL = chr 7
	char BS = chr 8
	char HT = chr 9
	char LF = chr 10
	char VT = chr 11
	char FF = chr 12
	char CR = chr 13
	char SO = chr 14
	char SI = chr 15
	char DLE = chr 16
	char DC1 = chr 17
	char DC2 = chr 18
	char DC3 = chr 19
	char DC4 = chr 20
	char NAK = chr 21
	char SYN = chr 22
	char ETB = chr 23
	char CAN = chr 24
	char EM = chr 25
	char SUB = chr 26
	char ESC = chr 27
	char FS = chr 28
	char GS = chr 29
	char RS = chr 30
	char US = chr 31
	char DEL = chr 127

instance Decharacterize Control where
	dechar (ord -> 0) = Just NULL
	dechar (ord -> 1) = Just SOH
	dechar (ord -> 2) = Just STX
	dechar (ord -> 3) = Just ETX
	dechar (ord -> 4) = Just EOT
	dechar (ord -> 5) = Just ENQ
	dechar (ord -> 6) = Just ACK
	dechar (ord -> 7) = Just BEL
	dechar (ord -> 8) = Just BS
	dechar (ord -> 9) = Just HT
	dechar (ord -> 10) = Just LF
	dechar (ord -> 11) = Just VT
	dechar (ord -> 12) = Just FF
	dechar (ord -> 13) = Just CR
	dechar (ord -> 14) = Just SO
	dechar (ord -> 15) = Just SI
	dechar (ord -> 16) = Just DLE
	dechar (ord -> 17) = Just DC1
	dechar (ord -> 18) = Just DC2
	dechar (ord -> 19) = Just DC3
	dechar (ord -> 20) = Just DC4
	dechar (ord -> 21) = Just NAK
	dechar (ord -> 22) = Just SYN
	dechar (ord -> 23) = Just ETB
	dechar (ord -> 24) = Just CAN
	dechar (ord -> 25) = Just EM
	dechar (ord -> 26) = Just SUB
	dechar (ord -> 27) = Just ESC
	dechar (ord -> 28) = Just FS
	dechar (ord -> 29) = Just GS
	dechar (ord -> 30) = Just RS
	dechar (ord -> 31) = Just US
	dechar (ord -> 127) = Just DEL
	dechar _ = Nothing

data Mark = Dot | Space | Exclamation | Apostrophe | Quotation | Comma | Colon | Semicolon
	| Slash | Backslash | Bar | Underscore | Ampersand | Number | Dollar | Percent
	| Asterisk | Plus | Hyphen | Equals | At | Circumflex | Grave

instance Characterize Mark where
	char Dot = '.'
	char Space = ' '
	char Exclamation = '!'
	char Apostrophe = '\''
	char Quotation = '"'
	char Comma = ','
	char Colon = ':'
	char Semicolon = ';'
	char Slash = '/'
	char Backslash = '\\'
	char Bar = '|'
	char Underscore = '_'
	char Ampersand = '&'
	char Number = '#'
	char Dollar = '$'
	char Percent = '%'
	char Asterisk = '*'
	char Plus = '+'
	char Hyphen = '-'
	char Equals = '='
	char At = '@'
	char Circumflex = '^'
	char Grave = '`'

instance Decharacterize Mark where
	dechar '.' = Just Dot
	dechar ' ' = Just Space
	dechar '!' = Just Exclamation
	dechar '\'' = Just Apostrophe
	dechar '"' = Just Quotation
	dechar ',' = Just Comma
	dechar ':' = Just Colon
	dechar ';' = Just Semicolon
	dechar '/' = Just Slash
	dechar '\\' = Just Backslash
	dechar '|' = Just Bar
	dechar '_' = Just Underscore
	dechar '&' = Just Ampersand
	dechar '#' = Just Number
	dechar '$' = Just Dollar
	dechar '%' = Just Percent
	dechar '*' = Just Asterisk
	dechar '+' = Just Plus
	dechar '-' = Just Hyphen
	dechar '=' = Just Equals
	dechar '@' = Just At
	dechar '^' = Just Circumflex
	dechar '`' = Just Grave
	dechar _ = Nothing

data Case = Upper | Lower

data Style = Round | Square | Angle | Curly

data Position = Opening | Closing

data Textual = Letter Case Latin | Digit Arabic | Bracket Position Style | Sign Mark

instance Characterize Textual where
	char (Letter Upper A) = 'A'
	char (Letter Upper B) = 'B'
	char (Letter Upper C) = 'C'
	char (Letter Upper D) = 'D'
	char (Letter Upper E) = 'E'
	char (Letter Upper F) = 'F'
	char (Letter Upper G) = 'G'
	char (Letter Upper H) = 'H'
	char (Letter Upper I) = 'I'
	char (Letter Upper K) = 'K'
	char (Letter Upper L) = 'L'
	char (Letter Upper M) = 'M'
	char (Letter Upper N) = 'N'
	char (Letter Upper O) = 'O'
	char (Letter Upper P) = 'P'
	char (Letter Upper Q) = 'Q'
	char (Letter Upper R) = 'R'
	char (Letter Upper S) = 'S'
	char (Letter Upper T) = 'T'
	char (Letter Upper V) = 'V'
	char (Letter Upper X) = 'X'
	char (Letter Upper Y) = 'Y'
	char (Letter Upper Z) = 'Z'
	char (Letter Lower A) = 'a'
	char (Letter Lower B) = 'b'
	char (Letter Lower C) = 'c'
	char (Letter Lower D) = 'd'
	char (Letter Lower E) = 'e'
	char (Letter Lower F) = 'f'
	char (Letter Lower G) = 'g'
	char (Letter Lower H) = 'h'
	char (Letter Lower I) = 'i'
	char (Letter Lower K) = 'k'
	char (Letter Lower L) = 'l'
	char (Letter Lower M) = 'm'
	char (Letter Lower N) = 'n'
	char (Letter Lower O) = 'o'
	char (Letter Lower P) = 'p'
	char (Letter Lower Q) = 'q'
	char (Letter Lower R) = 'r'
	char (Letter Lower S) = 's'
	char (Letter Lower T) = 't'
	char (Letter Lower V) = 'v'
	char (Letter Lower X) = 'x'
	char (Letter Lower Y) = 'y'
	char (Letter Lower Z) = 'z'
	char (Bracket Opening Round) = '('
	char (Bracket Opening Square) = '['
	char (Bracket Opening Angle) = '<'
	char (Bracket Opening Curly) = '{'
	char (Bracket Closing Round) = ')'
	char (Bracket Closing Square) = ']'
	char (Bracket Closing Angle) = '>'
	char (Bracket Closing Curly) = '}'
	char (Digit d) = char d
	char (Sign m) = char m

instance Decharacterize Textual where
	dechar 'A' = Just $ Letter Upper A
	dechar 'B' = Just $ Letter Upper B
	dechar 'C' = Just $ Letter Upper C
	dechar 'D' = Just $ Letter Upper D
	dechar 'E' = Just $ Letter Upper E
	dechar 'F' = Just $ Letter Upper F
	dechar 'G' = Just $ Letter Upper G
	dechar 'H' = Just $ Letter Upper H
	dechar 'I' = Just $ Letter Upper I
	dechar 'K' = Just $ Letter Upper K
	dechar 'L' = Just $ Letter Upper L
	dechar 'M' = Just $ Letter Upper M
	dechar 'N' = Just $ Letter Upper N
	dechar 'O' = Just $ Letter Upper O
	dechar 'P' = Just $ Letter Upper P
	dechar 'Q' = Just $ Letter Upper Q
	dechar 'R' = Just $ Letter Upper R
	dechar 'S' = Just $ Letter Upper S
	dechar 'T' = Just $ Letter Upper T
	dechar 'V' = Just $ Letter Upper V
	dechar 'X' = Just $ Letter Upper X
	dechar 'Y' = Just $ Letter Upper Y
	dechar 'Z' = Just $ Letter Upper Z
	dechar 'a' = Just $ Letter Lower A
	dechar 'b' = Just $ Letter Lower B
	dechar 'c' = Just $ Letter Lower C
	dechar 'd' = Just $ Letter Lower D
	dechar 'e' = Just $ Letter Lower E
	dechar 'f' = Just $ Letter Lower F
	dechar 'g' = Just $ Letter Lower G
	dechar 'h' = Just $ Letter Lower H
	dechar 'i' = Just $ Letter Lower I
	dechar 'k' = Just $ Letter Lower K
	dechar 'l' = Just $ Letter Lower L
	dechar 'm' = Just $ Letter Lower M
	dechar 'n' = Just $ Letter Lower N
	dechar 'o' = Just $ Letter Lower O
	dechar 'p' = Just $ Letter Lower P
	dechar 'q' = Just $ Letter Lower Q
	dechar 'r' = Just $ Letter Lower R
	dechar 's' = Just $ Letter Lower S
	dechar 't' = Just $ Letter Lower T
	dechar 'v' = Just $ Letter Lower V
	dechar 'x' = Just $ Letter Lower X
	dechar 'y' = Just $ Letter Lower Y
	dechar 'z' = Just $ Letter Lower Z
	dechar '(' = Just $ Bracket Opening Round
	dechar '[' = Just $ Bracket Opening Square
	dechar '<' = Just $ Bracket Opening Angle
	dechar '{' = Just $ Bracket Opening Curly
	dechar ')' = Just $ Bracket Closing Round
	dechar ']' = Just $ Bracket Closing Square
	dechar '>' = Just $ Bracket Closing Angle
	dechar '}' = Just $ Bracket Closing Curly
	dechar '0' = Just $ Digit A0
	dechar '1' = Just $ Digit A1
	dechar '2' = Just $ Digit A2
	dechar '3' = Just $ Digit A3
	dechar '4' = Just $ Digit A4
	dechar '5' = Just $ Digit A5
	dechar '6' = Just $ Digit A6
	dechar '7' = Just $ Digit A7
	dechar '8' = Just $ Digit A8
	dechar '9' = Just $ Digit A9
	dechar '.' = Just $ Sign Dot
	dechar ' ' = Just $ Sign Space
	dechar '!' = Just $ Sign Exclamation
	dechar '\'' = Just $ Sign Apostrophe
	dechar '"' = Just $ Sign Quotation
	dechar ',' = Just $ Sign Comma
	dechar ':' = Just $ Sign Colon
	dechar ';' = Just $ Sign Semicolon
	dechar '/' = Just $ Sign Slash
	dechar '\\' = Just $ Sign Backslash
	dechar '|' = Just $ Sign Bar
	dechar '_' = Just $ Sign Underscore
	dechar '&' = Just $ Sign Ampersand
	dechar '#' = Just $ Sign Number
	dechar '$' = Just $ Sign Dollar
	dechar '%' = Just $ Sign Percent
	dechar '*' = Just $ Sign Asterisk
	dechar '+' = Just $ Sign Plus
	dechar '-' = Just $ Sign Hyphen
	dechar '=' = Just $ Sign Equals
	dechar '@' = Just $ Sign At
	dechar '^' = Just $ Sign Circumflex
	dechar '`' = Just $ Sign Grave
	dechar _ = Nothing

data ASCII = Unprintable Control | Printable Textual
