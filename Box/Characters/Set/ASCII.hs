module Box.Characters.Set.ASCII (Control (..), Mark (..), Case (..), Style (..), Position (..), Textual (..), ASCII (..)) where

import Box.Characters.Characterize (Characterize (char))
import Box.Characters.Alphabet (Latin (A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T, V, X, Y, Z))
import Box.Characters.Numeral (Arabic)

data Control = NULL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS | HT | LF | VT | FF | CR | SO | SI
	| DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM | SUB | ESC | FS | GS | RS | US | DEL

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

data ASCII = Unprintable Control | Printable Textual
