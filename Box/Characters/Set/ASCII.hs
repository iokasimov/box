module Box.Characters.Set.ASCII (Control (..), Mark (..), Case (..), Style (..), Position (..), Textual (..), ASCII (..)) where

import "base" Data.Char (chr)

import Box.Characters.Characterize (Characterize (char))
import Box.Characters.Alphabet (Latin (A, B, C, D, E, F, G, H, I, K, L, M, N, O, P, Q, R, S, T, V, X, Y, Z))
import Box.Characters.Numeral (Arabic)

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
