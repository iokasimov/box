module Box.Characters.ASCII (Control (..), Mark (..), Case (..), Style (..), Position (..), Textual (..), ASCII (..)) where

import Box.Characters.Alphabet (Latin)
import Box.Characters.Numeral (Arabic)

data Control = NULL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS | HT | LF | VT | FF | CR | SO | SI
	| DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM | SUB | ESC | FS | GS | RS | US | DEL

data Mark = Dot | Space | Exclamation | Apostrophe | Quotation | Comma | Colon | Semicolon
	| Slash | Backslash | Bar | Underscore | Ampersand | Number | Dollar | Percent
	| Asterisk | Plus | Hyphen | Equals | At | Circumflex | Grave

data Case = Upper | Lower

data Style = Round | Square | Angle | Curly

data Position = Opening | Closing

data Textual = Letter Case Latin | Digit Arabic | Bracket Position Style | Sign Mark

data ASCII = Unprintable Control | Printable Textual
