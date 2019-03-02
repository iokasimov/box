module Box.IO (IO, print, putc) where

import "base" System.IO (getChar, putChar)
import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))
import "pandora" Pandora.Core.Morphism ((.))
import "pandora" Pandora.Pattern.Functor.Covariant (Covariant ((<$>), void))
import "pandora" Pandora.Pattern.Functor.Pointable (Pointable (point))
import "pandora" Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import "pandora" Pandora.Pattern.Functor.Traversable (Traversable (traverse))
import "pandora" Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import "pandora" Pandora.Pattern.Functor.Monad (Monad)

import Box.Characters.Characterize (Characterize (char))
import Box.Characters.Decharacterize (Decharacterize (dechar))

instance Covariant IO where
	f <$> x = bindIO x (returnIO . f)

instance Pointable IO where
	point = returnIO

instance Applicative IO where
	x <*> y = x >>= \x' -> y >>= returnIO . x'

instance Bindable IO where
	(>>=) = bindIO

instance Monad IO where

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\s -> case m s of (# new_s, _ #) -> unIO k new_s)

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

print :: (Characterize a, Traversable t) => t a -> IO ()
print = void . traverse putc

putc :: Characterize a => a -> IO ()
putc = putChar . char
