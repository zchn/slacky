-- | Simple lens functionality. Get out of here!
module Control.Lens (set, view) where

-- https://www.stackage.org/lts-6.11/package/base
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Control.Applicative (Const(Const, getConst))

set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set f a b =
    runIdentity
        (f
             (\_ ->
                   Identity a)
             b)

view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view f b = getConst (f Const b)
