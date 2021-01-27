{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

-- base
import Data.Coerce
  ( coerce )
import Data.Functor.Compose
  ( Compose(..) )
import Data.Functor.Identity
  ( Identity(..) )
import Data.Traversable
  ( for )

-- transformers
import Control.Monad.Trans.State.Strict
  ( StateT(..), State, evalState )
import Control.Monad.Trans.Writer.Strict
  ( runWriter, tell )

---------------------------------------------------------------

iunzipWith
  :: (Traversable t, Num i, Enum i)
  => (i -> a -> b) -> (i -> a -> c) -> t a -> ( t b, [c] )
iunzipWith f g ta
  = runWriter
  $ ifor 0 succ ta \ i a -> do
       tell [g i a]
       pure ( f i a )

ifor
  :: forall t f i a b
  .  ( Applicative f, Traversable t )
  => i -> ( i -> i ) -> t a -> ( i -> a -> f b ) -> f (t b)
ifor i0 upd ta f = (`evalState` i0) . getCompose $ result
  where
    result :: Compose (State i) f (t b)
    result = for ta \ a -> ( coerce ( \ i -> ( f i a, upd i ) ) )
