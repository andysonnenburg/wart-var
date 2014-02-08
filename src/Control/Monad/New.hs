{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.New
       ( MonadNew (..)
       ) where

import Control.Applicative
import qualified Control.Monad.ST.Lazy.Safe as Lazy
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Data.IORef
import Data.STRef
import qualified Data.STRef.Lazy as Lazy
import Prelude (IO, Monad (..), (.))

class (Applicative m, Monad m) => MonadNew var m where
  new :: a -> m (var a)

#ifndef HLINT
  default new :: (MonadTrans t, MonadNew var m) => a -> t m (var a)
  {-# INLINE new #-}
  new = lift . new
#endif

instance MonadNew IORef IO where
  {-# INLINE new #-}
  new = newIORef

instance MonadNew (STRef s) (ST s) where
  {-# INLINE new #-}
  new = newSTRef

instance MonadNew (STRef s) (Lazy.ST s) where
  {-# INLINE new #-}
  new = Lazy.newSTRef
