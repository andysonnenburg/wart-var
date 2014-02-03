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

  default new :: (MonadTrans t, MonadNew var m) => a -> t m (var a)
  new = lift . new

instance MonadNew IORef IO where
  new = newIORef

instance MonadNew (STRef s) (ST s) where
  new = newSTRef

instance MonadNew (STRef s) (Lazy.ST s) where
  new = Lazy.newSTRef
