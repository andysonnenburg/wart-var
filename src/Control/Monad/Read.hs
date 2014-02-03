{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Read
       ( MonadRead (..)
       ) where

import Control.Applicative
import qualified Control.Monad.ST.Lazy.Safe as Lazy
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Data.IORef
import Data.STRef
import qualified Data.STRef.Lazy as Lazy
import Prelude (IO, Monad (..), (.))

class (Applicative m, Monad m) => MonadRead var m where
  read :: var a -> m a

  default read :: (MonadTrans t, MonadRead var m) => var a -> t m a
  read = lift . read

instance MonadRead IORef IO where
  read = readIORef

instance MonadRead (STRef s) (ST s) where
  read = readSTRef

instance MonadRead (STRef s) (Lazy.ST s) where
  read = Lazy.readSTRef
