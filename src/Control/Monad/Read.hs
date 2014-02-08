{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Read
       ( MonadRead (..)
       , contents
       ) where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.ST.Lazy.Safe as Lazy
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Data.IORef
import Data.STRef
import qualified Data.STRef.Lazy as Lazy
import Prelude (IO, Monad (..), (.))

class (Applicative m, Monad m) => MonadRead var m where
  read :: var a -> m a

#ifndef HLINT
  default read :: (MonadTrans t, MonadRead var m) => var a -> t m a
  {-# INLINE read #-}
  read = lift . read
#endif

contents :: MonadRead var m => IndexPreservingAction m (var a) a
{-# INLINE contents #-}
contents = act read

instance MonadRead IORef IO where
  {-# INLINE read #-}
  read = readIORef

instance MonadRead (STRef s) (ST s) where
  {-# INLINE read #-}
  read = readSTRef

instance MonadRead (STRef s) (Lazy.ST s) where
  {-# INLINE read #-}
  read = Lazy.readSTRef
