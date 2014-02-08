{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Write
       ( MonadWrite (..)
       ) where

import Control.Applicative
import qualified Control.Monad.ST.Lazy.Safe as Lazy
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Data.IORef
import Data.STRef
import qualified Data.STRef.Lazy as Lazy
import Prelude (IO, Monad (..), (.))

class (Applicative m, Monad m) => MonadWrite var m where
  write :: var a -> a -> m ()

#ifndef HLINT
  default write :: (MonadTrans t, MonadWrite var m) => var a -> a -> t m ()
  {-# INLINE write #-}
  write v = lift . write v
#endif

instance MonadWrite IORef IO where
  {-# INLINE write #-}
  write = writeIORef

instance MonadWrite (STRef s) (ST s) where
  {-# INLINE write #-}
  write = writeSTRef

instance MonadWrite (STRef s) (Lazy.ST s) where
  {-# INLINE write #-}
  write = Lazy.writeSTRef
