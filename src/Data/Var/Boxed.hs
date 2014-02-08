{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Var.Boxed
       ( MonadVar (..)
       , contents
       , modify
       ) where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.ST.Lazy.Safe as Lazy
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict (RWST)
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict
import Data.IORef
import Data.Monoid
import Data.STRef
import qualified Data.STRef.Lazy as Lazy
import Prelude (IO, Monad (..), (.))

class (Applicative m, Monad m) => MonadVar m where
  type Var m :: * -> *
  new :: a -> m (Var m a)
  read :: Var m a -> m a
  write :: Var m a -> a -> m ()

#ifndef HLINT
  default new :: (MonadTrans t, MonadVar m) => a -> t m (Var m a)
  {-# INLINE new #-}
  new = lift . new
#endif

#ifndef HLINT
  default read :: (MonadTrans t, MonadVar m) => Var m a -> t m a
  {-# INLINE read #-}
  read = lift . read
#endif

#ifndef HLINT
  default write :: (MonadTrans t, MonadVar m) => Var m a -> a -> t m ()
  {-# INLINE write #-}
  write var = lift . write var
#endif

contents :: MonadVar m => IndexPreservingAction m (Var m a) a
{-# INLINE contents #-}
contents = act read

modify :: MonadVar m => Var m a -> (a -> a) -> m ()
{-# INLINE modify #-}
modify var f = read var >>= write var . f

instance MonadVar (ST s) where
  type Var (ST s) = STRef s
  {-# INLINE new #-}
  new = newSTRef
  {-# INLINE read #-}
  read = readSTRef
  {-# INLINE write #-}
  write = writeSTRef

instance MonadVar (Lazy.ST s) where
  type Var (Lazy.ST s) = STRef s
  {-# INLINE new #-}
  new = Lazy.newSTRef
  {-# INLINE read #-}
  read = Lazy.readSTRef
  {-# INLINE write #-}
  write = Lazy.writeSTRef

instance MonadVar IO where
  type Var IO = IORef
  {-# INLINE new #-}
  new = newIORef
  {-# INLINE read #-}
  read = readIORef
  {-# INLINE write #-}
  write = writeIORef

instance MonadVar m => MonadVar (ContT r m) where
  type Var (ContT r m) = Var m

instance (Error e, MonadVar m) => MonadVar (ErrorT e m) where
  type Var (ErrorT e m) = Var m

instance MonadVar m => MonadVar (IdentityT m) where
  type Var (IdentityT m) = Var m

instance MonadVar m => MonadVar (ListT m) where
  type Var (ListT m) = Var m

instance MonadVar m => MonadVar (MaybeT m) where
  type Var (MaybeT m) = Var m

instance (Monoid w, MonadVar m) => MonadVar (Lazy.RWST r w s m) where
  type Var (Lazy.RWST r w s m) = Var m

instance (Monoid w, MonadVar m) => MonadVar (RWST r w s m) where
  type Var (RWST r w s m) = Var m

instance MonadVar m => MonadVar (ReaderT r m) where
  type Var (ReaderT r m) = Var m

instance MonadVar m => MonadVar (Lazy.StateT s m) where
  type Var (Lazy.StateT s m) = Var m

instance MonadVar m => MonadVar (StateT s m) where
  type Var (StateT s m) = Var m

instance (Monoid w, MonadVar m) => MonadVar (Lazy.WriterT w m) where
  type Var (Lazy.WriterT w m) = Var m

instance (Monoid w, MonadVar m) => MonadVar (WriterT w m) where
  type Var (WriterT w m) = Var m
