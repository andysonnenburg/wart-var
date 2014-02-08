{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Var.Unboxed
       ( MonadVar (..)
       , contents
       , modify
       , STURef
       , IOURef
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad.ST.Lazy.Safe (strictToLazyST)
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
import Data.Monoid
import GHC.Exts (MutableByteArray#,
                 Int (..),
                 newByteArray#,
                 readIntArray#,
                 writeIntArray#)
import GHC.IO (IO (..))
import GHC.ST (ST (..))
import Prelude (Functor (..), Monad (..), ($), (.))

#include "MachDeps.h"

class (Applicative m, Monad m) => MonadVar a m where
  type Var m a
  new :: a -> m (Var m a)
  read :: Var m a -> m a
  write :: Var m a -> a -> m ()

#ifndef HLINT
  default new :: (MonadTrans t, MonadVar a m) => a -> t m (Var m a)
  {-# INLINE new #-}
  new = lift . new
#endif

#ifndef HLINT
  default read :: (MonadTrans t, MonadVar a m) => Var m a -> t m a
  {-# INLINE read #-}
  read = lift . read
#endif

#ifndef HLINT
  default write :: (MonadTrans t, MonadVar a m) => Var m a -> a -> t m ()
  {-# INLINE write #-}
  write var = lift . write var
#endif

contents :: MonadVar a m => IndexPreservingAction m (Var m a) a
{-# INLINE contents #-}
contents = act read

modify :: MonadVar a m => Var m a -> (a -> a) -> m ()
{-# INLINE modify #-}
modify var f = read var >>= write var . f

data STURef s a = STURef (MutableByteArray# s)

instance MonadVar Int (ST s) where
  type Var (ST s) Int = STURef s Int
  {-# INLINE new #-}
  new (I# a) = ST $ \ s -> case newByteArray# SIZEOF_HSINT# s of
    (# s', arr #) -> case writeIntArray# arr 0# a s' of
      s'' -> (# s'', STURef arr #)
  {-# INLINE read #-}
  read (STURef arr) = ST $ \ s -> case readIntArray# arr 0# s of
    (# s', a #) -> (# s', I# a #)
  {-# INLINE write #-}
  write (STURef arr) (I# a) = ST $ \ s -> case writeIntArray# arr 0# a s of
    s' -> (# s', () #)

instance MonadVar Int (Lazy.ST s) where
  type Var (Lazy.ST s) Int = STURef s Int
  {-# INLINE new #-}
  new = strictToLazyST . new
  {-# INLINE read #-}
  read = strictToLazyST . read
  {-# INLINE write #-}
  write var = strictToLazyST . write var

data IOURef a = IOURef (MutableByteArray# RealWorld)

instance MonadVar Int IO where
  type Var IO Int = IOURef Int
  {-# INLINE new #-}
  new (I# a) = IO $ \ s -> case newByteArray# SIZEOF_HSINT# s of
    (# s', arr #) -> case writeIntArray# arr 0# a s' of
      s'' -> (# s'', IOURef arr #)
  {-# INLINE read #-}
  read (IOURef arr) = IO $ \ s -> case readIntArray# arr 0# s of
    (# s', a #) -> (# s', I# a #)
  {-# INLINE write #-}
  write (IOURef arr) (I# a) = IO $ \ s -> case writeIntArray# arr 0# a s of
    s' -> (# s', () #)

instance MonadVar a m => MonadVar a (ContT r m) where
  type Var (ContT r m) a = Var m a

instance (Error e, MonadVar a m) => MonadVar a (ErrorT e m) where
  type Var (ErrorT e m) a = Var m a

instance MonadVar a m => MonadVar a (IdentityT m) where
  type Var (IdentityT m)a = Var m a

instance MonadVar a m => MonadVar a (ListT m) where
  type Var (ListT m)a = Var m a

instance MonadVar a m => MonadVar a (MaybeT m) where
  type Var (MaybeT m)a = Var m a

instance (Monoid w, MonadVar a m) => MonadVar a (Lazy.RWST r w s m) where
  type Var (Lazy.RWST r w s m)a = Var m a

instance (Monoid w, MonadVar a m) => MonadVar a (RWST r w s m) where
  type Var (RWST r w s m)a = Var m a

instance MonadVar a m => MonadVar a (ReaderT r m) where
  type Var (ReaderT r m)a = Var m a

instance MonadVar a m => MonadVar a (Lazy.StateT s m) where
  type Var (Lazy.StateT s m)a = Var m a

instance MonadVar a m => MonadVar a (StateT s m) where
  type Var (StateT s m)a = Var m a

instance (Monoid w, MonadVar a m) => MonadVar a (Lazy.WriterT w m) where
  type Var (Lazy.WriterT w m)a = Var m a

instance (Monoid w, MonadVar a m) => MonadVar a (WriterT w m) where
  type Var (WriterT w m)a = Var m a
