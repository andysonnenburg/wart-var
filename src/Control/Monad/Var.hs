module Control.Monad.Var
       ( module Control.Monad.New
       , module Control.Monad.Read
       , module Control.Monad.Write
       , modify
       ) where

import Control.Monad.New
import Control.Monad.Read
import Control.Monad.Write
import Prelude ((.), (=<<))

modify :: (MonadRead var m, MonadWrite var m) => var a -> (a -> a) -> m ()
modify var f = write var . f =<< read var
