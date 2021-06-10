module Faker.Class
  ( MonadFake(..)
  ) where

import Faker (FakeT(..), Fake)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)

class Monad m => MonadFake m where
  liftFake :: Fake a -> m a

instance MonadIO m => MonadFake (FakeT m) where
  liftFake (Fake f) = FakeT (liftIO . f)

instance MonadFake m => MonadFake (ReaderT r m) where
  liftFake = lift . liftFake
instance (Monoid w, MonadFake m) => MonadFake (WriterT w m) where
  liftFake = lift . liftFake
instance MonadFake m => MonadFake (StateT s m) where
  liftFake = lift . liftFake
instance MonadFake m => MonadFake (IdentityT m) where
  liftFake = lift . liftFake
instance MonadFake m => MonadFake (ExceptT e m) where
  liftFake = lift . liftFake
instance MonadFake m => MonadFake (MaybeT m) where
  liftFake = lift . liftFake
