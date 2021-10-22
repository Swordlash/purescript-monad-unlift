module Effect.Aff.Unlift where

import Prelude

import Effect.Aff (Aff)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Identity.Trans (IdentityT(..), runIdentityT)


class Monad m <= MonadUnliftAff m where
  withRunInAff :: forall b. ((forall a. m a -> Aff a) -> Aff b) -> m b

instance MonadUnliftAff Aff where
  withRunInAff inner = inner identity

instance MonadUnliftAff m => MonadUnliftAff (ReaderT r m) where
  withRunInAff inner = ReaderT \r -> withRunInAff \run -> inner (run <<< flip runReaderT r)

instance MonadUnliftAff m => MonadUnliftAff (IdentityT m) where
  withRunInAff inner = IdentityT $ withRunInAff \run -> inner (run <<< runIdentityT)