module Effect.Unlift where

import Prelude

import Control.Monad.Identity.Trans (IdentityT(..), runIdentityT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Effect (Effect)
import Effect.Class (class MonadEffect)

class MonadEffect m <= MonadUnliftEffect m where
  withRunInEffect :: forall b. ((forall a. m a -> Effect a) -> Effect b) -> m b

instance MonadUnliftEffect Effect where
  withRunInEffect runInEffect = runInEffect identity

instance MonadUnliftEffect m => MonadUnliftEffect (ReaderT r m) where
  withRunInEffect inner = ReaderT \r -> withRunInEffect \run -> inner (run <<< flip runReaderT r)

instance MonadUnliftEffect m => MonadUnliftEffect (IdentityT m) where
  withRunInEffect inner = IdentityT $ withRunInEffect \run -> inner (run <<< runIdentityT)