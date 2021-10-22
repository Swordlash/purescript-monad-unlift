module Effect.Aff.Unlift where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)


class MonadAff m <= MonadUnliftAff m where
  withRunInAff :: forall b. ((forall a. m a -> Aff a) -> Aff b) -> m b

instance MonadUnliftAff Aff where
  withRunInAff inner = inner identity

instance MonadUnliftAff m => MonadUnliftAff (ReaderT r m) where
  withRunInAff inner = ReaderT \r -> withRunInAff \run -> inner (run <<< flip runReaderT r)