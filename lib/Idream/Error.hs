
module Idream.Error ( module Control.Monad.Freer.Error
                    , convertError, runError'
                    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error


runError' :: forall e1 e2 effs a.
             (e1 -> e2)
          -> Eff (Error e1 ': effs) a -> Eff effs (Either e2 a)
runError' f = runError . convertError f


convertError :: forall e1 e2 effs a.
                (e1 -> e2)
             -> Eff (Error e1 ': effs) a
             -> Eff (Error e2 ': effs) a
convertError f = reinterpret $ \case Error e -> throwError $ f e
