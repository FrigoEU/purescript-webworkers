module WebWorker where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Foreign (Foreign)

foreign import data WebWorker :: Type

newtype EffectR (roles :: # RoleWW) a = EffectR (Effect a)
derive newtype instance functorEffectR :: Functor (EffectR roles)
derive newtype instance applyEffectR :: Apply (EffectR roles)
derive newtype instance applicativeEffectR :: Applicative (EffectR roles)
derive newtype instance bindEffectR :: Bind (EffectR roles)
derive newtype instance monadEffectR :: Monad (EffectR roles)
derive newtype instance monadEffectEffectR :: MonadEffect (EffectR roles)

runEffectR :: forall roles a. EffectR roles a -> Effect a
runEffectR (EffectR effect) = effect

foreign import kind RoleWW
foreign import data IsWW :: RoleWW
foreign import data OwnsWW :: RoleWW

newtype MessageEvent = MessageEvent {data :: Foreign}

foreign import mkWorker :: String -> EffectR (ownsww :: OwnsWW) WebWorker
foreign import terminateWorker ::  WebWorker -> EffectR (ownsww :: OwnsWW) Unit
foreign import onmessageFromWorker :: WebWorker -> (MessageEvent -> EffectR (ownsww :: OwnsWW) Unit) -> EffectR (ownsww :: OwnsWW) Unit
foreign import postMessageToWorker :: WebWorker -> Foreign -> EffectR (ownsww :: OwnsWW) Unit

foreign import postMessage :: Foreign -> EffectR (isww :: IsWW) Unit
foreign import onmessage :: (MessageEvent -> EffectR (isww :: IsWW) Unit) -> EffectR (isww :: IsWW) Unit
