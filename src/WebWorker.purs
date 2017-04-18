module WebWorker where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign (Foreign)
import Prelude (Unit)

foreign import data WebWorker :: Type
foreign import data IsWW :: Effect
foreign import data OwnsWW :: Effect

newtype MessageEvent = MessageEvent {data :: Foreign}

foreign import mkWorker :: forall eff.
                           String 
                           -> Eff (ownsww :: OwnsWW, err :: EXCEPTION | eff) WebWorker
foreign import terminateWorker :: forall eff.  WebWorker -> Eff (ownsww :: OwnsWW | eff) Unit
foreign import onmessageFromWorker :: forall eff. WebWorker 
                                      -> (MessageEvent -> Eff (ownsww :: OwnsWW | eff) Unit)
                                      -> Eff (ownsww :: OwnsWW | eff) Unit
foreign import postMessageToWorker :: forall eff. WebWorker 
                                              -> Foreign 
                                              -> Eff (ownsww :: OwnsWW | eff) Unit

foreign import postMessage :: forall eff. Foreign -> Eff (isww :: IsWW | eff) Unit
foreign import onmessage :: forall eff. (MessageEvent -> Eff (isww :: IsWW | eff) Unit)
                                              -> Eff (isww :: IsWW | eff) Unit
