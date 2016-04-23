module WebWorker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign (Foreign)
import Prelude (Unit)

foreign import data WebWorker :: *
foreign import data IsWW :: !
foreign import data OwnsWW :: !

newtype MessageEvent = MessageEvent {data :: Foreign}

foreign import supportsWebWorkers :: Boolean

foreign import mkWorker :: forall eff.
                           String 
                           -> Eff (ownsww :: OwnsWW, exception :: EXCEPTION | eff) WebWorker
foreign import onmessageFromWorker :: forall eff1 eff2. WebWorker 
                                      -> (MessageEvent -> Eff eff1 Unit)
                                      -> Eff (ownsww :: OwnsWW | eff2) Unit
foreign import postMessageToWorker :: forall eff. WebWorker 
                                              -> Foreign 
                                              -> Eff (ownsww :: OwnsWW | eff) Unit

foreign import postMessage :: forall eff. Foreign -> Eff (isww :: IsWW | eff) Unit
foreign import onmessage :: forall eff1 eff2. (MessageEvent -> Eff eff1 Unit)
                                              -> Eff (isww :: IsWW | eff2) Unit
