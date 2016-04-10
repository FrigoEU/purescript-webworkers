module WebWorker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign (Foreign)
import Prelude (Unit)

foreign import data WebWorker :: *
foreign import data IsWW :: !
foreign import data OwnsWW :: !

foreign import supportsWebWorkers :: Boolean

foreign import mkWorker :: forall eff1 eff2.
                           String 
                           -> (Foreign -> Eff eff1 Unit) 
                           -> Eff (ownsww :: OwnsWW, exception :: EXCEPTION | eff2) WebWorker
foreign import postMessageToWW :: forall eff. WebWorker 
                                              -> Foreign 
                                              -> Eff (ownsww :: OwnsWW | eff) Unit

foreign import postMessage :: forall eff. Foreign -> Eff (isww :: IsWW | eff) Unit
foreign import onmessage :: forall eff1 eff2. (Foreign -> Eff eff1 Unit)
                                              -> Eff (isww :: IsWW | eff2) Unit
