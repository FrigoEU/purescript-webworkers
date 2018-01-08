module Test.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (toForeign)
import Prelude (Unit, ($), (<>))
import Test.Main (readMessage)
import WebWorker (MessageEvent(MessageEvent), IsWW, postMessage, onmessage)

main :: forall eff. Eff ( isww :: IsWW | eff ) Unit
main = onmessage handler
  where
    errorM = {message: "Failed to read Message in WW"}
    succ m = {message: m <> m}
    handler (MessageEvent {data: fn}) =
      either (\_ -> postMessage $ toForeign errorM) 
      (\({message}) -> postMessage $ toForeign (succ message)) 
      (runExcept $ readMessage fn)
