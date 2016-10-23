module Test.Worker where

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Prelude (Unit, ($), (<>))
import Test.Main (Message(Message))
import WebWorker (MessageEvent(MessageEvent), IsWW, postMessage, onmessage)

main :: forall eff. Eff ( isww :: IsWW | eff ) Unit
main = onmessage handler
  where
    errorM = Message {message: "Failed to read Message in WW"}
    succ m = Message {message: m <> m}
    handler (MessageEvent {data: fn}) =
      either (\_ -> postMessage $ toForeign errorM) 
      (\(Message {message}) -> postMessage $ toForeign (succ message)) 
      (runExcept $ read fn)
