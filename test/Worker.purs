module Test.Worker where

import Control.Monad.Eff (Eff)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Prelude (Unit, ($), (<>))
import Test.Main (Message(Message))
import WebWorker (MessageEvent(MessageEvent), IsWW, postMessage, onmessage)

main :: forall eff. Eff ( isww :: IsWW | eff ) Unit
main = do
  onmessage (\(MessageEvent {data: fn}) -> either (\_ -> postMessage $ toForeign (Message {message: "Failed to read Message in WW"})) 
                                   (\(Message {message}) -> postMessage $ toForeign (Message {message: message <> message})) 
                                   (read fn))
