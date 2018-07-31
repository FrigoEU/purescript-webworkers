module Test.Worker where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Foreign (unsafeToForeign)
import Test.Main (readMessage)
import WebWorker (EffectR, IsWW, MessageEvent(MessageEvent), onmessage, postMessage)

main :: EffectR ( isww :: IsWW ) Unit
main = onmessage handler
  where
    errorM = {message: "Failed to read Message in WW"}
    succ m = {message: m <> m}
    handler (MessageEvent {data: fn}) =
      either (\_ -> postMessage $ unsafeToForeign errorM)
      (\({message}) -> postMessage $ unsafeToForeign (succ message))
      (runExcept $ readMessage fn)
