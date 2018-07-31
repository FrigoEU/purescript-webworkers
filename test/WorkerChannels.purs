module Test.WorkerChannels where

import Prelude

import Foreign.Object (empty)
import Test.Main (message2Channel, message1Channel)
import WebWorker (EffectR, IsWW)
import WebWorker.Channel (onmessageC, postMessageC, registerChannel)

main :: EffectR ( isww :: IsWW ) Unit
main = onmessageC chs
  where
    chstemp = registerChannel empty message1Channel
                  (\({message}) -> postMessageC message2Channel {message: message <> "workertestmess1"})
    chs = registerChannel chstemp message2Channel
            (\({message}) -> postMessageC message2Channel {message: message <> "testmess2"})
