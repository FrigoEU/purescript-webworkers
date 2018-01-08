module Test.WorkerChannels where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.StrMap (empty)
import Prelude (Unit, (<>))
import Test.Main (message2Channel, message1Channel)
import WebWorker (IsWW)
import WebWorker.Channel (onmessageC, postMessageC, registerChannel)

main :: forall eff. Eff ( isww :: IsWW, console :: CONSOLE | eff ) Unit
main = onmessageC chs
  where 
    chstemp = registerChannel empty message1Channel 
                  (\({message}) -> postMessageC message2Channel {message: message <> "workertestmess1"})
    chs = registerChannel chstemp message2Channel 
            (\({message}) -> postMessageC message2Channel {message: message <> "testmess2"})
