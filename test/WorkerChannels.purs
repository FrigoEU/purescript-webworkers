module Test.WorkerChannels where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.StrMap (empty)
import Prelude (Unit, (<>))
import Test.Main (Message1(Message1), Message2(Message2), message2Channel, message1Channel)
import WebWorker (IsWW)
import WebWorker.Channel (onmessageC, postMessageC, registerChannel)

main :: forall eff. Eff ( isww :: IsWW, console :: CONSOLE | eff ) Unit
main = onmessageC chs
  where 
    chstemp = registerChannel empty message1Channel 
                  (\(Message1 {message}) -> postMessageC message2Channel 
                                              (Message2 {message: message <> "workertestmess1"}))
    chs = registerChannel chstemp message2Channel 
            (\(Message2 {message}) -> postMessageC message2Channel 
                                        (Message2 {message: message <> "testmess2"}))
