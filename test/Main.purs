module Test.Main where

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)
import Data.Either (Either, either)
import Data.Foreign (Foreign, MultipleErrors, readString)
import Data.Foreign.Index (readProp)
import Data.StrMap (empty)
import Prelude (Unit, bind, discard, pure, show, ($), (==), (>>=))
import Simple.JSON (readJSON, write, writeJSON)
import Test.Unit (timeout, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import WebWorker (onmessageFromWorker, MessageEvent(MessageEvent), OwnsWW, postMessageToWorker, mkWorker)
import WebWorker.Channel (onmessageFromWorkerC, registerChannel, postMessageToWorkerC, Channel(Channel))

readMessage :: Foreign -> Either MultipleErrors { message :: String}                               
readMessage obj = runExcept $ readProp "message" obj >>= readString >>= \message -> pure {message}

main :: forall eff. Eff ( timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT , ownsww :: OwnsWW, err :: EXCEPTION, console :: CONSOLE | eff ) Unit
main = runTest do
  test "make worker, send and receive Message object" do
    ww <- liftEff $ mkWorker "testworker.js"
    _ <- liftEff $ postMessageToWorker ww $ write {message: "testmessage"}
    timeout 1000 $ do mess <- makeAff \err success -> do onmessageFromWorker ww (\(MessageEvent {data: fn}) -> either (\e -> err $ error $ show e)
                                                                                                                      (\({message}) -> success message)
                                                                                                                      (readMessage fn))
                      assert "message is doubled" $ (mess == "testmessagetestmessage")
  test "make worker, send and receive with Channels" do
    ww <- liftEff $ mkWorker "testworkerchannels.js"
    liftEff $ postMessageToWorkerC ww message1Channel {message: "testmess1"}
    timeout 1000 $ do mess <- makeAff (\err success -> let chs = registerChannel empty message2Channel (\({message}) -> success message)
                                                        in onmessageFromWorkerC ww chs) 
                      assert "message is concatted" $ (mess == "testmess1workertestmess1")

message1Channel :: Channel {message :: String}
message1Channel = Channel { name: "message1", encode: writeJSON, decode: readJSON}
message2Channel :: Channel {message :: String}
message2Channel = Channel { name: "message2", encode: writeJSON, decode: readJSON}
