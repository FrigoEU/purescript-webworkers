module Test.Main where

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Argonaut.Decode (gDecodeJson, class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read, readProp, class IsForeign)
import Data.Generic (class Generic)
import Data.StrMap (empty)
import Prelude (Unit, (==), ($), show, bind, return, (>>=))
import Test.Unit (TIMER, timeout, test, runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import WebWorker (Channel(Channel), onmessageFromWorkerC, postMessageToWorkerC, registerChannel, MessageEvent(MessageEvent), OwnsWW, onmessageFromWorker, postMessageToWorker, mkWorker)

newtype Message = Message {message :: String}
instance isForeignMessage :: IsForeign Message where
  read obj = readProp "message" obj >>= \message -> return $ Message {message}

main :: forall eff. Eff ( timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT , ownsww :: OwnsWW, exception :: EXCEPTION, console :: CONSOLE | eff ) Unit
main = runTest do
  test "make worker, send and receive Message object" do
    ww <- liftEff $ mkWorker "testworker.js"
    liftEff $ postMessageToWorker ww $ toForeign (Message {message: "testmessage"})
    timeout 1000 $ do mess <- makeAff (\err success -> do 
                                          onmessageFromWorker ww (\(MessageEvent {data: fn}) -> either (\e -> err $ error $ show e)
                                                                                        (\(Message {message}) -> success message)
                                                                                        (read fn)))
                      assert "message is doubled" $ (mess == "testmessagetestmessage")
  test "make worker, send and receive with Channels" do
    ww <- liftEff $ mkWorker "testworkerchannels.js"
    liftEff $ postMessageToWorkerC ww message1Channel (Message1 {message: "testmess1"})
    timeout 1000 $ do mess <- makeAff (\err success -> let chs = registerChannel empty message2Channel (\(Message2 {message}) -> success message)
                                                        in onmessageFromWorkerC ww chs) 
                      assert "message is concatted" $ (mess == "testmess1workertestmess1")

newtype Message1 = Message1 {message :: String}
derive instance genericMessage1 :: Generic Message1
instance encodeJsonMessage1 :: EncodeJson Message1 where encodeJson = gEncodeJson
instance decodeJsonMessage1 :: DecodeJson Message1 where decodeJson = gDecodeJson
newtype Message2 = Message2 {message :: String}
derive instance genericMessage2 :: Generic Message2
instance encodeJsonMessage2 :: EncodeJson Message2 where encodeJson = gEncodeJson
instance decodeJsonMessage2 :: DecodeJson Message2 where decodeJson = gDecodeJson

message1Channel :: Channel Message1
message1Channel = Channel "message1" 
message2Channel :: Channel Message2
message2Channel = Channel "message2" 
