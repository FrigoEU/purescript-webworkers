module Test.Main where

import Prelude

import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (F, Foreign, readString)
import Foreign.Index (readProp)
import Foreign.Object (empty)
import Simple.JSON (readJSON, write, writeJSON)
import Test.Unit (timeout, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)
import WebWorker (EffectR(..), MessageEvent(MessageEvent), mkWorker, onmessageFromWorker, postMessageToWorker, runEffectR)
import WebWorker.Channel (onmessageFromWorkerC, registerChannel, postMessageToWorkerC, Channel(Channel))

readMessage :: Foreign -> F { message :: String}
readMessage obj = readProp "message" obj >>= readString >>= \message -> pure {message}

main :: Effect Unit
main = runTest do
  test "make worker, send and receive Message object" do
    ww <- liftEffect <<< runEffectR $ mkWorker "testworker.js"
    _ <- liftEffect <<< runEffectR $ postMessageToWorker ww $ write {message: "testmessage"}
    timeout 1000 $ do mess <- makeAff \cb -> mempty <* runEffectR do onmessageFromWorker ww (\(MessageEvent {data: fn}) -> either (\e -> EffectR $ cb <<< Left $ error $ show e)
                                                                                                             (\({message}) -> EffectR $ cb $ Right message)
                                                                                                             (readMessage fn # runExcept))
                      assert "message is doubled" $ (mess == "testmessagetestmessage")
  test "make worker, send and receive with Channels" do
    ww <- liftEffect <<< runEffectR $ mkWorker "testworkerchannels.js"
    liftEffect <<< runEffectR $ postMessageToWorkerC ww message1Channel {message: "testmess1"}
    timeout 1000 $ do mess <- makeAff (\cb -> let chs = registerChannel empty message2Channel (\({message}) -> EffectR $ cb <<< Right $ message)
                                                        in mempty <* runEffectR $ onmessageFromWorkerC ww chs)
                      assert "message is concatted" $ (mess == "testmess1workertestmess1")

message1Channel :: Channel {message :: String}
message1Channel = Channel { name: "message1", encode: writeJSON, decode: readJSON >>> except}
message2Channel :: Channel {message :: String}
message2Channel = Channel { name: "message2", encode: writeJSON, decode: readJSON >>> except}
