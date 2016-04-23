module Test.Main where

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read, readProp, class IsForeign)
import Prelude (Unit, (==), ($), show, bind, return, (>>=))
import Test.Unit (TIMER, timeout, test, runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import WebWorker (MessageEvent(MessageEvent), OwnsWW, onmessageFromWorker, postMessageToWorker, mkWorker)

newtype Message = Message {message :: String}
instance isForeignMessage :: IsForeign Message where
  read obj = readProp "message" obj >>= \message -> return $ Message {message}

main :: forall eff. Eff ( timer :: TIMER , avar :: AVAR , testOutput :: TESTOUTPUT , ownsww :: OwnsWW, exception :: EXCEPTION | eff ) Unit
main = runTest do
  test "make worker, send and receive Message object" do
    ww <- liftEff $ mkWorker "testworker.js"
    liftEff $ postMessageToWorker ww $ toForeign (Message {message: "testmessage"})
    timeout 100 $ do mess <- makeAff (\err success -> do 
                                          onmessageFromWorker ww (\(MessageEvent {data: fn}) -> either (\e -> err $ error $ show e)
                                                                                        (\(Message {message}) -> success message)
                                                                                        (read fn)))
                     assert "message is doubled" $ (mess == "testmessagetestmessage")
