module WebWorker where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(Right, Left), either)
import Data.Foreign (toForeign, Foreign)
import Data.Foreign.Class (read, readProp)
import Data.Maybe (maybe)
import Data.StrMap (lookup, insert, StrMap)
import Prelude (id, Unit, return, ($), show, (<>), bind, unit, (<*>), (<$>))

foreign import data WebWorker :: *
foreign import data IsWW :: !
foreign import data OwnsWW :: !

newtype MessageEvent = MessageEvent {data :: Foreign}
newtype Channel a = Channel String

foreign import supportsWebWorkers :: Boolean

foreign import mkWorker :: forall eff.
                           String 
                           -> Eff (ownsww :: OwnsWW, exception :: EXCEPTION | eff) WebWorker
foreign import onmessageFromWorker :: forall eff. WebWorker 
                                      -> (MessageEvent -> Eff eff Unit)
                                      -> Eff (ownsww :: OwnsWW | eff) Unit
foreign import postMessageToWorker :: forall eff. WebWorker 
                                              -> Foreign 
                                              -> Eff (ownsww :: OwnsWW | eff) Unit

foreign import postMessage :: forall eff. Foreign -> Eff (isww :: IsWW | eff) Unit
foreign import onmessage :: forall eff. (MessageEvent -> Eff eff Unit)
                                              -> Eff (isww :: IsWW | eff) Unit

type Channels eff = StrMap (Json -> Eff eff Unit)

registerChannel :: forall eff a. (DecodeJson a) =>
                                 Channels (console :: CONSOLE | eff) 
                                 -> Channel a 
                                 -> (a -> Eff (console :: CONSOLE | eff) Unit) 
                                 -> Channels (console :: CONSOLE | eff)
registerChannel chs (Channel str) handle = 
  insert str (\json -> either (\_ -> log $ "Failed to decode json for channel " <> str) 
                              handle
                              (decodeJson json)) chs

onmessageC :: forall eff. Channels (console :: CONSOLE | eff) -> Eff (isww :: IsWW, console :: CONSOLE | eff) Unit
onmessageC chs = onmessage (handleMessageWithChannels chs)

onmessageFromWorkerC :: forall eff. WebWorker -> Channels (console :: CONSOLE | eff) -> Eff (ownsww :: OwnsWW, console :: CONSOLE | eff) Unit
onmessageFromWorkerC ww chs = onmessageFromWorker ww (handleMessageWithChannels chs)

handleMessageWithChannels :: forall eff. Channels (console :: CONSOLE | eff) -> MessageEvent -> Eff (console :: CONSOLE | eff) Unit
handleMessageWithChannels chs (MessageEvent {data: d})  = 
  either (\_ -> return unit) handleMess ({t: _, m: _} <$> readProp "type" d <*> readProp "message" d)
    where 
      handleMess :: {t :: String, m :: Foreign} -> Eff (console :: CONSOLE | eff) Unit
      handleMess {t, m} = 
        either log id do
          handler <- maybe (Left $ "Couldn't find handler for channel " <> t) Right (lookup t chs)
          str <- either (\e -> Left $ "Couldn't parse message from channel " <> t <> " to string: " <> show e) Right (read m)
          json <- either (\e -> Left $ "Couldn't parse message from channel " <> t <> " to json: " <> show e) Right (jsonParser str)
          return $ handler json

postMessageC :: forall a eff. (EncodeJson a) => Channel a -> a -> Eff (isww :: IsWW | eff) Unit
postMessageC (Channel str) a = postMessage (toForeign {type: str, message: ((printJson $ encodeJson a) :: String)})

postMessageToWorkerC :: forall a eff. (EncodeJson a) => WebWorker -> Channel a -> a -> Eff (ownsww :: OwnsWW | eff) Unit
postMessageToWorkerC ww (Channel str) a = postMessageToWorker ww (toForeign {type: str, message: ((printJson $ encodeJson a) :: String)})
