module WebWorker.Channel (
  registerChannel,
  Channel(..),
  Channels(..),
  onmessageC,
  onmessageFromWorkerC,
  postMessageC,
  postMessageToWorkerC
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Right, Left), either)
import Data.Foreign (Foreign, MultipleErrors, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Maybe (maybe)
import Data.StrMap (insert, lookup, StrMap)
import Prelude (Unit, unit, pure, show, bind, id, ($), (<*>), (<$>), (<>), (>=>))
import WebWorker (postMessageToWorker, postMessage, MessageEvent(MessageEvent), onmessageFromWorker, onmessage, OwnsWW, WebWorker, IsWW)

newtype Channel a = Channel { name :: String
                            , encode :: a -> String
                            , decode :: String -> Either MultipleErrors a
                            }
type Channels eff = StrMap (String -> Eff eff Unit)

registerChannel :: forall eff a. Channels (console :: CONSOLE | eff)
                                 -> Channel a
                                 -> (a -> Eff (console :: CONSOLE | eff) Unit)
                                 -> Channels (console :: CONSOLE | eff)
registerChannel chs (Channel {name, decode}) handle =
  insert name (\str -> either (\_ -> log $ "Failed to decode string for channel " <> name)
                              handle
                              (decode str)) chs

onmessageC :: forall eff. Channels (isww :: IsWW, console :: CONSOLE | eff) -> Eff (isww :: IsWW, console :: CONSOLE | eff) Unit
onmessageC chs = onmessage (handleMessageWithChannels chs)

onmessageFromWorkerC :: forall eff. WebWorker -> Channels (ownsww :: OwnsWW, console :: CONSOLE | eff) -> Eff (ownsww :: OwnsWW, console :: CONSOLE | eff) Unit
onmessageFromWorkerC ww chs = onmessageFromWorker ww (handleMessageWithChannels chs)

handleMessageWithChannels :: forall eff. Channels (console :: CONSOLE | eff) -> MessageEvent -> Eff (console :: CONSOLE | eff) Unit
handleMessageWithChannels chs (MessageEvent {data: d})  =
  either (\_ -> pure unit)
         handleMess
         (runExcept $ {t: _, m: _} <$> (readProp "type" >=> readString) d <*> readProp "message" d)
    where
      handleMess :: {t :: String, m :: Foreign} -> Eff (console :: CONSOLE | eff) Unit
      handleMess {t, m} =
        either log id do
          handler <- maybe (Left $ "Couldn't find handler for channel " <> t) Right (lookup t chs)
          str <- either (\e -> Left $ "Couldn't parse message from channel " <> t <> " to string: " <> show e) Right (runExcept $ readString m)
          pure $ handler str

postMessageC :: forall a eff. Channel a -> a -> Eff (isww :: IsWW | eff) Unit
postMessageC (Channel {name, encode}) a = postMessage (toForeign {type: name, message: (encode a :: String)})

postMessageToWorkerC :: forall a eff. WebWorker -> Channel a -> a -> Eff (ownsww :: OwnsWW | eff) Unit
postMessageToWorkerC ww (Channel {name, encode}) a = postMessageToWorker ww (toForeign {type: name, message: encode a})
