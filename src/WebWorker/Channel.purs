module WebWorker.Channel (
  registerChannel,
  Channel(..),
  Channels(..),
  onmessageC,
  onmessageFromWorkerC,
  postMessageC,
  postMessageToWorkerC
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(Right, Left), either)
import Data.Maybe (maybe)
import Effect.Class.Console (log)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Object (Object, insert, lookup)
import WebWorker (EffectR, IsWW, MessageEvent(MessageEvent), OwnsWW, WebWorker, onmessage, onmessageFromWorker, postMessage, postMessageToWorker, kind RoleWW)

newtype Channel a = Channel { name :: String
                            , encode :: a -> String
                            , decode :: String -> F a
                            }
type Channels (roles :: # RoleWW) = Object (String -> EffectR roles Unit)

registerChannel :: forall roles a. Channels roles
                                   -> Channel a
                                   -> (a -> EffectR roles Unit)
                                   -> Channels roles
registerChannel chs (Channel {name, decode}) handle =
  insert name (\str -> either (\_ -> log $ "Failed to decode string for channel " <> name)
                              handle
                              (runExcept $ decode str )) chs

onmessageC :: Channels (isww :: IsWW) -> EffectR (isww :: IsWW) Unit
onmessageC chs = onmessage (handleMessageWithChannels chs)

onmessageFromWorkerC :: WebWorker -> Channels (ownsww :: OwnsWW) -> EffectR (ownsww :: OwnsWW) Unit
onmessageFromWorkerC ww chs = onmessageFromWorker ww (handleMessageWithChannels chs)

handleMessageWithChannels :: forall roles. Channels roles -> MessageEvent -> EffectR roles Unit
handleMessageWithChannels chs (MessageEvent {data: d})  =
  either (\_ -> pure unit)
         handleMess
         (runExcept $ {t: _, m: _} <$> (readProp "type" >=> readString) d <*> readProp "message" d)
    where
      handleMess :: {t :: String, m :: Foreign} -> EffectR roles Unit
      handleMess {t, m} =
        either log identity do
          handler <- maybe (Left $ "Couldn't find handler for channel " <> t) Right (lookup t chs)
          str <- either (\e -> Left $ "Couldn't parse message from channel " <> t <> " to string: " <> show e) Right (runExcept $ readString m)
          pure $ handler str

postMessageC :: forall a. Channel a -> a -> EffectR (isww :: IsWW) Unit
postMessageC (Channel {name, encode}) a = postMessage (unsafeToForeign {type: name, message: (encode a :: String)})

postMessageToWorkerC :: forall a. WebWorker -> Channel a -> a -> EffectR (ownsww :: OwnsWW) Unit
postMessageToWorkerC ww (Channel {name, encode}) a = postMessageToWorker ww (unsafeToForeign {type: name, message: encode a})
