-- | The pure half of the QQ open platform gateway protocol.
--
-- The connection is outbound and deliberately dumb: this platform dials the
-- bot, so nothing has to be reachable from the internet for Max to receive
-- events.  What the protocol does care about is session continuity — a dropped
-- connection is resumed from the last sequence number and the platform replays
-- what was missed, which is why this adapter keeps a cursor at all and needs no
-- message-history backfill.
--
-- Opcodes, payloads and the close-code policy live here as pure functions so
-- they can be tested without a network; "Max.QQOfficial" owns the socket.
module Max.QQOfficial.Gateway
  ( GatewayFrame (..),
    CloseRecovery (..),
    parseGatewayFrame,
    readySession,
    gatewayConnectTarget,
    identifyPayload,
    resumePayload,
    heartbeatPayload,
    closeRecovery,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), eitherDecodeStrict', object, withObject, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T

data GatewayFrame = GatewayFrame
  { frameOp :: !Int,
    frameSeq :: !(Maybe Int64),
    -- | Event name; only meaningful for a dispatch.
    frameType :: !(Maybe Text),
    frameData :: !Value,
    -- | OpCode 10 carries the heartbeat period in milliseconds.
    frameHeartbeatMillis :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

parseGatewayFrame :: BS.ByteString -> Either Text GatewayFrame
parseGatewayFrame bytes = case eitherDecodeStrict' bytes of
  Left err -> Left ("QQ official gateway frame: " <> T.pack err)
  Right value -> case parseEither frameParser value of
    Left err -> Left ("QQ official gateway frame: " <> T.pack err)
    Right frame -> Right frame
  where
    frameParser = withObject "gateway frame" $ \o -> do
      op <- o .: "op"
      seqNo <- o .:? "s"
      name <- o .:? "t"
      dataValue <- o .:? "d" .!= Object mempty
      -- A heartbeat period only exists on the hello frame, and a dispatch's
      -- payload is an object with a different shape; neither may fail the parse.
      let interval = case dataValue of
            Object fields -> case KeyMap.lookup "heartbeat_interval" fields of
              Just (Number n) -> Just (truncate n :: Int)
              _ -> Nothing
            _ -> Nothing
      pure (GatewayFrame op seqNo name dataValue interval)

-- | The session a ready frame hands out, together with the bot's own identity.
--
-- The user block is this platform's only report of what the bot is called and
-- what id its own messages carry; both are worth keeping, and neither is a
-- numeric account id.
readySession :: Value -> Maybe (Text, Maybe Text, Maybe Text)
readySession payload = case parseMaybe parser payload of
  Nothing -> Nothing
  Just ready -> Just ready
  where
    parser = withObject "ready frame" $ \o -> do
      session <- o .: "session_id"
      user <- o .:? "user"
      let userField name = case user of
            Nothing -> Nothing
            Just value -> parseMaybe (withObject "ready user" (\u -> u .:? name)) value
      pure (session, textField "id" (userField "id"), textField "username" (userField "username"))
    textField _ Nothing = Nothing
    textField _ (Just (String value)) = Just value
    textField _ (Just _) = Nothing

-- | @wss://host/path@ as @connectTLS@ wants it.  The port is implied by @wss@
-- and the platform always returns one.
gatewayConnectTarget :: Text -> Maybe Text
gatewayConnectTarget url = do
  rest <- T.stripPrefix "wss://" url <|> T.stripPrefix "ws://" url
  let (authority, path) = T.breakOn "/" rest
      path' = if T.null path then "/" else path
  if T.null authority || ':' `T.isInfixOf` authority
    then Nothing
    else Just (authority <> ":443" <> path')

-- | Identify is how a fresh session starts.  The token is presented as
-- @QQBot \<access token\>@, which is not the string the OpenAPI header carries.
identifyPayload :: Text -> Int64 -> Value
identifyPayload token intents =
  object
    [ "token" .= ("QQBot " <> token),
      "intents" .= intents,
      -- One shard: this adapter is a single instance and does not spread one
      -- bot across connections.
      "shard" .= ([0, 1] :: [Int]),
      "properties"
        .= object
          [ "$os" .= ("linux" :: Text),
            "$browser" .= ("max" :: Text),
            "$device" .= ("max" :: Text)
          ]
    ]

-- | Resume reattaches to a session the platform may still remember.  @seq@ is
-- the last dispatch sequence this process handled, and the platform replays
-- everything after it.
resumePayload :: Text -> Text -> Int64 -> Value
resumePayload token sessionId seqNo =
  object
    [ "token" .= ("QQBot " <> token),
      "session_id" .= sessionId,
      "seq" .= seqNo
    ]

-- | Heartbeat carries the last sequence number seen, so the platform knows what
-- this connection has already processed.  The first heartbeat, before any
-- dispatch, sends null.
heartbeatPayload :: Maybe Int64 -> Value
heartbeatPayload seqNo = object ["op" .= (1 :: Int), "d" .= seqNo]

-- | What a close code means for the next attempt.
data CloseRecovery
  = -- | Try Resume on a new socket; the session is still alive.
    RecoveryResume
  | -- | Start over with Identify.
    RecoveryIdentify
  | -- | The bot is off the shelf or banned.  Reconnecting is pointless and looks
    -- like an attack; stop and let a human look.
    RecoveryFatal
  deriving stock (Eq, Show)

closeRecovery :: Int -> CloseRecovery
closeRecovery code = case code of
  4007 -> RecoveryIdentify -- seq error: resume is not possible
  4009 -> RecoveryResume -- connection expired; resume is explicitly allowed
  4010 -> RecoveryFatal -- invalid shard
  4012 -> RecoveryFatal -- invalid version
  4914 -> RecoveryFatal -- bot taken offline
  4915 -> RecoveryFatal -- bot banned
  _ -> RecoveryIdentify