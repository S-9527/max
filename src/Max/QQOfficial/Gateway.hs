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
    GatewayTarget (..),
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

import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, parseMaybe, withObject, (.:), (.:?), (.!=))
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
readySession payload = parseMaybe parser payload
  where
    parser = withObject "ready frame" $ \o -> do
      session <- o .: "session_id"
      user <- o .:? "user"
      pure (session, userText "id" user, userText "username" user)
    -- The user block is the only place this platform says what the bot is
    -- called; a bot that sends no @user@ block is not a failure, just a
    -- session without a name to render.
    userText _ Nothing = Nothing
    userText name (Just value) = parseMaybe (withObject "ready user" (\u -> u .: name)) value

-- | The parts of a gateway address the socket needs.
--
-- The address is @wss://host/path@, and the path may carry the compression
-- request the platform makes there (@?compress=zlib@).  Splitting that flag out
-- of the address is what keeps the socket and the platform from disagreeing
-- about whether frames are compressed.
data GatewayTarget = GatewayTarget
  { gtHost :: !Text,
    gtPort :: !Int,
    gtPath :: !Text,
    gtDeflate :: !Bool
  }
  deriving stock (Eq, Show)

-- | Split a gateway address, or 'Nothing' when it is not one this adapter dials.
--
-- Only @wss@ is accepted.  Max's WebSocket library speaks plain @ws@, so the
-- secure client this adapter dials with cannot do the other one; a platform that
-- answered with a plaintext address would fail at the handshake with a far less
-- obvious message than this one.
gatewayConnectTarget :: Text -> Maybe GatewayTarget
gatewayConnectTarget url = do
  rest <- T.stripPrefix "wss://" url
  let (authority, path) = T.breakOn "/" rest
      path' = if T.null path then "/" else path
  -- The platform returns a bare host.  A port here would mean the address is
  -- not the shape this adapter was written against, so refuse it rather than
  -- silently connecting somewhere else.
  if T.null authority || ":" `T.isInfixOf` authority
    then Nothing
    else
      Just
        GatewayTarget
          { gtHost = authority,
            gtPort = 443,
            gtPath = path',
            gtDeflate = "compress=zlib" `T.isInfixOf` path'
          }

-- | Identify is how a fresh session starts.  The token is presented as
-- @QQBot \<access token\>@, which is not the string the OpenAPI header carries.
--
-- The opcode envelope is part of the payload: every gateway frame is
-- @{"op": n, "d": \{…\}}@, and a frame sent without it is a frame the platform
-- does not read \u2014 which it answers by simply never establishing a session.
identifyPayload :: Text -> Int64 -> Value
identifyPayload token intents =
  object
    [ "op" .= (2 :: Int),
      "d"
        .= object
          [ "token" .= ("QQBot " <> token),
            "intents" .= intents,
            -- One shard: this adapter is a single instance and does not spread
            -- one bot across connections.
            "shard" .= ([0, 1] :: [Int]),
            "properties"
              .= object
                [ "$os" .= ("linux" :: Text),
                  "$browser" .= ("max" :: Text),
                  "$device" .= ("max" :: Text)
                ]
          ]
    ]

-- | Resume reattaches to a session the platform may still remember.  @seq@ is
-- the last dispatch sequence this process handled, and the platform replays
-- everything after it.
resumePayload :: Text -> Text -> Int64 -> Value
resumePayload token sessionId seqNo =
  object
    [ "op" .= (6 :: Int),
      "d"
        .= object
          [ "token" .= ("QQBot " <> token),
            "session_id" .= sessionId,
            "seq" .= seqNo
          ]
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
  -- An unauthorised or malformed intent is refused by identify itself, and the
  -- platform documents neither RESUME nor IDENTIFY as a way out: reconnecting
  -- with the same intents would only be closed again.
  4013 -> RecoveryFatal -- invalid intent
  4014 -> RecoveryFatal -- intent not authorised for this application
  4914 -> RecoveryFatal -- bot taken offline
  4915 -> RecoveryFatal -- bot banned
  _ -> RecoveryIdentify