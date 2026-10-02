-- | HTTPS surface of the QQ open platform: the access token, the gateway
-- address, and message sending.
--
-- Two conventions of this platform shape everything here.  First, a business
-- failure is still an HTTP 200 with an error code in the body, so an HTTP
-- status alone proves nothing and every response is inspected.  Second, an
-- access token lives two hours and is only *replaced* inside the last minute
-- of its life, so it is fetched lazily through one shared cell rather than on a
-- refresh timer.
module Max.QQOfficial.API
  ( TokenCache,
    newTokenCache,
    currentToken,
    invalidateToken,
    qqOfficialGatewayUrl,
    QQOfficialSend (..),
    QQOfficialFailure (..),
    renderQQOfficialFailure,
    classifyQQOfficialFailure,
    classifyFailureDetail,
    SendPlan (..),
    textSendPlan,
    sendQQOfficialMessage,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, withObject, (.:), (.:?), (.!=))
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Max.HttpRuntime
  ( HttpPool (StandardPool),
    HttpRuntime,
    TransportFailure (..),
    body,
    parseRequestEither,
    pathPiece,
    renderTransportFailure,
    runBuffered,
  )
import Max.Platform.Types (ConversationKind (..))
import Max.QQOfficial.Types
import Network.HTTP.Client qualified as HTTP

-- | A token plus the moment it stops being usable.
data CachedToken = CachedToken
  { cachedToken :: !Text,
    cachedTokenValidUntil :: !UTCTime
  }

newtype TokenCache = TokenCache (TVar (Maybe CachedToken))

newTokenCache :: IO TokenCache
newTokenCache = TokenCache <$> newTVarIO Nothing

invalidateToken :: TokenCache -> IO ()
invalidateToken (TokenCache ref) = atomically (writeTVar ref Nothing)

-- | A token that is good for at least one more request.
--
-- Asking again inside the replacement window returns a fresh token; asking
-- earlier returns the same one, which is why the margin below sits inside that
-- window rather than at the instant of expiry.
currentToken :: HttpRuntime -> QQOfficialConfig -> TokenCache -> IO (Either Text Text)
currentToken runtime cfg cache = do
  now <- getCurrentTime
  cached <- readTVarIO ref
  case cached of
    Just token | token.cachedTokenValidUntil > addUTCTime refreshMargin now ->
      pure (Right token.cachedToken)
    _ ->
      fetchToken runtime cfg >>= \case
        Left err -> pure (Left err)
        Right fresh -> do
          atomically (writeTVar ref (Just fresh))
          pure (Right fresh.cachedToken)
  where
    TokenCache ref = cache
    refreshMargin = fromIntegral qqOfficialTokenRefreshMarginSeconds :: NominalDiffTime

-- | Trade the app credentials for a bearer token.  The response carries no
-- HTTP status worth trusting: a bad secret arrives as 200 with @code@ 100016.
fetchToken :: HttpRuntime -> QQOfficialConfig -> IO (Either Text CachedToken)
fetchToken runtime cfg =
  apiRequest runtime cfg Nothing "POST" "/app/getAppAccessToken" (Just body) >>= \case
    Left failure -> pure (Left (renderTransportFailure failure))
    Right value -> case businessCode value of
      Just code ->
        pure . Left $
          "QQ official token rejected " <> T.pack (show code) <> ": " <> fromMaybe "" (stringField "message" value)
      Nothing -> case parseEither tokenParser value of
        Left err -> pure (Left ("QQ official token response: " <> T.pack err))
        Right (token, expires) -> do
          now <- getCurrentTime
          pure . Right $
            CachedToken
              { cachedToken = token,
                cachedTokenValidUntil = addUTCTime (fromIntegral expires) now
              }
  where
    body = object ["appId" .= cfg.qoAppId, "clientSecret" .= cfg.qoAppSecret]
    tokenParser :: Value -> Parser (Text, Int)
    tokenParser = withObject "access token" $ \o -> do
      token <- o .: "access_token" :: Parser Text
      -- Documented as "at most 7200"; a platform that reports less is obeyed
      -- rather than assumed away.
      expires <- o .:? "expires_in" .!= (7200 :: Int)
      pure (token, expires)

-- | The gateway address is discovered rather than configured: the platform
-- returns it, and it differs between the sandbox and production.
qqOfficialGatewayUrl :: HttpRuntime -> QQOfficialConfig -> TokenCache -> IO (Either Text Text)
qqOfficialGatewayUrl runtime cfg cache =
  currentToken runtime cfg cache >>= \case
    Left err -> pure (Left err)
    Right token ->
      apiRequest runtime cfg (Just token) "GET" ("/gateway/bot" :: Text) Nothing >>= \case
        Left failure -> pure (Left (renderTransportFailure failure))
        Right value -> case parseEither (withObject "gateway" (\o -> o .: "url" :: Parser Text)) value of
          Left err -> pure (Left ("QQ official gateway response: " <> T.pack err))
          Right url -> pure (Right url)

-- | What one accepted send reports back.
data QQOfficialSend = QQOfficialSend
  { sentMessageId :: !Text,
    -- | @ext_info.ref_idx@: the only id this platform accepts when the message
    -- is quoted later.  The response's own @id@ is what a *deletion* needs, and
    -- the two are different namespaces for the same message.
    sentRefIndex :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | One planned send.  The field set is the platform's: @msg_type@ picks which
-- content field is live, @msg_id@ makes the send a passive reply inside its
-- five-minute window, and @msg_seq@ is what keeps two answers to the same
-- message from colliding (err_code 40054005).
data SendPlan = SendPlan
  { planMsgType :: !Int,
    planContent :: !(Maybe Text),
    planMediaFileInfo :: !(Maybe Text),
    planReference :: !(Maybe Text),
    planMsgId :: !(Maybe Text),
    planMsgSeq :: !(Maybe Int),
    planIsWakeup :: !Bool
  }
  deriving stock (Eq, Show)

textSendPlan :: Text -> Maybe Text -> Maybe Int -> SendPlan
textSendPlan content reference msgSeq =
  SendPlan
    { planMsgType = 0,
      planContent = Just content,
      planMediaFileInfo = Nothing,
      planReference = reference,
      planMsgId = Nothing,
      planMsgSeq = msgSeq,
      planIsWakeup = False
    }

data QQOfficialFailure
  = -- | The request never reached the platform, or its reply never came back.
    -- Whether it took effect is not knowable, and this platform offers no
    -- idempotency key that would make asking again safe.
    QQOfficialTransport !TransportFailure
  | -- | The platform answered, with a business error code.
    QQOfficialRejected !Int !Text
  | -- | A success status whose body was not the documented shape.
    QQOfficialMalformed !Text
  deriving stock (Eq, Show)

renderQQOfficialFailure :: QQOfficialFailure -> Text
renderQQOfficialFailure = \case
  QQOfficialTransport failure -> "QQ official transport: " <> renderTransportFailure failure
  QQOfficialRejected code message ->
    "QQ official rejected " <> T.pack (show code) <> (if T.null message then "" else ": " <> message)
  QQOfficialMalformed detail -> "QQ official response: " <> detail

-- | Whether a second attempt is safe.  Only a failure that provably happened
-- before the platform could accept the send qualifies; everything else is
-- outcome-unknown, because a retry would be a second message in the group.
classifyQQOfficialFailure :: QQOfficialFailure -> Bool
classifyQQOfficialFailure = \case
  QQOfficialTransport failure -> classifyFailureDetail failure
  QQOfficialRejected code _ -> code `elem` retryableErrCodes
  QQOfficialMalformed _ -> False

-- | The same question asked of a bare transport failure.
--
-- A rate-limit refusal is the one response that is both "received" and
-- "certainly not acted on".  Timeouts, TLS failures and 5xx bodies are not: the
-- send may already be in the group.
classifyFailureDetail :: TransportFailure -> Bool
classifyFailureDetail = \case
  RequestConstructionFailure _ -> True
  ConnectionTimeoutFailure -> True
  ConnectionFailed _ -> True
  ProxyFailure _ -> True
  HttpStatusFailure code _ _ _ -> code == 429
  _ -> False

-- | Server-side error codes that explicitly mean "nothing happened, try later".
retryableErrCodes :: [Int]
retryableErrCodes =
  [ 304018, -- SESSION_NOT_EXIST: the bot has no gateway connection
    304022, -- PUSH_TIME
    40034100, -- 主动消息发送超过频控限制
    50055001, -- 消息发送异常，请稍后重试
    50055006, -- ARK 消息发送异常
    40054005 -- 消息被去重: the same msg_id + msg_seq, answered with the next seq
  ]

-- | Send one message.  @openid@ is the destination: a @group_openid@ for a
-- group, a @user_openid@ for a one-to-one chat.  The two use different routes
-- and their uploaded media is not interchangeable.
sendQQOfficialMessage ::
  HttpRuntime ->
  QQOfficialConfig ->
  TokenCache ->
  ConversationKind ->
  -- | @msg_id@ of the message being answered, when it is still inside the
  -- passive window.
  Maybe Text ->
  Text ->
  SendPlan ->
  IO (Either QQOfficialFailure QQOfficialSend)
sendQQOfficialMessage runtime cfg cache kind mMsgId openid plan =
  currentToken runtime cfg cache >>= \case
    Left err -> pure (Left (QQOfficialTransport (ProtocolFailure err)))
    Right token ->
      apiRequest runtime cfg (Just token) "POST" (sendPath kind openid) (Just (renderPlan mMsgId plan)) >>= \case
        Left failure -> pure (Left (QQOfficialTransport failure))
        Right value -> case businessCode value of
          Just code ->
            pure . Left $
              QQOfficialRejected code (fromMaybe "" (stringField "message" value))
          Nothing -> case parseEither sendParser value of
            Left err -> pure (Left (QQOfficialMalformed (T.pack err)))
            Right send -> pure (Right send)

sendPath :: ConversationKind -> Text -> Text
sendPath kind openid = case kind of
  ConversationGroup -> "/v2/groups/" <> pathPiece openid <> "/messages"
  ConversationDirect -> "/v2/users/" <> pathPiece openid <> "/messages"

sendParser :: Value -> Parser QQOfficialSend
sendParser = withObject "send response" $ \o -> do
  messageId <- o .:? "id" .!= ("" :: Text)
  reference <- o .:? "ext_info" >>= \case
    Nothing -> pure Nothing
    Just info -> pure (stringField "ref_idx" info)
  pure (QQOfficialSend messageId reference)

renderPlan :: Maybe Text -> SendPlan -> Value
renderPlan mMsgId plan =
  object $
    [ "msg_type" .= planMsgType plan
    , "is_wakeup" .= planIsWakeup plan
    ]
      <> ["content" .= content | Just content <- [planContent plan]]
      <> ["media" .= object ["file_info" .= fileInfo] | Just fileInfo <- [planMediaFileInfo plan]]
      <> ["message_reference" .= object ["message_id" .= reference] | Just reference <- [planReference plan]]
      <> ["msg_id" .= messageId | Just messageId <- [mMsgId]]
      <> ["msg_seq" .= msgSeq | Just msgSeq <- [planMsgSeq plan]]

-- | One HTTPS round trip against the open platform.
--
-- Every 2xx body is decoded here, including the failures: this API reports a
-- rejected send with HTTP 200 and an error code, so a status check would call
-- every refusal a success.
apiRequest ::
  HttpRuntime ->
  QQOfficialConfig ->
  Maybe Text ->
  BS.ByteString ->
  Text ->
  Maybe Value ->
  IO (Either TransportFailure Value)
apiRequest runtime cfg mToken method path payload = do
  let url = qqOfficialApiBase cfg <> path
  parseRequestEither (T.unpack url) >>= \case
    Left failure -> pure (Left failure)
    Right request0 -> do
      let request =
            request0
              { HTTP.method = method,
                HTTP.requestHeaders =
                  ("Content-Type", "application/json")
                    : [("Authorization", "QQBot " <> TE.encodeUtf8 token) | Just token <- [mToken]],
                HTTP.requestBody = maybe (HTTP.RequestBodyBS "") (HTTP.RequestBodyLBS . encode) payload,
                HTTP.responseTimeout = HTTP.responseTimeoutMicro qqOfficialHttpTimeoutMicros
              }
      runBuffered runtime StandardPool qqOfficialMaxResponseBytes qqOfficialStatusPreviewBytes request >>= \case
        Left failure -> pure (Left failure)
        Right response -> case eitherDecodeStrict' response.body of
          Left err -> pure (Left (ProtocolFailure ("QQ official JSON: " <> T.pack err)))
          Right value -> pure (Right value)

-- | The platform's success flag.  @err_code@ is the OpenAPI-wide field and
-- @0@ means success; the token endpoint reports its failures as @code@ instead,
-- where any value at all is a failure.
businessCode :: Value -> Maybe Int
businessCode = \case
  Object fields ->
    case KeyMap.lookup "err_code" fields of
      Just (Number n) -> nonZeroCode (truncate n :: Int)
      _ -> case KeyMap.lookup "code" fields of
        Just (Number n) -> nonZeroCode (truncate n :: Int)
        _ -> Nothing
  _ -> Nothing

nonZeroCode :: Int -> Maybe Int
nonZeroCode 0 = Nothing
nonZeroCode code = Just code

stringField :: Text -> Value -> Maybe Text
stringField name (Object fields) = case KeyMap.lookup (Key.fromText name) fields of
  Just (String value) -> Just value
  _ -> Nothing
stringField _ _ = Nothing