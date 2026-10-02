-- | The QQ open platform's own bot, as one more platform Max speaks.
--
-- The shape of this adapter is dictated by the platform rather than by
-- convenience:
--
-- * It dials out.  Max connects to the gateway and to the OpenAPI, so a
--   deployment needs no public address and no inbound callback — the reverse of
--   the OneBot edge, where a protocol end on the far side connects to us.
-- * A session is resumable.  Every dispatch carries a sequence number and a
--   resumed connection replays everything after the last one this process
--   handled, so recovery is a cursor rather than a message-history sweep.
-- * Answers are rationed.  A group message may be answered five times inside
--   five minutes and a one-to-one chat four times inside an hour; past that the
--   platform refuses.  Max plans a reply in as many chunks as the content
--   deserves, so the wire parts are folded to that budget, and an answer that
--   arrives too late is re-sent as an ordinary message instead of being lost.
--
-- Media is deliberately declared on the text tier for now.  Rich media needs a
-- two-step upload for a short-lived @file_info@ whose request shapes are worth
-- exercising against a live bot before an adapter depends on them; until then a
-- picture folds to readable text rather than being advertised and dropped.
module Max.QQOfficial
  ( QQOfficialRuntime (..),
    newQQOfficialRuntime,
    qqOfficialWorker,
    qqOfficialDeliveryTransport,
    qqOfficialLegacyId,
    resolveQQOfficialOwners,
    PassiveWindow (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (fromException)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (Value, encode)
import Data.Aeson.Types (Parser, parseMaybe, withObject, (.:))
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Effectful
import Effectful.Log
import Effectful.PostgreSQL (WithConnection)
import Max.DB.PlatformIds (compatibilityId, compatibilityIdForDirectChat)
import qualified Max.DB.PlatformIds as PlatformIds
import Max.EpisodeScheduler (EpisodeScheduler, bumpEpisode)
import Max.HttpRuntime (HttpRuntime)
import Max.IR (Body (..))
import Max.IR.Digest (digest)
import Max.IR.Lower (LoweredMessage (..))
import Max.Platform.Delivery
  ( DeliveryOperation (..),
    DeliveryTransport (..),
    fanOutMediaChunks,
    loweredText,
  )
import Max.Platform.Delivery.Parts
import Max.Platform.Envelope (InboundEnvelope (..), IngestClass (..))
import Max.Platform.Ingress (Ingress, queueIngest)
import Max.Platform.Store.Delivery (DeliveryRequest (..))
import Max.Platform.Store.Endpoint
  ( RegisteredEndpoint (..),
    ensureConfiguredEndpoint,
    ensurePlatformAccount,
  )
import Max.Platform.Store.Ingest
  ( CursorRecord (..),
    IngestOptions (..),
    IngestResult (..),
    NewIngest (..),
    advanceIngestCursorCAS,
    defaultIngestOptions,
    ingestEnvelope,
    readIngestCursor,
  )
import Max.Platform.Types
import Max.QQOfficial.API
import Max.QQOfficial.Events
import Max.QQOfficial.Gateway
import Max.QQOfficial.Types
import Max.Util (trySyncIO)
import Network.WebSockets
  ( CompressionOptions (NoCompression, PermessageDeflateCompression),
    ConnectionOptions (connectionCompressionOptions),
  )
import Network.WebSockets qualified as WS
import OneBot.Types (GroupId (..))
import System.Timeout (timeout)
import Wuss qualified as Wuss

--------------------------------------------------------------------------------
-- Runtime state
--------------------------------------------------------------------------------

-- | A message this process received that can still be answered passively.
--
-- Both ids matter and they name different things: the platform's @msg_id@ is
-- what a passive answer must quote, and the reference index is what a *visible*
-- quote must name.  The arrival time decides whether either is still valid.
data PassiveWindow = PassiveWindow
  { passiveMsgId :: !Text,
    passiveReceivedAt :: !UTCTime
  }

data QQOfficialRuntime = QQOfficialRuntime
  { qorHttpRuntime :: !HttpRuntime,
    qorConfig :: !QQOfficialConfig,
    qorTokenCache :: !TokenCache,
    -- | Reference index to the window in which its message can still be
    -- answered.  Bounded by age: an entry older than any passive window is
    -- useless, so it is dropped rather than kept.
    qorPassives :: !(TVar (Map NativeEventId PassiveWindow)),
    -- | Last sequence number used for a given @msg_id@, with the moment it was
    -- used.  Two answers to the same message must carry different sequence
    -- numbers (err_code 40054005), and the table is bounded by that same age.
    qorMsgSeq :: !(TVar (Map Text (Int, UTCTime))),
    -- | The bot's own identity as the gateway reports it, learned at READY.
    qorContext :: !(TVar QQOfficialContext)
  }

newQQOfficialRuntime :: HttpRuntime -> QQOfficialConfig -> IO QQOfficialRuntime
newQQOfficialRuntime runtime cfg = do
  cache <- newTokenCache
  passives <- newTVarIO Map.empty
  msgSeq <- newTVarIO Map.empty
  context <- newTVarIO (initialContext cfg)
  pure
    QQOfficialRuntime
      { qorHttpRuntime = runtime,
        qorConfig = cfg,
        qorTokenCache = cache,
        qorPassives = passives,
        qorMsgSeq = msgSeq,
        qorContext = context
      }

initialContext :: QQOfficialConfig -> QQOfficialContext
initialContext cfg =
  QQOfficialContext
    { ctxSelfId = cfg.qoAppId,
      ctxSelfLabel = if T.null (T.strip cfg.qoBotName) then "机器人" else cfg.qoBotName,
      ctxSelfIds = [cfg.qoAppId]
    }

-- | The synthetic conversation id for one open platform conversation.
--
-- A group keeps its raw synthetic id, which lands at or below @-10^12@ and is
-- therefore read as a group everywhere.  A one-to-one chat is re-banded into the
-- interval 'isPrivateChat' accepts, because a chat that kept its raw synthetic
-- id would be handled as a room by every caller that asks the sign — the
-- permission tier, the prompt's framing, the memory scope, the @!@ commands.
qqOfficialLegacyId :: (WithConnection :> es, IOE :> es) => ConversationKind -> Text -> Eff es Int64
qqOfficialLegacyId kind native = case kind of
  ConversationGroup -> compatibilityId platformName "channel" native
  ConversationDirect -> compatibilityIdForDirectChat platformName "channel" native
  where
    platformName = renderPlatform PlatformQQOfficial

-- | Owner openids as the numeric ids Max authorizes.
--
-- The message store keys @user_id@ with @compatibilityId "qqofficial" "user"@,
-- so this is the same lookup the ingest path performs; an operator configures
-- the openid the platform shows them and never the number behind it.
resolveQQOfficialOwners :: (WithConnection :> es, IOE :> es) => QQOfficialConfig -> Eff es [Int64]
resolveQQOfficialOwners cfg =
  traverse (PlatformIds.mappedId (renderPlatform PlatformQQOfficial) "user") cfg.qoOwners

--------------------------------------------------------------------------------
-- Ingress
--------------------------------------------------------------------------------

-- | One gateway session: connect, identify or resume, then read until the socket
-- ends.  Returns the resume point the next attempt should use.
qqOfficialWorker ::
  (WithConnection :> es, Log :> es, IOE :> es) =>
  QQOfficialRuntime ->
  Maybe EpisodeScheduler ->
  Ingress ->
  Eff es ()
qqOfficialWorker runtime scheduler ingress = localDomain "qqofficial" $ do
  let cfg = qorConfig runtime
  account <- ensurePlatformAccount PlatformQQOfficial (NativeAccountId cfg.qoAppId) qqOfficialCapabilities
  stored <- readIngestCursor account qqOfficialStreamKey
  logInfo "qqofficial worker started" $
    object
      [ "app_id" .= cfg.qoAppId,
        "api_base" .= qqOfficialApiBase cfg,
        "sandbox" .= cfg.qoSandbox,
        "intents" .= cfg.qoIntents,
        -- Recorded, not requested: whether every group message arrives is the
        -- group's own setting, not something identify can ask for.  The bot
        -- asks for the intent bit either way and the platform decides.
        "full_group_messages" .= cfg.qoFullGroupMessages,
        "resume_from" .= resumeFromCursor stored
      ]
  let go current = do
        outcome <- runSession runtime account current scheduler ingress
        case outcome of
          SessionEnded next -> do
            liftIO (threadDelay reconnectDelayMicros)
            go next
          SessionFatal code -> do
            -- Reconnecting to a bot that has been taken offline or banned looks
            -- like an attack and will never succeed.  Stay up, stay quiet.
            logAttention "qqofficial gateway closed permanently; not reconnecting" $
              object ["close_code" .= code, "app_id" .= cfg.qoAppId]
            forever (liftIO (threadDelay fatalIdleMicros))
  go (resumeFromCursor stored)

-- | Whether this session ended and how to pick the next one up.
data SessionOutcome
  = SessionEnded !(Maybe (Text, Int64))
  | SessionFatal !Int

-- | Nothing stored means a fresh Identify, which is also what an unusable
-- session id degrades to.
resumeFromCursor :: Maybe CursorRecord -> Maybe (Text, Int64)
resumeFromCursor = \case
  Nothing -> Nothing
  Just record -> case record.cursor of
    PlatformCursor value -> parseMaybe (withObject "qqofficial cursor" point) value
    where
      point o = (,) <$> o .: "session_id" <*> o .: "seq"

encodeCursor :: Text -> Int64 -> Value
encodeCursor session seqNo = object ["session_id" .= session, "seq" .= seqNo]

runSession ::
  (WithConnection :> es, Log :> es, IOE :> es) =>
  QQOfficialRuntime ->
  PlatformAccountId ->
  Maybe (Text, Int64) ->
  Maybe EpisodeScheduler ->
  Ingress ->
  Eff es SessionOutcome
runSession runtime account resume scheduler ingress = do
  let cfg = qorConfig runtime
      http = qorHttpRuntime runtime
      cache = qorTokenCache runtime
  gatewayUrl <- liftIO (qqOfficialGatewayUrl http cfg cache)
  case gatewayUrl of
    Left err -> unavailable err
    Right url -> case gatewayConnectTarget url of
      Nothing -> unavailable ("unusable QQ official gateway address: " <> url)
      Just target -> do
        token <- liftIO (currentToken http cfg cache)
        case token of
          Left err -> unavailable err
          Right accessToken -> do
            -- The gateway is @wss@, and the WebSocket library Max already uses
            -- speaks plain @ws@ only, so the secure client comes from Wuss,
            -- which wraps the same connection type.
            connected <-
              liftIO
                ( trySyncIO
                    (Wuss.newSecureClientConnectionWith
                      (T.unpack target.gtHost)
                      (fromIntegral target.gtPort)
                      (T.unpack target.gtPath)
                      (gatewayOptions target)
                      []
                    )
                )
            case connected of
              Left err -> unavailable ("connect: " <> T.pack (show err))
              Right (conn, finish) -> do
                outcome <-
                  do
                    handshaken <- liftIO (handshake cfg accessToken resume conn)
                    case handshaken of
                      Left err -> unavailable ("handshake: " <> err)
                      Right () -> receiveLoop runtime account resume scheduler ingress conn
                -- Wuss hands back the connection with the action that closes the
                -- channel it opened; the socket is not closed until that runs.
                _ <- liftIO (trySyncIO finish)
                pure outcome
  where
    unavailable err = do
      logAttention "qqofficial gateway unavailable" $ object ["error" .= err]
      pure (SessionEnded resume)

-- | Socket options for one gateway address.
--
-- Compression is not a preference: the platform states in the address whether it
-- will send deflated frames, so the socket answers the same way rather than
-- negotiating.
gatewayOptions :: GatewayTarget -> WS.ConnectionOptions
gatewayOptions target =
  WS.defaultConnectionOptions
    { connectionCompressionOptions =
        if target.gtDeflate
          then PermessageDeflateCompression WS.defaultPermessageDeflate
          else NoCompression
    }

-- | Identify starts a session; Resume reattaches to one the platform still
-- remembers and asks it to replay what was missed.
handshake ::
  QQOfficialConfig -> Text -> Maybe (Text, Int64) -> WS.Connection -> IO (Either Text ())
handshake cfg accessToken resume conn = case resume of
  Just (session, seqNo) -> send (resumePayload accessToken session seqNo)
  Nothing -> send (identifyPayload accessToken cfg.qoIntents)
  where
    send payload =
      trySyncIO (WS.sendTextData conn (encode payload)) >>= \case
        Left err -> pure (Left ("send: " <> T.pack (show err)))
        Right () -> pure (Right ())

data LoopControl = ContinueLoop | StopLoop !SessionOutcome

-- | The read side of one session.
--
-- Every blocking call is wrapped, so a dead socket produces a value rather than
-- an exception and the socket is always closed afterwards.
receiveLoop ::
  (WithConnection :> es, Log :> es, IOE :> es) =>
  QQOfficialRuntime ->
  PlatformAccountId ->
  Maybe (Text, Int64) ->
  Maybe EpisodeScheduler ->
  Ingress ->
  WS.Connection ->
  Eff es SessionOutcome
receiveLoop runtime account resume scheduler ingress conn = do
  intervalRef <- liftIO (newTVarIO defaultHeartbeatMillis)
  seqRef <- liftIO (newTVarIO (snd <$> resume))
  -- The resume point this session has reached, whatever the next attempt needs.
  -- The cell is layered: outside is whether anything was learned at all, inside
  -- is the point, which is what @fromMaybe@ reads.
  pointRef <- liftIO (newTVarIO (Just resume))
  pump <- liftIO (async (heartbeatLoop conn intervalRef seqRef))
  -- The loop reads the cells the do block above created, so it is bound with
  -- @let@ rather than @where@: a @where@ clause cannot see them.
  let loop = do
        received <- liftIO (receiveFrame conn)
        case received of
          Right Nothing -> loop
          Right (Just frame) -> handleFrame runtime account resume scheduler ingress conn intervalRef seqRef pointRef frame >>= \case
            ContinueLoop -> loop
            StopLoop outcome -> pure outcome
          Left (GatewayClosed code) -> do
            logInfo "qqofficial gateway closed" $ object ["close_code" .= code]
            case closeRecovery code of
              RecoveryResume -> do
                point <- liftIO (readTVarIO pointRef)
                pure (SessionEnded (fromMaybe resume point))
              RecoveryIdentify -> pure (SessionEnded Nothing)
              RecoveryFatal -> pure (SessionFatal code)
          Left (GatewayFailed err) -> do
            -- A dropped socket is the ordinary case for a long-lived connection;
            -- the resume point is what makes the next attempt lossless.
            logAttention "qqofficial gateway read failed" $ object ["error" .= err]
            point <- liftIO (readTVarIO pointRef)
            pure (SessionEnded (fromMaybe resume point))
  outcome <- loop
  liftIO (cancel pump)
  pure outcome

data GatewayEnd
  = -- | The peer closed with a code.
    GatewayClosed !Int
  | -- | The socket failed, or a frame could not be read.
    GatewayFailed !Text

-- | Wait for one frame.
--
-- A parse failure is dropped rather than fatal: an unrecognised frame is not
-- something this adapter can act on, and dropping it keeps a sequence gap that
-- the platform's own replay can close.
receiveFrame :: WS.Connection -> IO (Either GatewayEnd (Maybe GatewayFrame))
receiveFrame conn = do
  attempt <- trySyncIO (timeout idleWaitMicros (WS.receiveData conn :: IO LBS.ByteString))
  case attempt of
    Left err -> pure (classifyRead err)
    -- Nothing means the platform said nothing for a whole interval.  That is not
    -- a failure: the heartbeat still runs and the next read waits again.
    Right Nothing -> pure (Right Nothing)
    Right (Just payload) -> pure $ case parseGatewayFrame (LBS.toStrict payload) of
      Right frame -> Right (Just frame)
      Left _ -> Right Nothing
  where
    -- The library owns the close handshake, so a close arrives as an exception
    -- rather than as a value; its code is what decides how the next attempt
    -- picks the session up.
    classifyRead err = case fromException err of
      Just (WS.CloseRequest code _) -> Left (GatewayClosed (fromIntegral code))
      _ -> Left (GatewayFailed (T.pack (show err)))

-- | Heartbeat on the period the gateway asked for, carrying the last sequence
-- number this connection handled.
heartbeatLoop :: WS.Connection -> TVar Int -> TVar (Maybe Int64) -> IO ()
heartbeatLoop conn intervalRef seqRef = forever $ do
  millis <- readTVarIO intervalRef
  threadDelay (millis * 1000)
  seqNo <- readTVarIO seqRef
  void (trySyncIO (WS.sendTextData conn (encode (heartbeatPayload seqNo))))

handleFrame ::
  (WithConnection :> es, Log :> es, IOE :> es) =>
  QQOfficialRuntime ->
  PlatformAccountId ->
  -- | Where this session started, used when the session has not moved the point
  -- any further and a close still has to name where to resume from.
  Maybe (Text, Int64) ->
  Maybe EpisodeScheduler ->
  Ingress ->
  WS.Connection ->
  TVar Int ->
  TVar (Maybe Int64) ->
  TVar (Maybe (Maybe (Text, Int64))) ->
  GatewayFrame ->
  Eff es LoopControl
handleFrame runtime account resume scheduler ingress conn intervalRef seqRef pointRef frame = case frame.frameOp of
  10 -> do
    liftIO (atomically (writeTVar intervalRef (fromMaybe defaultHeartbeatMillis frame.frameHeartbeatMillis)))
    pure ContinueLoop
  11 -> pure ContinueLoop -- heartbeat acknowledged
  1 -> do
    -- The platform asked for a heartbeat rather than sending one; answer with
    -- the last sequence number seen.
    seqNo <- liftIO (readTVarIO seqRef)
    void (liftIO (trySyncIO (WS.sendTextData conn (encode (heartbeatPayload seqNo)))))
    pure ContinueLoop
  7 -> do
    -- Reconnect requested: this socket is finished, and the resume point is
    -- what the next one continues from.
    point <- liftIO (readTVarIO pointRef)
    pure (StopLoop (SessionEnded (fromMaybe resume point)))
  9 -> pure (StopLoop (SessionEnded Nothing)) -- invalid session: identify again
  0 -> dispatchFrame
  _ -> pure ContinueLoop
  where
    dispatchFrame = case (frame.frameType, frame.frameSeq) of
      (Just "READY", _) -> handleReady
      (Just "RESUMED", _) -> do
        logInfo "qqofficial gateway session resumed" $ object ["seq" .= frame.frameSeq]
        liftIO (atomically (writeTVar seqRef frame.frameSeq))
        recordPoint runtime account Nothing frame.frameSeq
        pure ContinueLoop
      (Just name, seqNo)
        | qqOfficialMessageEvent name -> do
            ctx <- liftIO (readTVarIO (qorContext runtime))
            ingestMessage runtime scheduler ingress ctx name frame.frameData
            liftIO (atomically (writeTVar seqRef seqNo))
            recordPoint runtime account Nothing seqNo
            pure ContinueLoop
      _ -> do
        -- Every dispatch advances the point, including the ones this adapter
        -- ignores: a resume must not replay them.
        liftIO (atomically (writeTVar seqRef frame.frameSeq))
        recordPoint runtime account Nothing frame.frameSeq
        pure ContinueLoop

    handleReady = case readySession frame.frameData of
      Nothing -> do
        logAttention "qqofficial ready frame carried no session" $ object ["seq" .= frame.frameSeq]
        pure ContinueLoop
      Just (session, userId, username) -> do
        liftIO . atomically $ do
          writeTVar seqRef frame.frameSeq
          writeTVar pointRef (Just (Just (session, fromMaybe 0 frame.frameSeq)))
        when (isJust userId) (liftIO (updateContext runtime userId username))
        recordPoint runtime account (Just session) frame.frameSeq
        logInfo "qqofficial gateway ready" $
          object ["session" .= session, "seq" .= frame.frameSeq, "bot" .= username]
        pure ContinueLoop

-- | Adopt the bot's own identity as the gateway reports it.  Messages sent by
-- the bot are recognised by matching this id or the application id, and Max
-- renders a self-mention with the platform's own name for the bot.
updateContext :: QQOfficialRuntime -> Maybe Text -> Maybe Text -> IO ()
updateContext runtime userId username = do
  current <- readTVarIO (qorContext runtime)
  -- The bot identifies itself by the open platform's user id, which is not the
  -- application id; both are accepted so an echo matches either way.
  let cfgId = ctxSelfId current
      selfIds = maybe (ctxSelfIds current) (\value -> [self | self <- [cfgId, value], not (T.null self)]) userId
  atomically $
    writeTVar
      (qorContext runtime)
      current
        { ctxSelfIds = selfIds,
          ctxSelfLabel = fromMaybe (ctxSelfLabel current) username
        }

-- | Persist the resume point.
--
-- A session id that has not changed yet is kept, because only a ready frame
-- introduces one.  A lost compare-and-swap means another writer won; the replay
-- then deduplicates, which is exactly what the cursor contract promises.
recordPoint ::
  (WithConnection :> es, Log :> es, IOE :> es) =>
  QQOfficialRuntime ->
  PlatformAccountId ->
  Maybe Text ->
  Maybe Int64 ->
  Eff es ()
recordPoint runtime account session seqNo = do
  current <- readIngestCursor account qqOfficialStreamKey
  let stored = sessionOf current
      session' = fromMaybe stored session
      seq' = fromMaybe 0 seqNo
  published <-
    advanceIngestCursorCAS
      account
      qqOfficialStreamKey
      ((.revision) <$> current)
      (PlatformCursor (encodeCursor session' seq'))
      (Just (sourceFingerprint (qorConfig runtime)))
  when (isNothing published) $
    logInfo "qqofficial cursor CAS lost; replay will deduplicate" $
      object ["session" .= session', "seq" .= seq']
  where
    sessionOf = \case
      Nothing -> ""
      Just record -> case record.cursor of
        PlatformCursor value ->
          fromMaybe "" (parseMaybe (withObject "qqofficial cursor" (\o -> o .: "session_id" :: Parser Text)) value)

ingestMessage ::
  (WithConnection :> es, Log :> es, IOE :> es) =>
  QQOfficialRuntime ->
  Maybe EpisodeScheduler ->
  Ingress ->
  QQOfficialContext ->
  Text ->
  Value ->
  Eff es ()
ingestMessage runtime scheduler ingress ctx name payload = case qqOfficialEvent ctx name payload of
  Left err -> logAttention "qqofficial event not ingested" $ object ["event" .= name, "error" .= err]
  Right event -> do
    let cfg = qorConfig runtime
    legacy <- qqOfficialLegacyId event.qoeKind event.qoeConversationNative
    registered <-
      ensureConfiguredEndpoint
        PlatformQQOfficial
        (NativeAccountId cfg.qoAppId)
        (NativeConversationId event.qoeConversationNative)
        event.qoeKind
        EndpointStandalone
        (Just legacy)
        qqOfficialCapabilities
    received <- liftIO getCurrentTime
    -- A message the bot sent itself is a delivery echo at best; recording a
    -- passive window for one would let Max answer itself.
    unless (event.qoeSenderIsSelf) $
      liftIO (rememberWindow runtime event.qoeNativeEventId (PassiveWindow event.qoeMessageId received))
    forM_ scheduler $ \sched -> liftIO (bumpEpisode sched (GroupId legacy))
    let -- Our own messages carry the application id as their sender, which is
        -- exactly what the store compares against to recognise a delivery echo.
        sender
          | event.qoeSenderIsSelf = NativeUserId cfg.qoAppId
          | otherwise = NativeUserId event.qoeSenderNative
        envelope =
          InboundEnvelope
            { endpointId = registered.endpointId,
              nativeEventId = event.qoeNativeEventId,
              senderNativeId = sender,
              senderDisplayName = event.qoeSenderDisplay,
              occurredAt = event.qoeOccurredAt,
              receivedAt = received,
              eventKind = EventMessage,
              ingestClass = LiveDelivery,
              content = Body event.qoeContent,
              relations = event.qoeRelations,
              sourceCursor = Nothing,
              rawPayload = Just event.qoeRaw
            }
    result <- ingestEnvelope defaultIngestOptions {selfEventsAreEchoes = True} envelope
    liftIO (queueIngest ingress result)
    case result of
      Ingested fresh ->
        logInfo "qqofficial event ingested" $
          object
            [ "event" .= event.qoeEventName,
              "native_event_id" .= event.qoeNativeEventId,
              "canonical_message_id" .= fresh.canonicalMessageId,
              "content" .= digest fresh.canonicalBody
            ]
      AlreadyIngested {} -> pure ()
      DeliveryEcho {} -> pure ()
      EchoUnmatched -> pure ()

-- | Remember that a message can still be answered.
--
-- Age is the natural bound and the only one needed: an entry older than any
-- passive window can never be answered again, so both tables are pruned by time
-- instead of growing with the traffic.
rememberWindow :: QQOfficialRuntime -> NativeEventId -> PassiveWindow -> IO ()
rememberWindow runtime native window = do
  now <- getCurrentTime
  let cutoff = addUTCTime (negate passiveRetentionSeconds) now
      fresh entry = entry > cutoff
  atomically $ do
    entries <- readTVar (qorPassives runtime)
    writeTVar (qorPassives runtime) (Map.insert native window (Map.filter (fresh . (.passiveReceivedAt)) entries))
    numbers <- readTVar (qorMsgSeq runtime)
    writeTVar (qorMsgSeq runtime) (Map.filter (fresh . snd) numbers)

--------------------------------------------------------------------------------
-- Delivery
--------------------------------------------------------------------------------

-- | One planned wire part: the text as this platform may carry it, plus what the
-- answer needs to be attributable to the message it answers.
data OutgoingPart = OutgoingPart
  { partText :: !Text,
    -- | Reference index of the message being quoted, for the first part only.
    partReference :: !(Maybe Text),
    partWindow :: !(Maybe PassiveWindow)
  }

qqOfficialDeliveryTransport :: QQOfficialRuntime -> DeliveryTransport
qqOfficialDeliveryTransport runtime =
  DeliveryTransport
    { platform = PlatformQQOfficial,
      deliver = \journal claim -> \case
        DeliverMessage lowered -> deliverChunks journal claim lowered
        -- Reached only if the declared capabilities and this transport ever
        -- disagree; say so rather than pretending the message went out.
        DeliverEdit {} -> pure (AttemptPermanentlyFailed "QQ官方机器人不能编辑消息")
        DeliverReaction {} -> pure (AttemptSuppressed "QQ官方机器人没有表情表态")
        DeliverRedaction _ -> pure (AttemptSuppressed "QQ官方机器人的撤回窗口只有两分钟，未启用")
    }
  where
    deliverChunks journal claim lowered = do
      let kind = qqOfficialKindOfLegacyId claim.compatibilityConversationId
          openid = unNativeConversationId claim.nativeConversationId
          chunks = mergeChunksToBudget (qqOfficialReplyPartBudget kind) (fanOutMediaChunks lowered.chunks)
      window <- passiveWindowFor runtime kind lowered.replyNative
      prepareParts (planPart kind window lowered.replyNative) chunks >>= \case
        Left err -> pure (AttemptPermanentlyFailed err)
        Right payloads ->
          runDeliveryParts
            journal
            -- Not idempotent: the platform offers no key that would make a
            -- repeated send recognisable as the same message.
            NonIdempotentParts
            AttemptConfirmed
            [ wireFingerprint (if i == 0 then lowered.replyNative else Nothing) chunk
            | (i, chunk) <- zip [0 :: Int ..] chunks
            ]
            payloads
            (sendPart runtime kind openid)

    -- The visible quote belongs to the first part only: repeating it would make
    -- the platform render every paragraph as a quote of the same line.
    planPart kind window replyTarget index chunk = case loweredText chunk of
      Left err -> pure (Left err)
      Right body ->
        let -- A URL fails a group message outright, and Max produces URLs
            -- constantly, so they are replaced rather than sent.  The
            -- replacement is visible in the message the group sees, which is
            -- where this degradation belongs: this is a platform rule, not a
            -- loss in the ledger.
            (text, _replaced) =
              if kind == ConversationGroup then stripOutboundUrls body else (body, 0)
         in pure . Right $
              OutgoingPart
                { partText = text,
                  partReference = if index == 0 then unNativeEventId <$> replyTarget else Nothing,
                  partWindow = if index == 0 then window else Nothing
                }

    sendPart _runtime kind openid _index part = do
      -- The sequence number is claimed once per part and only when there is a
      -- window to answer inside: two answers to one message must differ.
      passive <- case part.partWindow of
        Nothing -> pure Nothing
        Just window -> Just . (window.passiveMsgId,) <$> nextMsgSeq runtime window.passiveMsgId
      attempt <- sendOnce runtime kind openid part passive
      case attempt of
        Right send -> pure (AttemptConfirmed (NativeEventId <$> send.sentRefIndex))
        Left failure -> case (part.partWindow, failure) of
          -- The window closed between planning and sending.  The content is
          -- still wanted; say it as an ordinary message instead of dropping it.
          (Just _, QQOfficialRejected code _)
            | code `elem` passiveWindowExpiredCodes ->
                sendOnce runtime kind openid part Nothing >>= \case
                  Right send -> pure (AttemptConfirmed (NativeEventId <$> send.sentRefIndex))
                  Left retryFailure -> pure (attemptFor retryFailure)
          _ -> pure (attemptFor failure)

    -- Only a failure that provably happened before the platform could accept
    -- the send may be retried; everything else might already be in the group.
    attemptFor failure
      | classifyQQOfficialFailure failure = AttemptRetryable (renderQQOfficialFailure failure)
      | otherwise = case failure of
          QQOfficialTransport _ -> AttemptOutcomeUnknown (renderQQOfficialFailure failure)
          QQOfficialRejected _ _ -> AttemptRejected (renderQQOfficialFailure failure)
          QQOfficialMalformed _ -> AttemptPermanentlyFailed (renderQQOfficialFailure failure)

sendOnce ::
  QQOfficialRuntime ->
  ConversationKind ->
  Text ->
  OutgoingPart ->
  -- | The @msg_id@ being answered and the sequence number this send claims,
  -- together; 'Nothing' sends as an ordinary message instead of a passive answer.
  Maybe (Text, Int) ->
  IO (Either QQOfficialFailure QQOfficialSend)
sendOnce runtime kind openid part passive =
  sendQQOfficialMessage
    (qorHttpRuntime runtime)
    (qorConfig runtime)
    (qorTokenCache runtime)
    kind
    (fst <$> passive)
    openid
    (textSendPlan part.partText part.partReference (snd <$> passive))

-- | The passive window is short by design, and a group window is shorter than a
-- chat one; outside it the answer is sent as an ordinary message.
passiveWindowFor ::
  QQOfficialRuntime -> ConversationKind -> Maybe NativeEventId -> IO (Maybe PassiveWindow)
passiveWindowFor runtime kind replyTarget = case replyTarget of
  Nothing -> pure Nothing
  Just native -> do
    entries <- readTVarIO (qorPassives runtime)
    now <- getCurrentTime
    pure $ case Map.lookup native entries of
      Just window
        | diffUTCTime now window.passiveReceivedAt < windowSeconds ->
            Just window
      _ -> Nothing
  where
    windowSeconds = case kind of
      ConversationGroup -> groupPassiveWindowSeconds
      ConversationDirect -> directPassiveWindowSeconds

nextMsgSeq :: QQOfficialRuntime -> Text -> IO Int
nextMsgSeq runtime msgId = do
  now <- getCurrentTime
  atomically $ do
    let ref = qorMsgSeq runtime
    current <- readTVar ref
    let next = maybe 0 fst (Map.lookup msgId current) + 1
    writeTVar ref (Map.insert msgId (next, now) current)
    pure next

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

-- | Documented passive-reply budgets, kept slightly under the platform's own
-- numbers so a slow clock cannot turn a valid answer into a refusal.
groupPassiveWindowSeconds :: NominalDiffTime
groupPassiveWindowSeconds = 240

directPassiveWindowSeconds :: NominalDiffTime
directPassiveWindowSeconds = 3300

-- | Error codes meaning the answer's window has closed.
passiveWindowExpiredCodes :: [Int]
passiveWindowExpiredCodes = [40034128, 304103, 40034005]

-- | Keep every remembered window a little longer than any passive window.
passiveRetentionSeconds :: NominalDiffTime
passiveRetentionSeconds = 3600

defaultHeartbeatMillis :: Int
defaultHeartbeatMillis = 45000

idleWaitMicros :: Int
idleWaitMicros = 120 * 1000 * 1000

reconnectDelayMicros :: Int
reconnectDelayMicros = 5 * 1000 * 1000

fatalIdleMicros :: Int
fatalIdleMicros = 3600 * 1000 * 1000

-- | Cursor fingerprint: which application and host this stream belongs to.
sourceFingerprint :: QQOfficialConfig -> Text
sourceFingerprint cfg = qqOfficialApiBase cfg <> "|" <> cfg.qoAppId