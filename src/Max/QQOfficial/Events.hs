-- | One inbound message of the QQ open platform, normalized into the canonical
-- ingest vocabulary.
--
-- This boundary is where the platform's values stop.  Two properties of the
-- protocol shape most of what follows:
--
-- * A group event names its conversation with a @group_openid@ and its people
--   with a @member_openid@, both scoped to this application and worth nothing
--   outside it.  They are stored as opaque native ids; the numeric conversation
--   key the rest of Max speaks is allocated in 'Max.DB.PlatformIds'.
-- * The id a message is deduplicated by is not the id it is quoted by.  A
--   passive answer carries the event's @msg_id@; a visible quote needs the
--   @msg_idx@ from the message scene.  'qoeNativeEventId' therefore carries the
--   reference index, because that is the one Max must hand back later, and the
--   whole payload stays in the raw column for the rest.
module Max.QQOfficial.Events
  ( QQOfficialContext (..),
    QQOfficialEvent (..),
    qqOfficialMessageEvent,
    qqOfficialEvent,
    qqOfficialTimestamp,
    sceneExtensions,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..))
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe, withObject, (.:?), (.!=))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, localTimeToUTC, minutesToTimeZone, parseTimeM)
import Max.IR
import Max.Platform.Types
  ( ConversationKind (..),
    MessageRelation (..),
    NativeEventId (..),
    NativeUserId (..),
  )

-- | Who this bot is on the platform.  The open platform has no numeric account
-- id: the application id and the gateway's own user id are both opaque strings,
-- and a message is recognised as ours by matching either.
data QQOfficialContext = QQOfficialContext
  { ctxSelfId :: !Text,
    ctxSelfLabel :: !Text,
    ctxSelfIds :: ![Text]
  }

data QQOfficialEvent = QQOfficialEvent
  { qoeEventName :: !Text,
    qoeNativeEventId :: !NativeEventId,
    -- | The platform's own @msg_id@ for this message.  A passive answer must
    -- quote *this* id, while a visible quote must name 'qoeNativeEventId'; the
    -- two are different namespaces and confusing them produces either a
    -- rejected send or a quote the platform will not render.
    qoeMessageId :: !Text,
    qoeKind :: !ConversationKind,
    -- | @group_openid@ or @user_openid@, depending on 'qoeKind'.
    qoeConversationNative :: !Text,
    qoeSenderNative :: !Text,
    qoeSenderIsSelf :: !Bool,
    qoeSenderDisplay :: !(Maybe Text),
    qoeOccurredAt :: !UTCTime,
    qoeContent :: ![Node 'Ingest],
    qoeRelations :: ![MessageRelation],
    qoeRaw :: !Value
  }

-- | The three events that carry a message.  Everything else on this intent bit
-- is membership or a notice, which is state and not transcript.
qqOfficialMessageEvent :: Text -> Bool
qqOfficialMessageEvent name =
  name `elem` ["GROUP_AT_MESSAGE_CREATE", "GROUP_MESSAGE_CREATE", "C2C_MESSAGE_CREATE"]

qqOfficialEvent :: QQOfficialContext -> Text -> Value -> Either Text QQOfficialEvent
qqOfficialEvent ctx name raw = case (parseMaybe eventParser raw, parseMaybe sceneParser raw) of
  (Nothing, _) -> Left "unrecognised message event"
  (_, Nothing) -> Left "unrecognised message scene"
  (Just event, Just scene) -> build event scene
  where
    build event scene = do
      occurredAt <- qqOfficialTimestamp event.timestamp
      let author = event.author
          sender = fromMaybe "" (firstJust [stringField "member_openid" author, stringField "user_openid" author, stringField "id" author])
          isSelf = objectField "bot" author == Just (Bool True) && sender `elem` ctxSelfIds ctx
          -- The platform strips the leading @-bot@ from a group at-message body
          -- and its @mentions@ list explicitly excludes the bot, so without this
          -- node an addressed message would arrive as one that addresses nobody.
          addressed = name == "GROUP_AT_MESSAGE_CREATE"
          selfNode =
            [ NMention (NativeUserId (ctxSelfId ctx)) (ctxSelfLabel ctx)
            | addressed,
              not (T.null (ctxSelfId ctx))
            ]
          body = nodesForMessageType event.messageType event.content event.arkData event.elements
          attachments = attachmentNodes (event.attachments <> concatMap elementAttachments event.elements)
          mentions = mentionNodes event.mentions
          relations =
            [ ReplyTo (NativeEventId target)
            | Just target <- [Map.lookup "ref_msg_idx" scene],
              not (T.null target)
            ]
          native = fromMaybe event.messageId (Map.lookup "msg_idx" scene)
      -- A message with no identifiable sender cannot be authorized, attributed
      -- or rostered; committing one would put a nameless principal in the
      -- ledger.
      if T.null sender
        then Left "message event names no sender"
        else
          Right
            QQOfficialEvent
              { qoeEventName = name,
                qoeNativeEventId = NativeEventId native,
                qoeMessageId = event.messageId,
                qoeKind = event.kind,
                qoeConversationNative = event.conversation,
                qoeSenderNative = sender,
                qoeSenderIsSelf = isSelf,
                qoeSenderDisplay = nonBlank (fromMaybe "" (stringField "username" author)),
                qoeOccurredAt = occurredAt,
                qoeContent = mergeText (selfNode <> body <> attachments <> mentions),
                qoeRelations = relations,
                qoeRaw = raw
              }

data RawEvent = RawEvent
  { messageId :: !Text,
    kind :: !ConversationKind,
    conversation :: !Text,
    author :: !Value,
    content :: !Text,
    messageType :: !Int,
    arkData :: !(Maybe Value),
    elements :: ![Value],
    attachments :: ![Value],
    mentions :: ![Value],
    timestamp :: !Text
  }

-- | @parseMaybe@ takes a @Value -> Parser@ function, so each of these states its
-- own name rather than relying on the caller to apply it.
eventParser :: Value -> Parser RawEvent
eventParser = withObject "message event" $ \o -> do
  messageId <- o .:? "id" .!= ("" :: Text)
  author <- o .:? "author" .!= Object mempty
  group <- o .:? "group_openid"
  user <- o .:? "user_openid"
  content <- o .:? "content" .!= ("" :: Text)
  messageType <- o .:? "message_type" .!= (0 :: Int)
  ark <- o .:? "ark_data"
  elements <- o .:? "msg_elements" .!= ([] :: [Value])
  attachments <- o .:? "attachments" .!= ([] :: [Value])
  mentions <- o .:? "mentions" .!= ([] :: [Value])
  timestamp <- o .:? "timestamp" .!= ("" :: Text)
  conversation <- case (group, user) of
    (Just g, _) | not (T.null g) -> pure g
    (_, Just u) | not (T.null u) -> pure u
    _ -> fail "event names neither group_openid nor user_openid"
  pure
    RawEvent
      { messageId,
        kind = if maybe False (not . T.null) group then ConversationGroup else ConversationDirect,
        conversation,
        author,
        content,
        messageType,
        arkData = ark,
        elements,
        attachments,
        mentions,
        timestamp
      }

-- | @message_scene@ is the only place a group message's referenceable index
-- lives, and its @ext@ is a list of @key=value@ strings rather than an object.
sceneParser :: Value -> Parser (Map Text Text)
sceneParser = withObject "message scene" $ \o -> do
  scene <- o .:? "message_scene" .!= Object mempty
  ext <- withObject "message scene" (\s -> s .:? "ext" .!= ([] :: [Value])) scene
  pure (sceneExtensions ext)

sceneExtensions :: [Value] -> Map Text Text
sceneExtensions entries = Map.fromList (mapMaybe pair entries)
  where
    -- Only the separator goes: a @msg_idx@ is base64 and ends in @==@, so a
    -- split on every @=@ would leave the value truncated.
    pair (String entry) = case T.breakOn "=" entry of
      (key, rest) | not (T.null key), not (T.null rest) -> Just (T.strip key, T.strip (T.drop 1 rest))
      _ -> Nothing
    pair _ = Nothing

-- | @0@ is plain text, @3@ a structured card, and @101@/@102@/@103@ the
-- composite shapes: parallel, chat history and quote.  The composites arrive
-- with their parts in @msg_elements@ and a rendered transcript in @content@;
-- the parts win, because they keep the structure the transcript has flattened.
nodesForMessageType :: Int -> Text -> Maybe Value -> [Value] -> [Node 'Ingest]
nodesForMessageType messageType content ark elements = case messageType of
  3 -> case ark of
    Just card -> [NCard (cardFromArk card)] <> textNodes content
    Nothing -> textNodes content
  101 -> compositeNodes
  102 -> compositeNodes
  103 -> compositeNodes
  _ -> textNodes content
  where
    compositeNodes = case concatMap elementNodes elements of
      [] -> textNodes content
      nested -> nested
    textNodes text = [NText text | not (T.null (T.strip text))]

-- | Flatten one element and, recursively, anything it contains.  A merged
-- message has no id this platform can be asked to expand later, so its own
-- rendering is kept as text rather than a forward reference nothing resolves.
elementNodes :: Value -> [Node 'Ingest]
elementNodes element = case parseMaybe elementParser element of
  Nothing -> []
  Just (messageType, content, ark, attachments, children) ->
    mergeText $
      [NCard (cardFromArk card) | messageType == 3, Just card <- [ark]]
        <> [NText content | not (T.null (T.strip content))]
        <> attachmentNodes attachments
        <> concatMap elementNodes children

elementParser :: Value -> Parser (Int, Text, Maybe Value, [Value], [Value])
elementParser = withObject "msg element" $ \o -> do
  messageType <- o .:? "message_type" .!= (0 :: Int)
  content <- o .:? "content" .!= ("" :: Text)
  ark <- o .:? "ark_data"
  attachments <- o .:? "attachments" .!= ([] :: [Value])
  children <- o .:? "msg_elements" .!= ([] :: [Value])
  pure (messageType, content, ark, attachments, children)

elementAttachments :: Value -> [Value]
elementAttachments element =
  fromMaybe [] (parseMaybe (withObject "msg element" (\o -> o .:? "attachments" .!= ([] :: [Value]))) element)

-- | A card this platform sent.
--
-- Only @ark_name@ and @prompt@ sit at the top of an ark payload; everything a
-- card actually shows — its title, description, link and picture — lives inside
-- the @fields@ object.  Reading them from the top level would produce an empty
-- card for every link someone shares.
cardFromArk :: Value -> Card
cardFromArk ark =
  Card
    { title = fieldsText "title" `orElse` arkText "ark_name",
      subtitle = fieldsText "desc" `orElse` arkText "prompt",
      url = fieldsText "jump_url",
      tag = fieldsText "tag",
      preview = fieldsText "preview" >>= mediaRemoteRef,
      raw = Just ark
    }
  where
    fields = arkObject "fields" ark
    fieldsText name = nonBlank =<< (stringField name =<< fields)
    arkText name = nonBlank =<< stringField name ark
    orElse (Just value) _ = Just value
    orElse Nothing fallback = fallback

arkObject :: Text -> Value -> Maybe Value
arkObject name value = case value of
  Object fields -> case KeyMap.lookup (Key.fromText name) fields of
    Just nested@(Object _) -> Just nested
    _ -> Nothing
  _ -> Nothing

-- | Any field of an object, whatever its type.  Callers that want text go
-- through 'stringField'; the ark card fields nest an object under @fields@,
-- which this is the only reader that has to reach.
objectField :: Text -> Value -> Maybe Value
objectField name value = case value of
  Object fields -> KeyMap.lookup (Key.fromText name) fields
  _ -> Nothing

-- | Inbound media.  The download URL carries a short-lived signature and stops
-- working shortly after the event, so the node keeps it as a remote reference
-- and the shared fetch worker imports it immediately.
--
-- A voice message also carries the platform's own speech-to-text result; that
-- text is the only part of it Max can read, so it becomes the caption rather
-- than a blank transcript line.
attachmentNodes :: [Value] -> [Node 'Ingest]
attachmentNodes attachments = mapMaybe node attachments
  where
    node attachment = do
      contentType <- stringField "content_type" attachment
      -- An ingest-phase media node holds the reference as a Maybe: an
      -- attachment with no usable URL stays a media marker instead of vanishing.
      let ref = firstJust [stringField "url" attachment, stringField "voice_wav_url" attachment] >>= mediaRemoteRef
      pure
        ( NMedia
            ref
            MediaMeta
              { kind = mediaKindFor contentType,
                mime = nonBlank contentType,
                sizeBytes = numberField "size" attachment,
                name = nonBlank =<< stringField "filename" attachment,
                description = nonBlank =<< stringField "asr_refer_text" attachment,
                raw = Just attachment
              }
        )

mediaKindFor :: Text -> MediaKind
mediaKindFor contentType
  | "image/" `T.isPrefixOf` contentType = MImage
  | "video/" `T.isPrefixOf` contentType = MVideo
  | "audio/" `T.isPrefixOf` contentType = MAudio
  | contentType == "voice" = MAudio
  | otherwise = MFile

-- | People named in the text.  The platform's list excludes the bot and carries
-- no offset into the body, so these become trailing mentions: the identity
-- survives, the position does not.
mentionNodes :: [Value] -> [Node 'Ingest]
mentionNodes mentions = mapMaybe node mentions
  where
    node mention = do
      native <- firstJust [stringField "member_openid" mention, stringField "user_openid" mention, stringField "id" mention]
      let label = nonBlank (fromMaybe "" (stringField "username" mention))
      pure (NMention (NativeUserId native) (fromMaybe native label))

-- | @2026-07-21T08:00:00+08:00@.  The offset is not optional and is not always
-- @Z@; a message whose offset was dropped would land in the ledger at a
-- plausible but wrong hour, so an unreadable timestamp is an error rather than a
-- quiet fall back to arrival time.
qqOfficialTimestamp :: Text -> Either Text UTCTime
qqOfficialTimestamp raw
  | T.null (T.strip raw) = Left "empty timestamp"
  | otherwise =
      let stripped = T.strip raw
          -- The Z form has to be taken off before the offset is looked for: the
          -- date's own hyphens would otherwise be read as an offset sign.
          (naive, offsetText) =
            if "Z" `T.isSuffixOf` stripped
              then (T.dropEnd 1 stripped, "+00:00")
              else splitOffset stripped
          (whole, _fraction) = T.breakOn "." naive
          minutes = fromRight 0 (parseOffset offsetText)
       in case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack whole) of
            Nothing -> Left ("unrecognised timestamp: " <> raw)
            Just local -> Right (localTimeToUTC (minutesToTimeZone minutes) local)

-- | Split a trailing offset off an RFC3339 timestamp.  The date itself contains
-- hyphens, so only the last sign in the string can begin an offset.
splitOffset :: Text -> (Text, Text)
splitOffset text = case signs of
  [] -> (text, "")
  _ ->
    let at = last signs
     in (T.take at text, T.drop at text)
  where
    signs = [index | (index, c) <- zip [0 ..] (T.unpack text), c == '+' || c == '-']

parseOffset :: Text -> Either Text Int
parseOffset text = case T.uncons text of
  Nothing -> Left "no offset"
  Just (sign, digits) -> case sign of
    '+' -> withSign digits
    '-' -> negate <$> withSign digits
    _ -> Left ("bad offset sign: " <> text)
  where
    withSign digits =
      let compact = T.filter (/= ':') digits
          hours = T.take 2 compact
          minutes = T.drop 2 compact
          acceptable =
            T.length hours == 2
              && T.all isDigit hours
              && (\part -> T.null part || (T.length part == 2 && T.all isDigit part)) minutes
       in if acceptable
            then Right (digitsOf hours * 60 + (if T.null minutes then 0 else digitsOf minutes))
            else Left ("bad offset: " <> text)
    digitsOf = T.foldl' (\acc c -> acc * 10 + fromEnum c - fromEnum '0') (0 :: Int)

stringField :: Text -> Value -> Maybe Text
stringField name value = case objectField name value of
  Just (String text) -> Just text
  _ -> Nothing

numberField :: Text -> Value -> Maybe Int64
numberField name value = case objectField name value of
  Just (Number n) -> Just (truncate n)
  _ -> Nothing

-- | @A@ when present and non-empty, else @B@.  The platform sends @""@ for a
-- field it has no value for, which is not the same as sending nothing.
firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (<|>) Nothing