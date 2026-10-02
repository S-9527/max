{-# LANGUAGE DeriveGeneric #-}

-- | Configuration and outbound capability declaration for the QQ open
-- platform's own bot (@api.bot.qq.com@).
--
-- This is a separate platform rather than another QQ edge on purpose.  A
-- OneBot endpoint and an official bot endpoint are different accounts with
-- different identities (numeric uins versus per-app openids), different
-- capabilities, and different delivery evidence, and none of those are things
-- a shared @PlatformQQ@ could describe honestly.
module Max.QQOfficial.Types
  ( QQOfficialConfig (..),
    qqOfficialApiBase,
    qqOfficialCapabilities,
    qqOfficialReplyPartBudget,
    qqOfficialKindOfLegacyId,
    stripOutboundUrls,
    mergeChunksToBudget,
    qqOfficialGatewayIntents,
    qqOfficialStreamKey,
    qqOfficialHttpTimeoutMicros,
    qqOfficialMaxResponseBytes,
    qqOfficialStatusPreviewBytes,
    qqOfficialTokenRefreshMarginSeconds,
    urlPlaceholder,
  )
where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Max.IR (Node (..))
import Max.IR.Lower (OutboundCaps (..), Tier (..), textOnlyCaps)
import Max.Platform.Types (ConversationKind (..))
import OneBot.Types (GroupId (..), isPrivateChat)

data QQOfficialConfig = QQOfficialConfig
  { qoAppId :: !Text,
    qoAppSecret :: !Text,
    -- | Override the API host.  The sandbox host is a separate deployment and
    -- the open platform has changed its spelling before, so an explicit value
    -- is a configuration fix rather than a code change.
    qoApiBase :: !(Maybe Text),
    qoSandbox :: !Bool,
    -- | Gateway intent bits.  Only bits this application actually holds may be
    -- requested: an unauthorised intent makes the gateway drop the connection
    -- during identify.
    qoIntents :: !Int64,
    -- | Human label for this bot, used when rendering a self-mention.  The
    -- gateway also reports the bot's own display name, which wins when present.
    qoBotName :: !Text,
    -- | Owners by openid.  Max authorizes owners by numeric id, so these are
    -- resolved to synthetic ids at startup; the openids stay the only thing an
    -- operator can read off the platform.
    qoOwners :: ![Text],
    -- | Whether every group message is expected.  Not requested by identify:
    -- whether the full-message events are delivered is the *group's* own
    -- setting, so this records the intent and shows up in the startup log
    -- rather than changing a handshake field.
    qoFullGroupMessages :: !Bool
  }
  deriving stock (Eq, Generic)

-- | The AppSecret is a credential; 'Show' must never render it.
instance Show QQOfficialConfig where
  show cfg =
    "QQOfficialConfig {qoAppId = "
      <> show cfg.qoAppId
      <> ", qoAppSecret = <redacted>"
      <> ", qoApiBase = "
      <> show (qqOfficialApiBase cfg)
      <> ", qoSandbox = "
      <> show cfg.qoSandbox
      <> ", qoIntents = "
      <> show cfg.qoIntents
      <> ", qoBotName = "
      <> show cfg.qoBotName
      <> ", qoOwners = "
      <> show (length cfg.qoOwners)
      <> " openid(s), qoFullGroupMessages = "
      <> show cfg.qoFullGroupMessages
      <> "}"

qqOfficialApiBase :: QQOfficialConfig -> Text
qqOfficialApiBase cfg = fromMaybe (sandboxOrProduction cfg.qoSandbox) cfg.qoApiBase

sandboxOrProduction :: Bool -> Text
sandboxOrProduction sandbox
  | sandbox = "https://sandbox.api.sgroup.qq.com"
  | otherwise = "https://api.bot.qq.com"

-- | @GROUP_AND_C2C_EVENT@, the one bit that carries group at-messages, full
-- group messages and one-to-one chats.  Group membership changes and the
-- notice events ride the same bit.
qqOfficialGatewayIntents :: Int64
qqOfficialGatewayIntents = 1 `shiftL` 25

-- | What this platform can actually carry, declared honestly.
--
-- The open platform's send API has no at-segment, so a mention is only ever
-- its readable text; there are no faces and no reactions at all, which is why
-- 'emote' stays on the text tier instead of failing at send time — a face the
-- model was told it could use must not be able to take a whole reply down with
-- it.  Rich media needs a two-step upload for a @file_info@ with a lifetime;
-- until that path is exercised against a live bot, media folds to its text
-- tier rather than being advertised and lost.
qqOfficialCapabilities :: OutboundCaps
qqOfficialCapabilities =
  textOnlyCaps
    { mention = TierText,
      reply = TierNative,
      emote = TierText,
      image = TierText,
      sticker = TierText,
      video = TierText,
      audio = TierText,
      file = TierText,
      card = TierText,
      reaction = False,
      edit = False,
      redact = False,
      -- Comfortably inside err_code 40054007 (message too long); the platform's
      -- own ceiling is not published.
      maxTextBytes = Just 4000,
      maxNativeMedia = 1
    }

-- | How many messages one inbound message may be answered with.  A group
-- message is passive-replyable for 5 minutes and 5 sends; a one-to-one chat for
-- 60 minutes and 4.  Exceeding either is err_code 40034128.
qqOfficialReplyPartBudget :: ConversationKind -> Int
qqOfficialReplyPartBudget ConversationGroup = 5
qqOfficialReplyPartBudget ConversationDirect = 4

-- | Read the chat kind back from the synthetic conversation id.
--
-- This deliberately asks 'isPrivateChat' rather than re-deriving the interval:
-- 'Max.DB.PlatformIds.toQQPrivateChatRange' allocates direct chats inside the
-- range that predicate accepts, and asking the predicate itself means the
-- allocator and the send path cannot drift apart.
qqOfficialKindOfLegacyId :: Int64 -> ConversationKind
qqOfficialKindOfLegacyId legacy
  | legacy > 0 = ConversationGroup
  | isPrivateChat (GroupId legacy) = ConversationDirect
  | otherwise = ConversationGroup

-- | Group messages reject URLs outright (err_code 40054010), and Max produces
-- them constantly — search results, browser output, a citation.  Stripping them
-- here keeps the delivery from failing outright; the caller records the count so
-- the degradation is visible rather than silent.
stripOutboundUrls :: Text -> (Text, Int)
stripOutboundUrls = go 0 ""
  where
    go count acc rest = case breakScheme rest of
      Nothing -> (acc <> rest, count)
      Just (before, schemeLength) ->
        let scheme = T.take schemeLength rest
            after = T.drop schemeLength rest
            (urlText, tailText) = T.break isSpace after
         in if T.null urlText
              -- A bare scheme with no host is not a link; leave it alone and
              -- keep looking past it.
              then go count (acc <> before <> scheme) tailText
              else go (count + 1) (acc <> before <> urlPlaceholder) tailText

    breakScheme text =
      case [(before, T.length scheme) | scheme <- schemes, Just before <- [fst (T.breakOn scheme text)]] of
        [] -> Nothing
        matches -> Just (snd (foldr1 earlier matches))
      where
        earlier a b = if T.length (fst a) <= T.length (fst b) then a else b

    schemes = ["https://" :: Text, "http://"]

urlPlaceholder :: Text
urlPlaceholder = "[链接]"

-- | Fold wire parts down to what the platform will accept in one answer.
--
-- Max plans a reply as however many chunks the content deserves, and this
-- platform counts every chunk against a fixed per-message budget.  Merging is
-- only safe between parts that carry no native media, so a media part is never
-- folded into its neighbour.
mergeChunksToBudget :: Int -> [[Node p]] -> [[Node p]]
mergeChunksToBudget budget chunks
  | budget < 1 = chunks
  | otherwise = go chunks
  where
    go [] = []
    go (firstChunk : rest) = grow [firstChunk] rest
    grow acc [] = [acc]
    grow acc (nextChunk : more)
      | length acc >= budget = acc <> grow [nextChunk] more
      | foldable acc nextChunk = grow (acc <> nextChunk) more
      | otherwise = acc : grow [nextChunk] more

    foldable left right = all textLikeNode (left <> right)
    textLikeNode = \case
      NMedia {} -> False
      _ -> True

qqOfficialStreamKey :: Text
qqOfficialStreamKey = "gateway"

qqOfficialHttpTimeoutMicros :: Int
qqOfficialHttpTimeoutMicros = 30_000_000

qqOfficialMaxResponseBytes :: Int
qqOfficialMaxResponseBytes = 4 * 1024 * 1024

qqOfficialStatusPreviewBytes :: Int
qqOfficialStatusPreviewBytes = 4096

-- | An access token is only replaced inside the last minute of its life, so
-- refreshing starts that much early.
qqOfficialTokenRefreshMarginSeconds :: Int
qqOfficialTokenRefreshMarginSeconds = 90