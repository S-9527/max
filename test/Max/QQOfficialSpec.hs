module Max.QQOfficialSpec (spec) where

import Data.Aeson (Value, object, withObject, (.=))
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, toGregorian, timeOfDay)
import Max.DB.PlatformIds (toQQPrivateChatRange)
import Max.IR
import Max.IR.Lower (OutboundCaps (..), Tier (..))
import Max.Platform.Types
import Max.QQOfficial.API
import Max.QQOfficial.Events
import Max.QQOfficial.Gateway
import Max.QQOfficial.Types
import OneBot.Types (GroupId (..), isPrivateChat)
import Test.Hspec

ctx :: QQOfficialContext
ctx =
  QQOfficialContext
    { ctxSelfId = "1020000001",
      ctxSelfLabel = "小鲨",
      ctxSelfIds = ["1020000001", "6158788878435714165"]
    }

cfg :: QQOfficialConfig
cfg =
  QQOfficialConfig
    { qoAppId = "1020000001",
      qoAppSecret = "super-secret-value",
      qoApiBase = Nothing,
      qoSandbox = False,
      qoIntents = qqOfficialGatewayIntents,
      qoBotName = "小鲨",
      qoOwners = ["MEMBEROPENID1"],
      qoFullGroupMessages = True
    }

-- | A group event as the platform delivers it: the @-bot@ prefix already
-- stripped, the scene carrying the reference index, the author naming the sender
-- by openid.
groupAtEvent :: Value
groupAtEvent =
  object
    [ "id" .= ("ROBOT1.0_abc" :: Text),
      "content" .= (" /今日天气 " :: Text),
      "group_openid" .= ("GROUPOPENID1" :: Text),
      "timestamp" .= ("2026-07-21T10:00:00+08:00" :: Text),
      "message_type" .= (0 :: Int),
      "author"
        .= object
          [ "id" .= ("MEMBEROPENID1" :: Text),
            "member_openid" .= ("MEMBEROPENID1" :: Text),
            "username" .= ("小明" :: Text),
            "bot" .= False
          ],
      "message_scene"
        .= object
          [ "source" .= ("default" :: Text),
            "ext" .= [("msg_idx=REFIDX_xxx==" :: Text), ("auth_token=yyy" :: Text)]
          ]
    ]

c2cEvent :: Value
c2cEvent =
  object
    [ "id" .= ("ROBOT1.0_c2c" :: Text),
      "content" .= ("在吗" :: Text),
      "user_openid" .= ("USEROPENID1" :: Text),
      "timestamp" .= ("2026-07-21T10:00:00+08:00" :: Text),
      "message_type" .= (0 :: Int),
      "author"
        .= object
          [ "user_openid" .= ("USEROPENID1" :: Text),
            "username" .= ("小红" :: Text),
            "bot" .= False
          ],
      "message_scene" .= object ["ext" .= ["msg_idx=REFIDX_c2c=="]]
    ]

groupAtMentionEvent :: Value
groupAtMentionEvent =
  groupAtEvent
    <> object
      [ "mentions" .= [object ["member_openid" .= ("MEMBEROPENID2" :: Text), "username" .= ("小红" :: Text)]]
    ]

groupAtVoiceEvent :: Value
groupAtVoiceEvent =
  groupAtEvent
    <> object
      [ "attachments"
          .= [ object
                 [ "content_type" .= ("voice" :: Text),
                   "url" .= ("https://multimedia.example/download?rkey=x" :: Text),
                   "voice_wav_url" .= ("https://multimedia.example/wav?rkey=x" :: Text),
                   "size" .= (2048 :: Int),
                   "asr_refer_text" .= ("今天真热" :: Text)
                 ]
             ]
      ]

groupAtQuoteEvent :: Value
groupAtQuoteEvent =
  groupAtEvent
    <> object
      [ "message_type" .= (103 :: Int),
        "message_scene"
          .= object ["ext" .= ["msg_idx=REFIDX_quote==", "ref_msg_idx=REFIDX_target=="]]
      ]

ownMessageEvent :: Value
ownMessageEvent =
  object
    [ "id" .= ("ROBOT1.0_own" :: Text),
      "content" .= ("我刚说过的话" :: Text),
      "group_openid" .= ("GROUPOPENID1" :: Text),
      "timestamp" .= ("2026-07-21T10:01:00+08:00" :: Text),
      "message_type" .= (0 :: Int),
      "author"
        .= object
          [ "id" .= ("6158788878435714165" :: Text),
            "member_openid" .= ("6158788878435714165" :: Text),
            "username" .= ("小鲨" :: Text),
            "bot" .= True
          ],
      "message_scene" .= object ["ext" .= ["msg_idx=REFIDX_own=="]]
    ]

otherBotEvent :: Value
otherBotEvent =
  ownMessageEvent
    <> object
      [ "author"
          .= object
            [ "id" .= ("OTHERBOT" :: Text),
              "member_openid" .= ("OTHERBOT" :: Text),
              "username" .= ("别的机器人" :: Text),
              "bot" .= True
            ]
      ]

compositeEvent :: Value
compositeEvent =
  groupAtEvent
    <> object
      [ "message_type" .= (102 :: Int),
        "content" .= (" " :: Text),
        "msg_elements"
          .= [ object ["content" .= ("a" :: Text)],
               object ["content" .= ("b" :: Text)]
             ]
      ]

parseQQOfficial :: Text -> Value -> IO QQOfficialEvent
parseQQOfficial name payload = case qqOfficialEvent ctx name payload of
  Left err -> expectationFailure ("parse failed: " <> show err) >> error "unreachable"
  Right event -> pure event

mentionsIn :: [Node 'Ingest] -> [(Text, Text)]
mentionsIn = mapMaybe $ \case
  NMention (NativeUserId native) label -> Just (native, label)
  _ -> Nothing

mediaKindsIn :: [Node 'Ingest] -> [MediaKind]
mediaKindsIn = mapMaybe $ \case
  NMedia _ meta -> Just meta.kind
  _ -> Nothing

captionsIn :: [Node 'Ingest] -> [Text]
captionsIn = mapMaybe $ \case
  NMedia _ meta -> meta.description
  _ -> Nothing

utcDay :: UTCTime -> (Int, Int, Int, Int, Int, Int)
utcDay t =
  let (year, month, day) = toGregorian t
      (hour, minute, second) = timeOfDay t
   in (year, month, day, hour, minute, second)

-- | Hspec's @shouldNotContain@ works on lists; this asks the question a reader
-- actually has, which is whether the rendered text carries the secret anywhere.
shouldNotContain :: String -> String -> Expectation
shouldNotContain haystack needle =
  (T.isInfixOf (T.pack needle) (T.pack haystack)) `shouldBe` False

spec :: Spec
spec = do
  describe "QQ official identity" $ do
    -- This is the invariant seventeen call sites depend on: each of them reads
    -- the chat kind from the sign of this number, and a one-to-one chat read as
    -- a room changes the permission tier, the prompt's framing, the memory
    -- scope and the @!@ commands all at once.
    it "puts a re-banded direct chat inside the range isPrivateChat accepts" $
      map (isPrivateChat . GroupId . toQQPrivateChatRange) [-1000000000001, -1000000000002, -1000000000009]
        `shouldBe` [True, True, True]

    it "keeps a foreign group id out of that range" $ do
      isPrivateChat (GroupId (-1000000000000 - 7)) `shouldBe` False
      qqOfficialKindOfLegacyId (-1000000000000 - 7) `shouldBe` ConversationGroup

    it "reads a re-banded id back as a direct conversation" $ do
      let legacy = toQQPrivateChatRange (-1000000000000 - 1)
      qqOfficialKindOfLegacyId legacy `shouldBe` ConversationDirect
      isPrivateChat (GroupId legacy) `shouldBe` True

    it "never collides with a real QQ number" $ do
      -- QQ uins are at most ten digits; the band starts at eleven.
      abs (toQQPrivateChatRange (-1000000000000 - 1)) `shouldSatisfy` (> 100000000000)

    it "gives different conversations different ids" $
      toQQPrivateChatRange (-1000000000000 - 1) `shouldNotBe` toQQPrivateChatRange (-1000000000000 - 2)

  describe "QQ official timestamps" $ do
    it "applies the offset the platform sends" $
      fmap utcDay (qqOfficialTimestamp "2026-07-21T08:00:00+08:00")
        `shouldBe` Right (2026, 7, 21, 0, 0, 0)

    it "accepts a Z zone and fractional seconds" $
      fmap utcDay (qqOfficialTimestamp "2026-07-21T00:00:00.123Z") `shouldBe` Right (2026, 7, 21, 0, 0, 0)

    it "refuses to guess when the timestamp is unreadable" $ do
      qqOfficialTimestamp "" `shouldSatisfy` isLeft
      qqOfficialTimestamp "yesterday" `shouldSatisfy` isLeft

  describe "QQ official outbound text" $ do
    it "strips the URLs a group message refuses to carry" $ do
      -- err_code 40054010: one URL fails the whole send, and Max produces URLs
      -- constantly — search results, browser output, a citation.
      let (text, count) = stripOutboundUrls "看这个 https://example.com/a?b=1 就行"
      count `shouldBe` 1
      text `shouldBe` "看这个 [链接] 就行"

    it "strips every occurrence and leaves ordinary text alone" $ do
      let (text, count) = stripOutboundUrls "a http://x.y b https://z.w c"
      count `shouldBe` 2
      text `shouldBe` "a [链接] b [链接] c"

    it "leaves a bare scheme that is not a link" $
      stripOutboundUrls "see http:// for details" `shouldBe` ("see http:// for details", 0)

  describe "QQ official reply budget" $ do
    it "folds extra parts down to what the platform will accept" $ do
      let parts :: [[Node 'Lowered]]
          parts = [[NText "a"], [NText "b"], [NText "c"], [NText "d"], [NText "e"], [NText "f"]]
      length (mergeChunksToBudget 5 parts) `shouldBe` 5

    it "keeps separate parts when the budget allows it" $ do
      let parts :: [[Node 'Lowered]]
          parts = [[NText "a"], [NText "b"]]
      mergeChunksToBudget 5 parts `shouldBe` parts

    it "never folds a native media part into its neighbour" $ do
      -- One media part is one message on this platform, so folding two of them
      -- together would produce a message carrying two attachments where the API
      -- takes a single file_info.
      let media :: Node 'Lowered
          media = NMedia (mediaBlobRef (replicate 64 'a')) (MediaMeta MImage Nothing Nothing Nothing Nothing Nothing)
          parts :: [[Node 'Lowered]]
          parts = [[NText "a"], [media], [NText "b"]]
      length (mergeChunksToBudget 1 parts) `shouldBe` 3

    it "budgets a group and a chat differently, as the platform does" $ do
      qqOfficialReplyPartBudget ConversationGroup `shouldBe` 5
      qqOfficialReplyPartBudget ConversationDirect `shouldBe` 4

  describe "QQ official declared capabilities" $ do
    it "never claims a feature the send API does not have" $ do
      let caps = qqOfficialCapabilities
      -- No at-segment exists on the send API, so a mention is text.
      caps.mention `shouldBe` TierText
      caps.emote `shouldBe` TierText
      caps.reaction `shouldBe` False
      caps.edit `shouldBe` False
      caps.redact `shouldBe` False

    it "quotes natively, because a message reference really is native" $
      qqOfficialCapabilities.reply `shouldBe` TierNative

  describe "QQ official gateway protocol" $ do
    it "reads a hello frame" $
      fmap frameOp (parseGatewayFrame "{\"op\":10,\"d\":{\"heartbeat_interval\":45000}}") `shouldBe` Right 10

    it "reads the heartbeat period out of the payload" $
      fmap frameHeartbeatMillis (parseGatewayFrame "{\"op\":10,\"d\":{\"heartbeat_interval\":45000}}")
        `shouldBe` Right (Just 45000)

    it "does not let a dispatch payload break the parse" $ do
      fmap frameOp (parseGatewayFrame "{\"op\":1,\"d\":1337}") `shouldBe` Right 1
      fmap frameOp (parseGatewayFrame "{\"op\":0,\"s\":42,\"t\":\"READY\",\"d\":{}}") `shouldBe` Right 0

    it "presents the token the way the gateway expects it" $
      parseEither (withObject "identify" (\o -> o .: "token")) (identifyPayload "TOKEN" 1)
        `shouldBe` Right ("QQBot TOKEN" :: Text)

    it "asks for exactly one shard" $
      parseEither (withObject "identify" (\o -> o .: "shard")) (identifyPayload "TOKEN" 1)
        `shouldBe` Right ([0, 1] :: [Int])

    it "carries the last sequence number on a heartbeat" $
      heartbeatPayload (Just 1337) `shouldBe` object ["op" .= (1 :: Int), "d" .= (1337 :: Int64)]

    it "resumes with the sequence number this process last handled" $
      parseEither (withObject "resume" (\o -> o .: "seq")) (resumePayload "TOKEN" "session-1" 99)
        `shouldBe` Right (99 :: Int64)

    it "turns the address into a connect target" $ do
      gatewayConnectTarget "wss://api.bot.qq.com/websocket/" `shouldBe` Just "api.bot.qq.com:443/websocket/"
      gatewayConnectTarget "http://api.bot.qq.com/websocket" `shouldBe` Nothing

    it "decides what a close code means for the next attempt" $ do
      closeRecovery 4009 `shouldBe` RecoveryResume -- connection expired
      closeRecovery 4007 `shouldBe` RecoveryIdentify -- seq error
      closeRecovery 4013 `shouldBe` RecoveryIdentify -- unauthorised intent
      closeRecovery 4914 `shouldBe` RecoveryFatal -- bot offline
      closeRecovery 4915 `shouldBe` RecoveryFatal -- bot banned

    it "reads the session a ready frame hands out" $
      readySession
        (object ["session_id" .= ("abc" :: Text), "user" .= object ["id" .= ("7" :: Text), "username" .= ("小鲨" :: Text)]])
        `shouldBe` Just ("abc", Just "7", Just "小鲨")

  describe "QQ official inbound events" $ do
    it "knows which events carry a message" $ do
      qqOfficialMessageEvent "GROUP_AT_MESSAGE_CREATE" `shouldBe` True
      qqOfficialMessageEvent "GROUP_MESSAGE_CREATE" `shouldBe` True
      qqOfficialMessageEvent "C2C_MESSAGE_CREATE" `shouldBe` True
      qqOfficialMessageEvent "GROUP_ADD_ROBOT" `shouldBe` False
      qqOfficialMessageEvent "FRIEND_ADD" `shouldBe` False

    it "puts the bot's own mention back, because the platform strips it" $ do
      -- Without this node the message arrives as one that addresses nobody and
      -- Max never answers it.
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtEvent
      event.qoeContent `shouldBe` [NMention (NativeUserId "1020000001") "小鲨", NText " /今日天气 "]

    it "keys the message by the reference index, not by the event id" $ do
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtEvent
      event.qoeNativeEventId `shouldBe` NativeEventId "REFIDX_xxx=="
      -- A passive answer quotes this other id instead; they are not the same
      -- namespace and swapping them makes the platform refuse the send.
      event.qoeMessageId `shouldBe` "ROBOT1.0_abc"

    it "reads a group as a group and a chat as a chat" $ do
      group <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtEvent
      group.qoeKind `shouldBe` ConversationGroup
      group.qoeConversationNative `shouldBe` "GROUPOPENID1"
      chat <- parseQQOfficial "C2C_MESSAGE_CREATE" c2cEvent
      chat.qoeKind `shouldBe` ConversationDirect
      chat.qoeConversationNative `shouldBe` "USEROPENID1"

    it "keeps the people named in the text" $ do
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtMentionEvent
      mentionsIn event.qoeContent `shouldBe` [("1020000001", "小鲨"), ("MEMBEROPENID2", "小红")]

    it "keeps an attachment as a media reference" $ do
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtVoiceEvent
      mediaKindsIn event.qoeContent `shouldBe` [MAudio]

    it "takes a voice message's own transcription as the caption" $ do
      -- Max has no speech recognition here; the platform's result is the only
      -- part of a voice message anybody can read.
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtVoiceEvent
      captionsIn event.qoeContent `shouldBe` ["今天真热"]

    it "answers a quote with a reply relation" $ do
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" groupAtQuoteEvent
      event.qoeRelations `shouldBe` [ReplyTo (NativeEventId "REFIDX_target==")]

    it "recognises the bot's own message so it is never answered" $ do
      event <- parseQQOfficial "GROUP_MESSAGE_CREATE" ownMessageEvent
      event.qoeSenderIsSelf `shouldBe` True

    it "does not mistake another bot's message for its own" $ do
      event <- parseQQOfficial "GROUP_MESSAGE_CREATE" otherBotEvent
      event.qoeSenderIsSelf `shouldBe` False

    it "keeps a merged message as the parts the platform sent" $ do
      event <- parseQQOfficial "GROUP_AT_MESSAGE_CREATE" compositeEvent
      event.qoeContent `shouldBe` [NMention (NativeUserId "1020000001") "小鲨", NText "a", NText "b"]

    it "refuses an event that names no conversation" $
      qqOfficialEvent ctx "GROUP_AT_MESSAGE_CREATE" (object ["id" .= ("x" :: Text)]) `shouldSatisfy` isLeft

  describe "QQ official send plans" $ do
    it "sends text with a quote" $ do
      let plan = textSendPlan "hello" (Just "REFIDX_x==") (Just 2)
      plan.planMsgType `shouldBe` 0
      plan.planContent `shouldBe` Just "hello"
      plan.planReference `shouldBe` Just "REFIDX_x=="
      plan.planMsgSeq `shouldBe` Just 2

  describe "QQ official failure classification" $ do
    it "retries what provably did not take effect" $
      classifyQQOfficialFailure (QQOfficialRejected 40034100 "频控") `shouldBe` True

    it "treats a muted bot as a refusal rather than a retry" $
      classifyQQOfficialFailure (QQOfficialRejected 40054002 "机器人被禁言") `shouldBe` False

    it "treats an expired passive window as a refusal, not a retry" $
      -- Retrying the same answer would be refused again; the transport re-sends
      -- it as an ordinary message instead.
      classifyQQOfficialFailure (QQOfficialRejected 40034128 "被动回复时间或次数超限") `shouldBe` False

    it "treats a malformed success body as permanent" $
      classifyQQOfficialFailure (QQOfficialMalformed "no id") `shouldBe` False

  describe "QQ official configuration" $ do
    it "never renders the app secret" $
      show cfg `shouldNotContain` "super-secret-value"

    it "reports the owner count instead of the openids" $
      show cfg `shouldContain` "1 openid(s)"

    it "addresses the sandbox and production as separate deployments" $ do
      qqOfficialApiBase cfg `shouldBe` "https://api.bot.qq.com"
      qqOfficialApiBase cfg {qoSandbox = True} `shouldBe` "https://sandbox.api.sgroup.qq.com"
      qqOfficialApiBase cfg {qoApiBase = Just "https://example.test"} `shouldBe` "https://example.test"