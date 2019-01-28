{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Mail.Mailgun.Send
 ( MessageID, MailgunTags
 , ClickTrack(..), _DoTrackClick, _DontTrackClick, _TrackClickHtmlOnly
 , MailgunSendOptions(..)
 , tags, dkim, deliverAt, track, trackClicks, trackOpens, templateVariables
 , send
 ) where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson as JS
import           Data.Aeson.Lens
import           Data.Ascii (CIAscii)
import qualified Data.Ascii as ASCII
import qualified Data.ByteString.Lazy as LBS
import           Data.HashMap.Strict (HashMap)
import           Data.Machine
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as HTTP
import           Network.Mail.Mailgun.API
import           Network.Mail.Mailgun.Config
import           Network.Mail.Mime (Address, Mail, mailTo, mailCc, mailBcc
                                   ,renderMail', renderAddress)
import           Network.Wreq
import qualified Network.Wreq as HTTP
import           Safe
import           Text.Printf

type MessageID = Text

-- | 0 to 3 entries
type MailgunTags = [CIAscii]

data ClickTrack
 = DoTrackClick
 | DontTrackClick
 | TrackClickHtmlOnly
 deriving (Show, Eq, Ord)

makePrisms ''ClickTrack

clickTrackFormPart :: ClickTrack -> HTTP.Part
clickTrackFormPart DoTrackClick       = partText "o:tracking-clicks" "yes"
clickTrackFormPart DontTrackClick     = partText "o:tracking-clicks" "no"
clickTrackFormPart TrackClickHtmlOnly = partText "o:tracking-clicks" "htmlonly"

data MailgunSendOptions
  = MSO
    { _tags              :: MailgunTags
    , _dkim              :: Bool
    , _deliverAt         :: Maybe UTCTime
    , _track             :: Bool
    , _trackClicks       :: ClickTrack
    , _trackOpens        :: Bool
    , _templateVariables :: Map Text JS.Value
    }
  deriving (Show)

makeClassy ''MailgunSendOptions

yesNo :: Text -> Bool -> HTTP.Part
yesNo t True  = partText t "yes"
yesNo t False = partText t "no"

mgsoAsMultipart :: MailgunSendOptions -> [HTTP.Part]
mgsoAsMultipart o = mconcat
 [ o^..tags.each.to (partBS "o:tag" . ASCII.ciToByteString)
 , [partText "o:dkim" (if o^.dkim then "yes" else "no")]
 , o^..deliverAt.each.to (partString "o:deliverytime" . formatTime defaultTimeLocale "%a, %e %b %Y %T %z")
 , [o^.track.to (yesNo "o:tracking")]
 , [o^.trackClicks.to clickTrackFormPart]
 , [o^.trackOpens.to (yesNo "o:tracking-opens")]
 , map (\(k, v) -> partLBS ("v:" `T.append` k) (JS.encode v)) (o^.templateVariables.to Map.toList)
 ]

-- | Sends a given email.
send :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
     => Maybe MailgunSendOptions -> Mail -> m MessageID
send mo m = do
  test  <- view mailgunTestMode
  rndrd <- liftIO $! renderMail' m
  call (  MGPost (printf "/v3/%s/messages.mime") [] . mconcat $
    [ [yesNo "o:testmode" test]
    , maybe [] mgsoAsMultipart mo
    , [HTTP.partFileRequestBody "message" "message.mime" (HTTP.RequestBodyLBS rndrd)]
    , map (partText "to" . renderAddress) (mailTo m++mailCc m++mailBcc m)
    ]) (^?key "id"._JSON)

{- This has to be the form API, which leaves less control over attachments?
-- | Takes an email, ignoring the to addresses, and sends it to all the
--   addresses streamed in, paramterized by the JS.Values which can be used
--   in the templating.
sending :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m
          ,JS.ToJSON t)
        => Maybe MailgunSendOptions -> Mail
        -> ProcessT m (Address, t) MessageID
sending mo m = buffered 1000 ~> sendBatch ~> flattened
  where
    sendBatch = preplan $ do
      test  <- view mailgunTestMode
      rndrd <- liftIO $! renderMail' m
      call (sendReq test mo rndrd $
              (map (partText "to" . renderAddress) (mailTo m)))
           (^?key "id"._JSON)
-}

--renderAddress :: Address -> Text
--renderAddress (Address (Nothing) addr) = addr
--renderAddress (Address (Just nm) addr) = printf "%s <%s>" nm addr

{-
class IsEmail t where
  emailFrom    :: Lens'  t  Address
  emailCC      :: Lens'  t [Address]
  emailBCC     :: Lens'  t [Address]
  emailHeaders :: Lens'  t  Headers
  emailParts   :: Lens'  t [Alternatives]

instance IsEmail Mail where
  emailFrom    = lens mailFrom (\m a -> m {mailFrom=a})
  emailCC      = lens mailCc  (\m c -> m {mailCc=c})
  emailBCC     = lens mailBcc (\m b -> m {mailBcc=b})
  emailHeaders = lens mailHeaders (\m h -> m {mailHeaders=h})
  emailParts   = lens mailParts (\m p -> m {mailParts=p})

-- | Sends exactly one email.
--   This means multiple recipients will all get the same "to" header which has everyone
--   that the message is being sent to in it.
data SingleEmail
  = SingleEmail
    { _seFrom        :: Address
    , _seTo          :: [Address]
    , _seCC          :: [Address]
    , _seBCC         :: [Address]
    , _seHeaders     :: Headers
    , _seParts       :: [Alternatives]
    , _seSendOptions :: MailgunSendOptions
    }
  deriving (Show)

makeLenses ''SingleEmail

instance IsEmail SingleEmail where
  emailFrom    = seFrom
  emailTo      = seTo
  emailCC      = seCC
  emailBCC     = seBCC
  emailHeaders = seHeaders
  emailParts   = seParts

instance HasMailgunSendOptions SingleEmail where
  mailgunSendOptions = seSendOptions

{-
-- | Sends a seperate email to each recipient.
data BroadcastEmail
 = BroadcastEmail
   { _beFrom        :: Address
   , _beTo          :: Map Address JS.Value
   , _beCC          :: [Address]
   , _beBCC         :: [Address]
   , _beHeaders     :: Headers
   , _beParts       :: [Alternatives]
   , _beSendOptions :: MailgunSendOptions
   }
 deriving (Show)

makeLenses ''BroadcastEmail

instance IsEmail BroadcastEmail where
  emailFrom    = beFrom
  emailTo      = beTo.to Map.keys
  emailCC      = beCC
  emailBCC     = beBCC
  emailHeaders = beHeaders
  emailParts   = beParts

instance HasMailgunSendOptions BroadcastEmail where
  mailgunSendOptions = beSendOptions
-}



sendGeneric :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
            => [HTTP.Part] -> m MessageID
sendGeneric m = do
  t <- view mailgunTestMode
  call (MGPost (printf "/v3/%s/messages") [] (yesNo "o:testmode" t:m))
       (^?key "id"._JSON)

class MailgunEmail mail where
  {-# MINIMAL send #-}
  send :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
       => mail -> m MessageID
  sending :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
       => ProcessT m mail MessageID
  sending = autoM send
-}

