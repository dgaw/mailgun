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
 , sending
 ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson as JS
import           Data.Aeson.Lens
import           Data.Ascii (CIAscii)
import qualified Data.Ascii as ASCII
import qualified Data.ByteString.Lazy as BSL
import           Data.Machine
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.These
import           Data.Time
import           Network.Mail.Mailgun.API
import           Network.Mail.Mailgun.Config
import           Network.Mail.Mime (Address, addressEmail, Mail, renderMail', renderAddress)
import           Network.Mime
import           Network.Wreq
import qualified Network.Wreq as HTTP
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

mgsoAsMultipart :: Bool -> Maybe MailgunSendOptions -> [HTTP.Part]
mgsoAsMultipart test mo = mconcat $
 [ [yesNo "o:testmode" test]
 , maybe [] (\o -> mconcat
     [ o^..tags.each.to (partBS "o:tag" . ASCII.ciToByteString)
     , [partText "o:dkim" (if o^.dkim then "yes" else "no")]
     , o^..deliverAt.each.to (partString "o:deliverytime" .
                                         formatTime defaultTimeLocale "%a, %e %b %Y %T %z")
     , [o^.track.to (yesNo "o:tracking")]
     , [o^.trackClicks.to clickTrackFormPart]
     , [o^.trackOpens.to (yesNo "o:tracking-opens")]
     , map (\(k, v) -> partLBS ("v:" `T.append` k) (JS.encode v))
           (o^.templateVariables.to Map.toList)
     ]) mo
 ]

-- | Sends a given email.
send :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
     => Maybe MailgunSendOptions -> [Address] -> Mail -> m MessageID
send mo dests m = do
  test  <- view mailgunTestMode
  rndrd <- liftIO $! renderMail' m
  call (MGPost (printf "/v3/%s/messages.mime") [] . mconcat $
    [ mgsoAsMultipart test mo
    , [partLBS "message" rndrd & partFileName .~ Just "message.mime"]
    , map (partText "to" . renderAddress) dests
    ]) (^?key "id"._JSON)

{- This has to be the form API, which leaves less control over attachments?
 - Its unclear if the MIME API  allows recipient-variables.
  -}

type FromAddress  = Address
type CcAddresses  = [Address]
type BccAddresses = [Address]
type Subject      = Text
type HtmlBody     = Text
type TextBody     = Text
type InlineAttachments = [Attachment]
type Attachments       = [Attachment]
type Attachment        = (MimeType, Maybe FileName, BSL.ByteString)

attachmentToMutli :: Bool -> Attachment -> Part
attachmentToMutli inline (contentType, mFileName, body) =
  partLBS (if inline then "inline" else "attachment") body
    & partFileName .~ (fmap T.unpack mFileName)
    & partContentType .~ Just contentType

-- | Takes an email, ignoring the to addresses, and sends it to all the
--   addresses streamed in, paramterized by the JS.Values which can be used
--   in the templating.
sending :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m
          ,JS.ToJSON t)
        => Maybe MailgunSendOptions
        -> FromAddress
        -> CcAddresses
        -> BccAddresses
        -> Subject
        -> These HtmlBody TextBody
        -> InlineAttachments
        -> Attachments
        -> ProcessT m (Address, t) MessageID
sending mo fromAddr ccAddr bccAddr subj theseBodies inline attach =
  buffered 1000 ~> sendBatch
  where
    sendBatch = preplan $ do
      test  <- view mailgunTestMode
      let sharedParts = mconcat $
                        [ mgsoAsMultipart test mo
                        , [partText "from" . renderAddress $ fromAddr]
                        , map (partText "cc" . renderAddress)  ccAddr
                        , map (partText "bcc" . renderAddress) bccAddr
                        , [partText "subject" subj]
                        , mergeTheseWith
                              (pure . partText "html")
                              (pure . partText "text")
                              (++)
                              theseBodies
                        , map (attachmentToMutli True) inline
                        , map (attachmentToMutli False) attach
                        ]
      pure . autoM $ \batch' -> do
        let batch = Map.fromList . map (\tpl@(addr, _) -> (addressEmail addr, tpl)) $ batch'
        call (MGPost (printf "/v3/%s/messages") [] . mconcat $
               [ [partLBS "recipient-variables" . JS.encode .
                    JS.toJSON . fmap snd $ batch
                 ]
               , (map (partText "to") . map (renderAddress . fst) . Map.elems $ batch)
               , sharedParts
               ])
             (^?key "id"._JSON)
