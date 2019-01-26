{-# LANGUAGE TemplateHaskell #-}
module Network.Mail.Mailgun.Send where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Reader
import qualified Data.Aeson as JS
import           Data.Ascii
import           Data.HashMap.Strict (HashMap)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Network.Mail.Mailgun.Config
import           Network.Mail.Mime

class IsEmail t where
  emailFrom    :: Lens'  t  Address
  emailTo      :: Getter t [Address]
  emailCC      :: Lens'  t [Address]
  emailBCC     :: Lens'  t [Address]
  emailHeaders :: Lens'  t  Headers
  emailParts   :: Lens'  t [Alternatives]

instance IsEmail Mail where
  emailFrom    = lens mailFrom (\m a -> m {mailFrom=a})
  emailTo      = to mailTo
  emailCC      = lens mailCc  (\m c -> m {mailCc=c})
  emailBCC     = lens mailBcc (\m b -> m {mailBcc=b})
  emailHeaders = lens mailHeaders (\m h -> m {mailHeaders=h})
  emailParts   = lens mailParts (\m p -> m {mailParts=p})

class MailgunSend msg where
  send :: (HasMailgunConfig c, MonadReader c m) => msg -> m ()

-- | 0 to 3 entries
type MailgunTags = [CIAscii]

data MailgunSendOptions
  = MSO
    { _tags              :: MailgunTags
    , _dkim              :: Bool
    , _deliverAt         :: UTCTime
    , _track             :: Bool
    , _trackClicks       :: Bool
    , _trackOpens        :: Bool
    , _skipVerification  :: Bool
    , _templateVariables :: HashMap Text JS.Value
    }
  deriving (Show)

makeClassy ''MailgunSendOptions

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
