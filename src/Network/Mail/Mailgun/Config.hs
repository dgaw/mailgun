{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Mail.Mailgun.Config
 ( DomainName
 , MailgunConfig(..)
 , HasMailgunConfig(..)
 , mailgunGetConfig
 , mailgunFromEnv, mailgunFromIni
 , MailgunConfigException(..)
 , _MailgunApiKeyRequired, _MailgunDomainRequired
 , _MailgunInvalidRegion, _MailgunIniNotFound
 , _MailgunConextUnavailable
 ) where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import           Data.Foldable
import           Data.Ini
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Environment
import           System.FilePath

data MailgunConfigException
 = MailgunApiKeyRequired
 | MailgunDomainRequired
 | MailgunInvalidRegion
 | MailgunIniNotFound
 | MailgunConextUnavailable
 deriving (Show)

instance Exception MailgunConfigException

makePrisms ''MailgunConfigException

type DomainName = String

-- | The configuration we use when accessing the Mailgun API.
data MailgunConfig
 = MailgunConfig
   { _mailgunDomain    :: DomainName
     -- ^ The domain we're using mailgun with.
   , _mailgunApiKey    :: BS.ByteString
     -- ^ Our mailgun API key.
   , _mailgunApiDomain :: String
     -- ^ The base URL for the mailgun API, usually "https://api.mailgun.net"
   , _mailgunTestMode  :: Bool
   }
 deriving (Show, Eq)

makeClassy ''MailgunConfig

-- | The domain for the API URL used for the US region.
usApiDomain :: String
usApiDomain = "api.mailgun.net"

-- | The domain for the API URL used for the EU region.
euApiDomain :: String
euApiDomain = "api.eu.mailgun.net"

-- | Uses the available options to discover a MaingunConfig if possible.
mailgunGetConfig :: (MonadIO m, MonadCatch m) => m MailgunConfig
mailgunGetConfig = do
  ec <- runMaybeT . msum .
       map (\act -> act `catch` (\(_::MailgunConfigException) -> MaybeT $ pure Nothing)) $
       [ mailgunFromEnv
       , mailgunFromIni
       ]
  maybe (throwM MailgunConextUnavailable) pure ec

-- | Builds a MaingunConfig from enviromental variables.
--
--   MAILGUN_API_KEY:  Required; the API key for mailgun.
--   MAILGUN_DOMAIN:   Required; the domain in mailgun we're using.
--   MAILGUN_REGION:   Optional; Selects teh regional API endpoint.
--                               Valid values are 'US', and 'EU', defaults to 'US'.
--   MAILGUN_API_BASE: Optional; Override the base URL (primarily for testing).
--                               Takes presidence over MAILGUN_REGION.
--   MAILGUN_LIVE:     Optional: Unless set to True, set to test mode.
--                               In test mode Mailgun accepts but does not send messages. 
mailgunFromEnv :: (MonadIO m, MonadThrow m) => m MailgunConfig
mailgunFromEnv = do
  apiKey  <- maybe (throwM MailgunApiKeyRequired) (pure . TE.encodeUtf8 . T.pack) =<<
            liftIO (lookupEnv "MAILGUN_API_KEY")
  domain  <- maybe (throwM MailgunDomainRequired) pure =<<
            liftIO (lookupEnv "MAILGUN_DOMAIN")
  testmode <- ((Just "True")==) <$> liftIO (lookupEnv "MAILGUN_DOMAIN")
  apiDomain  <- liftIO (lookupEnv "MAILGUN_API_BASE") >>= \case
                 Just ab -> pure ab
                 Nothing ->
                   liftIO (lookupEnv "MAILGUN_REGION") >>= \case
                     Nothing   -> pure usApiDomain
                     Just "US" -> pure usApiDomain
                     Just "EU" -> pure euApiDomain
                     _    -> throwM MailgunInvalidRegion
  pure $ MailgunConfig domain apiKey apiDomain testmode

-- | Looks for an ini format file at ".mailgun" and "~/.mailgun" in that order.
--   Credentials are read from the ini in the format:
--   @
--   [mailgun]
--   region: US
--   domain: mydomain.com
--   key: 3ax6xnjp29jd6fds4gc373sgvjxteol0
--   api_domain: api.mailgun.com
--   live: True
--   @
--
--   The API key and domain are required, other values are optional.
mailgunFromIni :: forall m . (MonadIO m, MonadThrow m) => m MailgunConfig
mailgunFromIni = do
  ini <- (maybe (throwM MailgunIniNotFound) pure . asum) =<<
        liftIO (sequence
                [ readIniFileMay ".mailgun"
                , lookupEnv "HOME" >>=
                    maybe (pure Nothing) (\h -> readIniFileMay (h </> ".mailgun"))
                ])
  apiKey <- TE.encodeUtf8 . T.pack <$> lookupMailgun MailgunApiKeyRequired ini "key"
  domain <- lookupMailgun MailgunApiKeyRequired ini "domain"
  let testmode = (Just "True") == lookupMailgunMay ini "live"
  let apiDomain  = fromMaybe usApiDomain $
                   (lookupMailgunMay ini "api_domain")
                   <|> ((\case
                              "US" -> usApiDomain
                              "EU" -> euApiDomain)
                       <$> (lookupMailgunMay ini "region"))
  pure $ MailgunConfig domain apiKey apiDomain testmode
  where
    readIniFileMay :: FilePath -> IO (Maybe Ini)
    readIniFileMay fp = (maybeRight <$> readIniFile fp) `catchIOError` (const (pure Nothing))
    lookupMailgun :: MailgunConfigException -> Ini -> Text -> m String
    lookupMailgun e ini key = maybe (throwM e) pure $ lookupMailgunMay ini key
    lookupMailgunMay :: Ini -> Text -> Maybe String
    lookupMailgunMay ini key =
      either (const Nothing) (Just . T.unpack) $ lookupValue "mailgun" key ini
    maybeRight :: Either a b -> Maybe b
    maybeRight (Left _)  = Nothing
    maybeRight (Right b) = Just b
