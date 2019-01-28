{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Mail.Mailgun.Domains where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import qualified Data.Aeson as JS
import           Data.Aeson ((.:))
import           Data.Aeson.Filthy
import           Data.Aeson.Lens
import           Data.Machine
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Network.Mail.Mailgun.API
import           Network.Mail.Mailgun.Config

data SpamAction
 = SpamDisabled
 | SpamBlock
 | SpamTag
 deriving (Show, Eq, Ord)

makePrisms ''SpamAction

instance JS.FromJSON SpamAction where
  parseJSON = JS.withText "SpamAction" $ \case
              "disabled" -> pure SpamDisabled
              "block" -> pure SpamBlock
              "tag" -> pure SpamTag
              s -> fail $ "Unknown SpamAction "++show s

instance JS.ToJSON SpamAction where
  toJSON SpamDisabled = JS.String "disabled"
  toJSON SpamBlock    = JS.String "block"
  toJSON SpamTag      = JS.String "tag"

data DomainType
 = CustomDomain
 | SandboxDomain
 deriving (Show, Eq, Ord)

makePrisms ''DomainType

instance JS.FromJSON DomainType where
  parseJSON = JS.withText "DomainType" $ \case
              "custom" -> pure CustomDomain
              "sandbox" -> pure SandboxDomain
              t -> fail $ "Unknown DomainType "++show t

instance JS.ToJSON DomainType where
  toJSON CustomDomain  = JS.String "custom"
  toJSON SandboxDomain = JS.String "sandbox"

data Domain
 = Domain
   { _domainCreated    :: UTCTime
   , _domainSmtpLogin  :: Text
   , _domainSmtpPass   :: Text
   , _domainName       :: Text
   , _domainWildcard   :: Bool
   , _domainSpamAction :: SpamAction
   , _domainActive     :: Bool
   , _domainType       :: DomainType
   }
 deriving (Show)

makeClassy ''Domain

instance JS.FromJSON Domain where
  parseJSON = JS.withObject "Domain" $ \v -> Domain
              <$> (fromRFC2822Time <$> v .: "created_at")
              <*> v .: "smtp_login"
              <*> v .: "smtp_password"
              <*> v .: "name"
              <*> v .: "wildcard"
              <*> v .: "spam_action"
              <*> ((==("active"::Text)) <$> v .: "state")
              <*> v .: "type"

instance JS.ToJSON Domain where
  toJSON d = JS.object
             [("created_at", d^.domainCreated.(to RFC2822Time).to JS.toJSON)
             ,("smtp_login", d^.domainSmtpLogin.to JS.toJSON)
             ,("smtp_password", d^.domainSmtpPass.to JS.toJSON)
             ,("name", d^.domainName.to JS.toJSON)
             ,("wildcard", d^.domainWildcard.to JS.toJSON)
             ,("spam_action", d^.domainSpamAction.to JS.toJSON)
             ,("state", JS.String $ if d^.domainActive then "active" else "unverified")
             ,("type", d^.domainType.to JS.toJSON)
             ]

getDomains :: (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m) => SourceT m Domain
getDomains =
  getStream 0
    (\skip -> (skip, MGGet (const "/v3/domains") [("skip", T.pack $ show skip)
                                                ,("limit", T.pack $ show pagingSize)]))
    (\skipped respVal ->
       let mrs = respVal^?key "items"._JSON
       in fmap (\rs -> (if length rs == pagingSize then Just (skipped+length rs) else Nothing
                      ,rs)) mrs)
  where
    pagingSize = 500
