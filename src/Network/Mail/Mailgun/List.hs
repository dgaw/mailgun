{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Mail.Mailgun.List where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson ((.:))
import qualified Data.Aeson as JS
import           Data.Aeson.Lens
import           Data.Aeson.Filthy
import           Data.Machine
import qualified Data.Proxy as Proxy
import           Data.Text
import           Data.Time
import           Network.Mail.Mailgun.API
import           Network.Mail.Mailgun.Config
import           Network.Wreq
import           Text.Printf

data AccessLevel
 = AccessReadonly
 | AccessMembers
 | AccessEveryone
 deriving (Eq, Ord, Show)

encodeAccessLevel :: AccessLevel -> Text
encodeAccessLevel AccessReadonly = "readonly"
encodeAccessLevel AccessMembers  = "members"
encodeAccessLevel AccessEveryone = "everyone"

instance JS.FromJSON AccessLevel where
  parseJSON = JS.withText "AccessLevel" $ \case
              "readonly" -> pure AccessReadonly
              "members"  -> pure AccessMembers
              "everyone" -> pure AccessEveryone
              t -> fail $ "Unknown AccessLevel "++show t

instance JS.ToJSON AccessLevel where
  toJSON = JS.String . encodeAccessLevel

data MailingList f
 = MailingList
   { _listAccessLevel :: AccessLevel
   , _listAddress     :: Text
   , _listName        :: Text
   , _listDescription :: Text
   , _listCreated     :: f UTCTime
   , _listMemberCount :: f Integer
   }

makeLenses ''MailingList

instance Show (MailingList Identity) where
  show (MailingList al a n d c mc) =
    printf "MailingList %s %s %s %s %s %s"
           (show al) a n d (show c) (show mc)

instance Applicative f => JS.FromJSON (MailingList f) where
  parseJSON = JS.withObject "MailingList" $ \v -> MailingList
    <$>  v .: "access_level"
    <*>  v .: "address"
    <*>  v .: "name"
    <*>  v .: "description"
    <*> (pure . fromRFC2822Time <$> v .: "created_at")
    <*> (pure <$> v .: "members_count")

instance JS.ToJSON (MailingList Identity) where
  toJSON ml = JS.object
    [ ("access_level", ml^.listAccessLevel.to JS.toJSON)
    , ("address", ml^.listAddress.to JS.toJSON)
    , ("name", ml^.listName.to JS.toJSON)
    , ("description", ml^.listDescription.to JS.toJSON)
    , ("created_at", ml^.listCreated.to (RFC2822Time . runIdentity).to JS.toJSON)
    , ("members_count", ml^.listMemberCount.to runIdentity.to JS.toJSON)
    ]

instance JS.ToJSON (MailingList Proxy.Proxy) where
  toJSON ml = JS.object
    [ ("access_level", ml^.listAccessLevel.to JS.toJSON)
    , ("address", ml^.listAddress.to JS.toJSON)
    , ("name", ml^.listName.to JS.toJSON)
    , ("description", ml^.listDescription.to JS.toJSON)
    ]

data ListMember v
 = ListMember
   { _lmName :: Text
   , _lmAddress :: Text
   , _lmSubscribed :: Bool
   , _lmExtra :: v
   }
  deriving (Eq, Ord, Show)

makeLenses ''ListMember

instance JS.FromJSON v => JS.FromJSON (ListMember v) where
  parseJSON = JS.withObject "MailingList" $ \v -> ListMember
    <$> v .: "name"
    <*> v .: "address"
    <*> v .: "subscribed"
    <*> v .: "vars"

instance JS.ToJSON v => JS.ToJSON (ListMember v) where
  toJSON lm = JS.object
    [ ("name", lm^.lmName.to JS.toJSON)
    , ("address", lm^.lmAddress.to JS.toJSON)
    , ("subscribed", lm^.lmSubscribed.to JS.toJSON)
    , ("vars", lm^.lmExtra.to JS.toJSON)
    ]

createList :: (HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
           => MailingList Proxy.Proxy -> m ()
createList ml =
  call (MGPost (const "/v3/lists") []
        [ partText "address"      (ml^.listAddress)
        , partText "name"         (ml^.listName)
        , partText "description"  (ml^.listDescription)
        , partText "access_level" (ml^.listAccessLevel.to encodeAccessLevel)
        ])
       (const $ Just ())

getLists :: (HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
         => SourceT m (MailingList Identity)
getLists = paginatedStream (MGGet (const "/v3/lists/pages") []) (^.key "items"._JSON)

getList :: (HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
        => Text -> m (MailingList Identity)
getList addr =
  call (MGGet (const $ printf "/v3/lists/%s" addr) [])
       (^?key "list"._JSON)

removeList :: (HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
           => Text -> m ()
removeList ml =
  call (MGDelete (const $ printf "/v3/lists/%s" ml) [])
       (const $ Just ())

listMembers :: (JS.ToJSON v, JS.FromJSON v
              ,HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
            => Maybe Bool -> Text -> SourceT m (ListMember v)
listMembers msubbed ml =
  paginatedStream (MGGet (const $ printf "/v3/lists/%s/members/pages" ml) . mconcat $
                         [ maybe [] (\s -> [("subscribed", if s then "yes" else "no")]) msubbed
                         , [ ("limit", "1000") ]
                         ])
                  (^.key "items"._JSON)

getMember :: (JS.ToJSON v, JS.FromJSON v
            ,HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
          => Text -> Text -> m (ListMember v)
getMember ml mbr =
  call (MGGet (const $ printf "/v3/lists/%s/members/%s" ml mbr) [])
       (^?key "member"._JSON)

removeMember :: (HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
          => Text -> Text -> m ()
removeMember ml mbr =
  call (MGDelete (const $ printf "/v3/lists/%s/members/%s" ml mbr) [])
       (const $ Just ())

addMembers :: (JS.ToJSON v, JS.FromJSON v
              ,HasMailgunConfig c, MonadReader c m, MonadIO m, MonadThrow m)
           => Bool -> Text -> ProcessT m (ListMember v) ()
addMembers upsert ml =
  buffered 1000 ~> addMemberBatch
  where
    addMemberBatch = autoM $ \batch -> do
      call (MGPost (const $ printf "/v3/lists/%s/members.json" ml) []
            [ yesNo "upsert" upsert
            , partLBS "members" $ JS.encode batch
            ])
           (^?key "list"._JSON)
