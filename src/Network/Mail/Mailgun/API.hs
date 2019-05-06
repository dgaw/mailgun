{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Mail.Mailgun.API where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import qualified Data.Aeson as JS
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Machine
import           Network.Mail.Mailgun.Config
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.Wreq
import           Network.Wreq.Types (Postable)
import qualified Network.Wreq as HTTP
import           Text.Printf

data UnparsableResponse
 = UnparsableResponse JS.Value
 deriving (Show)

makePrisms ''UnparsableResponse
instance Exception UnparsableResponse

data MailgunApiError
 = RequestTooLarge
 | MailgunSideError
 | MailgunNotFound
 | UnknownResponseError Int
 deriving (Show)

makePrisms ''MailgunApiError
instance Exception MailgunApiError

data MGRequest
 = MGGet
   { _reqPath   :: DomainName -> String
   , _reqParams :: [(Text, Text)]
   }
 | MGDelete
   { _reqPath   :: DomainName -> String
   , _reqParams :: [(Text, Text)]
   }
 | forall b. Postable b => MGPost
   { _reqPath   :: DomainName -> String
   , _reqParams :: [(Text, Text)]
   , _reqBody   :: b
   }

makeLenses ''MGRequest

yesNo :: Text -> Bool -> HTTP.Part
yesNo t True  = partText t "yes"
yesNo t False = partText t "no"

wreqOptions :: MailgunConfig -> Options
wreqOptions = reader $ \c ->
  defaults & auth ?~ basicAuth (TE.encodeUtf8 "api") (c^.mailgunApiKey)

call :: forall c m r
     . (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
     => MGRequest
     -> (JS.Value -> Maybe r)
     -> m r
call rq respHandle = do
  c <- view mailgunConfig
  let o   = (c^.to wreqOptions) & params .~ (rq^.reqParams) & checkResponse .~ (Just $ \_ _ -> pure ())
  let url = mconcat ["https://", c^.mailgunApiDomain, (rq^.reqPath) (c^.mailgunDomain)]
  resp <- liftIO $ case rq of
           MGGet {} ->
             getWith o url
           MGDelete {} ->
             deleteWith o url
           MGPost {_reqBody=bdy} ->
             postWith o url bdy
  case resp^.responseStatus.statusCode of
    404 -> throwM MailgunNotFound
    413 -> throwM RequestTooLarge
    sts | sts `elem` [500, 502, 503, 504] -> throwM MailgunSideError
    200 -> do
      vr <- asValue resp
      case respHandle (vr^.responseBody) of
        Nothing -> throwM $ UnparsableResponse (vr^.responseBody)
        Just r -> pure r
    sts -> throwM $ UnknownResponseError sts

getStream :: forall t c m r s
          . (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
          => s
          -- ^ The initial start parameter (like @begin@)
          -> (s -> (t, MGRequest))
          -> (t -> JS.Value -> Maybe (Maybe s, [r]))
          -> SourceT m r
getStream seed rqMkr respHandle = construct (go seed)
  where
    go r = do
      let (c, rq) = rqMkr r
      (ms, res) <- lift $ call rq (respHandle c)
      for_ res yield
      for_ ms go

paginatedStream :: forall c m r
                . (HasMailgunConfig c, MonadIO m, MonadThrow m, MonadReader c m)
                => MGRequest -> (JS.Value -> Maybe [r]) -> SourceT m r
paginatedStream rq respHandle = preplan $ do
  dmn <- lift $ view mailgunApiDomain
  let upre = T.pack $ printf "https://%s" dmn
  pure $ getStream Nothing
            (\case
              Nothing -> ((), rq)
              Just s  ->
                ((), MGGet (const . maybe (error "no url pre") T.unpack .
                      T.stripPrefix upre $ s) []))
            (\() j -> let mr = respHandle j
                    in fmap (\case
                         [] -> (Nothing, [])
                         r -> (j^?key "paging".key "next"._JSON, r)) mr)
