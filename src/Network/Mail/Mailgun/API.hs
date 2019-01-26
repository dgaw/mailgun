{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Mail.Mailgun.API where

import           Control.Lens
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Data.ByteString.Lazy (ByteString)
import           Data.List.Lens
import qualified Data.Proxy as Reflection
import           Network.Mail.Mailgun.Config
import qualified Data.Text.Encoding as TE
import           Network.Wreq
import           Network.Wreq.Lens
import           Network.Wreq.Types (Postable, Putable)

wreqOptions :: MailgunConfig -> Options
wreqOptions = reader $ \c ->
  defaults & auth ?~ basicAuth (TE.encodeUtf8 "api") ((TE.encodeUtf8 "key-")<>(c^.mailgunApiKey))

post :: forall c m d r
     . (HasMailgunConfig c, MonadIO m, MonadReader c m, Postable d)
     => String
     -- ^ The path suffix for the API endpoint.
     -> (Response ByteString -> m r)
     -> d
     -- ^ The thing we'll encode as a form to do the API call.
     -> m r
post ps respHandle d = do
  c <- ask
  liftIO (postWith (c^.mailgunConfig.to wreqOptions) ((c^.mailgunApiBase)<>ps) d) >>=
    respHandle
