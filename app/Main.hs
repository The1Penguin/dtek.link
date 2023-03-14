{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Web.Scotty                           hiding (Options)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors
import           Control.Monad.IO.Class
import           Network.HTTP.Types.Status
import           Data.Aeson                           hiding (Options)
import           Data.Aeson.Types                     hiding (Options)
import           Data.Text.Internal.Lazy
import           GHC.Generics                         hiding (to, from)

import           Network.HTTP.Simple
import System.Environment (getEnv)

data Redirects = Redirects
  {
    from :: Text
  , to   :: Text
  } deriving (Show, Generic)

instance FromJSON Redirects where
  parseJSON (Object v) = Redirects <$>
    v .: "url" <*>
    v .: "link"
  parseJSON invalid    =
    prependFailure "parsing JSONResponse failed, "
      (typeMismatch "Object" invalid)

newtype Data = Data {redirs :: [Redirects]}
  deriving (Show, Generic)

instance FromJSON Data where
  parseJSON (Object v) = Data <$>
    v .: "data"
  parseJSON invalid    =
    prependFailure "parsing JSONResponse failed, "
      (typeMismatch "Object" invalid)


routes :: String -> ScottyM ()
routes address = do
  middleware logStdout
  middleware simpleCors
  
  get "/:word" $ do
    url :: Text <- param "word" 
    redirects <- liftIO $ fetchRedirs address
    let link = to $ head $ filter ((url ==) . from) redirects
    redirect link

  notFound $ 
    status $ mkStatus 404 "Link not found"

fetchRedirs :: String -> IO [Redirects]
fetchRedirs address = do
    req <- parseRequest address
    res <- httpJSON req
    return $ redirs (getResponseBody res :: Data)

main :: IO ()
main = scotty 3000 . routes =<< getEnv "API"
