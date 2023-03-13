{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Web.Scotty                           hiding (Options)
import           WithCli
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors
import           Control.Monad.IO.Class
import           Network.HTTP.Types.Status
import           Data.Aeson                           hiding (Options)
import           Data.Aeson.Types                     hiding (Options)
import           Data.Text.Internal.Lazy
import           Data.Text                            hiding (Text, head, filter)
import           Network.HTTP.Simple

data Options = Options
  {
    port :: Int
  , apiURL :: String
  } deriving (Show, Generic, HasArguments)

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

data Data = Data {redirs :: [Redirects]}
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
    request <- parseRequest address
    res <- httpJSON request
    return $ redirs (getResponseBody res :: Data)

main :: IO ()
main = withCli run

run :: Options -> IO ()
run (Options port api) =
  scotty port $ routes api
