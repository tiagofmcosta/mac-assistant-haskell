{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( entry,
  )
where

import Control.Concurrent
import Control.Lens
import Control.Logger.Simple
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.Map as M (Map, fromList, lookup)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Client (RequestBody (RequestBodyIO), streamFile)
import Network.URI.Encode as UE (encodeText)
import Network.Wreq
import System.Exit
import System.IO
import Turtle (empty, shells)

-- TYPES

newtype Intent = Intent {name :: T.Text}
  deriving (Show, Generic)

instance FromJSON Intent

instance ToJSON Intent

newtype Entity = Entity {value :: T.Text}
  deriving (Show, Generic)

instance FromJSON Entity

instance ToJSON Entity

data WitAiResponse = WitAiResponse
  { intents :: [Intent],
    entities :: M.Map T.Text [Entity]
  }
  deriving (Show, Generic)

instance FromJSON WitAiResponse

instance ToJSON WitAiResponse

--type Resp = Response WitAiResponse

-- FUNCTIONS

getRawPayload :: Payload
getRawPayload = Raw "audio/mpeg3" $ RequestBodyIO $ streamFile "audiocapture.mp3"

supportedCommands :: [T.Text]
supportedCommands = ["search_website"]

supportedWebsites :: M.Map T.Text T.Text
supportedWebsites =
  M.fromList
    [ ("youtube", "https://www.youtube.com/results?search_query={query}"),
      ("google", "https://www.google.com/search?q={query}&oq={query}&ie=UTF-8"),
      ("wikipedia", "https://en.wikipedia.org/w/index.php?title=Special:Search&search={query}")
    ]

getWebsiteUriTemplate :: Maybe T.Text -> Maybe T.Text
getWebsiteUriTemplate Nothing = Nothing
getWebsiteUriTemplate (Just k) = M.lookup k supportedWebsites

getEncodedSearchUri :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text
getEncodedSearchUri Nothing _ = Nothing
getEncodedSearchUri _ Nothing = Nothing
getEncodedSearchUri (Just template) (Just query) = Just $ T.replace "{query}" (UE.encodeText query) template

getWitAiSpeechApiUriForDate :: String -> String
getWitAiSpeechApiUriForDate = (++) "https://api.wit.ai/speech?v="

unableToUnderstandCommand :: IO ()
unableToUnderstandCommand = do
  putStrLn "Was not able to understand your request\n"
  waitForUserInput True

waitForUserInput :: Bool -> IO ()
waitForUserInput printMenu = do
  when printMenu $ do
    putStrLn "OPTIONS"
    putStrLn "* Press ENTER for a new command"
    putStrLn "* Press 'h' to see this menu"
    putStrLn "* Press 'q' to exit"

  hSetBuffering stdin NoBuffering
  c <- getChar

  case toLower c of
    '\n' -> entry
    'h' -> waitForUserInput True
    'q' -> exitSuccess
    _ -> waitForUserInput False

-- ENTRY

witAiToken :: B.ByteString
witAiToken = "4GTUGTL2A2DEQVHX7Q4DGZW7KRTLUOMS"

entry :: IO ()
entry = withGlobalLogging (LogConfig Nothing True) $ do
  let opts =
        defaults
          & auth ?~ oauth2Bearer witAiToken

  putStrLn "Please speak your command..."

  shells "ffmpeg -v 8 -f avfoundation -i \":default\" -t 5 audiocapture.mp3 -y" empty

  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  let formattedToday = formatTime defaultTimeLocale "%_Y%m%d" today
  let witAiSpeechApiUri = getWitAiSpeechApiUriForDate formattedToday

  --  response <- asJSON =<< postWith opts witAiSpeechApiUri getRawPayload :: IO Resp
  --  let witAiResponse = response ^. responseBody

  response <- postWith opts witAiSpeechApiUri getRawPayload

  let eitherResponse = eitherDecode (response ^. responseBody) :: Either String WitAiResponse

  case eitherResponse of
    Left err -> do
      logError $ T.pack err
      unableToUnderstandCommand
    Right parsedResponse -> do
      let witAiResponse = parsedResponse

      -- logDebug $ T.pack $ show witAiResponse

      let intentsFromResponse = intents witAiResponse
      when (null intentsFromResponse) unableToUnderstandCommand

      let intentName = name . head $ intentsFromResponse
      unless (intentName `elem` supportedCommands) unableToUnderstandCommand

      let entitiesFromResponse = entities witAiResponse
      let websiteName = value . head <$> M.lookup "website_name:website_name" entitiesFromResponse
      let websiteUriTemplate = getWebsiteUriTemplate websiteName
      let searchQuery = value . head <$> M.lookup "search_term:search_term" entitiesFromResponse
      let encodedSearchUri = getEncodedSearchUri websiteUriTemplate searchQuery

      when (isNothing encodedSearchUri) unableToUnderstandCommand

      putStrLn $ "Searching for '" ++ T.unpack (fromMaybe "" searchQuery) ++ "' on '" ++ T.unpack (fromMaybe "" websiteName) ++ "'\n"

      threadDelay 2000000

      let searchUri = fromMaybe "" encodedSearchUri
      shells [i|open '#{searchUri}' -a 'Google Chrome Canary'|] empty

      waitForUserInput True
