{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections       #-}

import Control.Concurrent
  (MVar, QSem, newMVar, newQSem, signalQSem, waitQSem, withMVar)
import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Exception (bracket_)
import Control.Monad (forM, forM_)
import Data.Aeson
import Data.Vector (Vector)
import Network.HTTP.Client
  (Manager, Request, Response, httpNoBody, parseRequest,requestHeaders,
    responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)

import qualified Data.ByteString as ByteString

newtype Links
  = Links (Vector [Char])

instance FromJSON Links where
  parseJSON =
    withObject "papers" $ \o -> do
      links :: Vector (Int, [Char]) <-
        o .: "links"
      pure (Links (snd <$> links))

main :: IO ()
main = do
  Just (Links links) <-
    decodeStrict <$> ByteString.readFile "papers.json"

  putStrLn ("Fetching " ++ show (length links) ++ " links")

  manager :: Manager <-
    newTlsManager

  -- Limit the number of concurrent outgoing HTTP requests
  sem :: QSem <-
    newQSem 4

  -- Prevent threads from printing to stdout at the same time
  iolock :: MVar () <-
    newMVar ()

  handles :: Vector (Async (), [Char]) <-
    forM links $ \link ->
      fmap (, link) . async . bracket_ (waitQSem sem) (signalQSem sem) $ do
        request :: Request <-
          parseRequest link

        response :: Response () <-
          httpNoBody
            request
              { requestHeaders = chromeUserAgent : requestHeaders request }
            manager

        case statusCode (responseStatus response) of
          200 ->
            pure ()
          code ->
            withMVar iolock (\_ -> putStrLn (show code ++ " " ++ link))

  forM_ handles $ \(handle, link) ->
    waitCatch handle >>= \case
      Left _ ->
        withMVar iolock (\_ -> putStrLn ("??? " ++ link))
      Right _ ->
        pure ()

chromeUserAgent :: Header
chromeUserAgent =
  ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36")
