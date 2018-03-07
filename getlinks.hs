{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections       #-}

import Control.Concurrent
import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Exception (bracket_, mask_)
import Control.Monad (forM, forM_)
import Data.Aeson
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Vector (Vector)
import GHC.Event (TimerManager, getSystemTimerManager, registerTimeout)
import Network.HTTP.Client
  (Manager, Request, Response, httpNoBody, parseRequest,requestHeaders,
    responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import Network.URI
  (URI(URI), URIAuth(URIAuth), parseURI, uriAuthority, uriRegName)

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap

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

  -- Only hit each domain once every ten seconds to avoid an IP ban
  domainLocks :: IORef (HashMap [Char] (MVar ())) <-
    newIORef mempty

  -- Limit the number of concurrent outgoing HTTP requests
  sem :: QSem <-
    newQSem 4

  -- Prevent threads from printing to stdout at the same time
  iolock :: MVar () <-
    newMVar ()

  handles :: Vector (Async (), [Char]) <-
    forM links $ \link ->
      fmap (, link) . async $
        case parseURI link of
          Just (URI {uriAuthority = Just (URIAuth {uriRegName = domain})}) ->
            withDomain domainLocks domain $
              bracket_ (waitQSem sem) (signalQSem sem) $ do
                request :: Request <-
                  parseRequest link

                response :: Response () <-
                  httpNoBody
                    request
                      { requestHeaders =
                          chromeUserAgent : requestHeaders request }
                    manager

                case statusCode (responseStatus response) of
                  200 ->
                    pure ()
                  code ->
                    withMVar iolock (\_ -> putStrLn (show code ++ " " ++ link))
          _ ->
            error ("Cannot parse URI: " ++ link)

  forM_ handles $ \(handle, link) ->
    waitCatch handle >>= \case
      Left _ ->
        withMVar iolock (\_ -> putStrLn ("??? " ++ link))
      Right _ ->
        pure ()

withDomain :: IORef (HashMap [Char] (MVar ())) -> [Char] -> IO a -> IO a
withDomain ref domain action = do
  lock :: MVar () <- do
    lock :: MVar () <-
      newMVar ()
    atomicModifyIORef'
      ref
      (\locks ->
        case HashMap.lookup domain locks of
          Nothing ->
            (HashMap.insert domain lock locks, lock)
          Just lock' ->
            (locks, lock'))

  manager :: TimerManager <-
    getSystemTimerManager

  mask_ $ do
    -- We can still receive an async exception here (takeMVar is interruptible)
    takeMVar lock

    -- But if we take the lock we're guaranteed not to die before scheduling its
    -- release
    void (registerTimeout manager (10*1000*1000) (putMVar lock ()))

  action

chromeUserAgent :: Header
chromeUserAgent =
  ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36")
