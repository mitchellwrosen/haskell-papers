{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections       #-}

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Exception (bracket_, fromException, mask_)
import Control.Monad
import Data.Aeson
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Vector (Vector)
import GHC.Event (TimerManager, getSystemTimerManager, registerTimeout)
import Network.HTTP.Client
  (HttpException(..), Manager, Request, Response, httpNoBody, method,
    parseRequest, requestHeaders, responseStatus)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import Network.URI
  (URI(URI), URIAuth(URIAuth), parseURI, uriAuthority, uriRegName)
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap

newtype Links
  = Links (Vector Link)

type Link
  = [Char]

instance FromJSON Links where
  parseJSON =
    withObject "papers" $ \o -> do
      links :: Vector (Int, Link) <- o .: "c"
      pure (Links (snd <$> links))

main :: IO ()
main = do
  [file :: FilePath] <-
    getArgs

  Just (Links links) <-
    decodeStrict <$> ByteString.readFile file

  hSetBuffering stdout LineBuffering

  manager :: Manager <-
    newTlsManager

  -- Limit the number of concurrent outgoing HTTP requests
  sem :: QSem <-
    newQSem 50

  -- Count the number of links that have been fetched
  bumpCount :: IO Int <- do
    ref <- newIORef 0
    pure (atomicModifyIORef' ref (\n -> (n+1, n+1)))

  handles :: Vector (Async (), [Char]) <-
    forM links $ \link ->
      fmap (, link) . async $
        case parseURI link of
          Just (URI {uriAuthority = Just (URIAuth {uriRegName = domain})}) ->
            withDomain domain $
              withSem sem $ do
                request :: Request <-
                  parseRequest link

                response :: Response () <-
                  httpNoBody
                    request
                      { method =
                          "HEAD"
                      , requestHeaders =
                          chromeUserAgent : requestHeaders request
                      }
                    manager

                n :: Int <-
                  bumpCount

                putln $
                  show n ++ " " ++ show (statusCode (responseStatus response))
                    ++ " " ++ link
          _ ->
            error ("Cannot parse URI: " ++ link)

  forM_ handles $ \(handle, link) ->
    waitCatch handle >>= \case
      Left ex -> do
        n :: Int <-
          bumpCount

        let s =
              case fromException ex of
                Just (HttpExceptionRequest _ content) ->
                  show content
                Just (InvalidUrlException _ reason) ->
                  reason
                Nothing ->
                  show ex

        putln (show n ++ " 999 " ++ link ++ " [" ++ s ++ "]")
      Right _ ->
        pure ()

withDomain :: [Char] -> IO a -> IO a
withDomain domain action = do
  lock :: MVar () <- do
    lock :: MVar () <-
      newMVar ()
    atomicModifyIORef'
      domainLocks
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

withSem :: QSem -> IO a -> IO a
withSem sem =
  bracket_ (waitQSem sem) (signalQSem sem)

putln :: String -> IO ()
putln =
  putStrLn
    >>> const
    >>> withMVar iolock

--------------------------------------------------------------------------------
-- Global variables

-- Prevent threads from printing to stdout at the same time
iolock :: MVar ()
iolock =
  unsafePerformIO (newMVar ())
{-# NOINLINE iolock #-}

-- Only hit each domain once every ten seconds to avoid an IP ban
domainLocks :: IORef (HashMap [Char] (MVar ()))
domainLocks =
  unsafePerformIO (newIORef mempty)
{-# NOINLINE domainLocks #-}
