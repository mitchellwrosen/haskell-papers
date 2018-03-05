{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

import Control.Monad.State hiding ((>>))
import Data.Aeson (FromJSON, ToJSON, (.:), (.=), object, toJSON, withObject)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Monoid
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import Prelude hiding ((>>), id)
import System.Environment
import System.Exit
import System.IO

import qualified Data.Aeson as Json
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Yaml as Yaml

data PaperIn = PaperIn
  { paperInName :: !Text
  , paperInLinks :: !(Vector Text)
  , paperInReferences :: !(Vector Text)
  }

instance FromJSON PaperIn where
  parseJSON =
    withObject "paper" $ \o ->
      PaperIn
        <$> o .: "name"
        <*> o .: "links"
        <*> o .: "references"

data PaperOut = PaperOut
  { paperOutName :: !Text
  , paperOutLinks :: !(Vector Text)
  , paperOutReferences :: !(Vector Int)
  }

instance ToJSON PaperOut where
  toJSON paper =
    object
      [ "name" .= paperOutName paper
      , "links" .= paperOutLinks paper
      , "references" .= paperOutReferences paper
      ]

main :: IO ()
main = do
  yaml <-
    getArgs >>= \case
      [] ->
        ByteString.getContents
      file:_ ->
        ByteString.readFile file
  case Yaml.decodeEither yaml of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right value ->
      value
        & transform
        & Json.encode
        & LByteString.putStr

data S = S
  { sPaperIds :: !(HashMap Text Int)
  , sTopLevelIds :: !IntSet
  , sNextPaperId :: !Int
  }

transform :: Vector PaperIn -> IntMap PaperOut
transform =
  mapM transform1
    >> (`runState` S mempty mempty 0)
    >> smash (foldMap (uncurry IntMap.singleton)) leftovers
 where
  smash :: Monoid c => (a -> c) -> (b -> c) -> (a, b) -> c
  smash f g (x, y) = f x <> g y

  -- All papers that were referenced but don't exist at the top level.
  leftovers :: S -> IntMap PaperOut
  leftovers s =
    sPaperIds s
      & HashMap.toList
      & filter (snd >> (`IntSet.notMember` sTopLevelIds s))
      & foldMap
          (\(name, id) ->
            IntMap.singleton id PaperOut
              { paperOutName = name
              , paperOutLinks = mempty
              , paperOutReferences = mempty
              })

transform1 :: PaperIn -> State S (Int, PaperOut)
transform1 paper = do
  id <- getPaperId (paperInName paper)

  ids <- gets sTopLevelIds
  if IntSet.member id ids
    then error ("Found " ++ unpack (paperInName paper) ++ " twice")
    else modify' (\s -> s { sTopLevelIds = IntSet.insert id ids })

  references <- mapM getPaperId (paperInReferences paper)

  pure
    (id, PaperOut
      { paperOutName = paperInName paper
      , paperOutLinks = paperInLinks paper
      , paperOutReferences = references
      })

getPaperId :: Text -> State S Int
getPaperId name = do
  S {sPaperIds, sTopLevelIds, sNextPaperId} <- get

  case HashMap.lookup name sPaperIds of
    Nothing -> do
      put S
        { sPaperIds = HashMap.insert name sNextPaperId sPaperIds
        , sTopLevelIds = sTopLevelIds
        , sNextPaperId = sNextPaperId + 1
        }
      pure sNextPaperId
    Just id ->
      pure id

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)
