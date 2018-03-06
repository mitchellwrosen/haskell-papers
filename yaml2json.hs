{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

import Control.Monad.State hiding ((>>))
import Data.Aeson
  (FromJSON, ToJSON, (.:), (.:?), (.=), object, toJSON, withObject)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
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
import qualified Data.IntSet as IntSet
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml

data PaperIn = PaperIn
  { paperInTitle :: !Text
  , paperInAuthors :: !(Vector Text)
  , paperInYear :: !(Maybe Int)
  , paperInReferences :: !(Vector Text)
  , paperInLinks :: !(Vector Text)
  }

instance FromJSON PaperIn where
  parseJSON =
    withObject "paper" $ \o -> do
      title <- o .: "title"
      author <- o .:? "author"
      authors <- o .:? "authors"
      year <- o .:? "year"
      references <- o .:? "references"
      link <- o .:? "link"
      links <- o .:? "links"
      pure PaperIn
        { paperInTitle = title
        , paperInAuthors =
            case (author, authors) of
              (Nothing, Nothing) -> mempty
              (Nothing, Just xs) -> xs
              (Just x, Nothing) -> pure x
              (Just _, Just _) -> fail "Found both 'author' and 'authors'"
        , paperInYear = year
        , paperInReferences = fromMaybe mempty references
        , paperInLinks =
            case (link, links) of
              (Nothing, Nothing) -> mempty
              (Nothing, Just xs) -> xs
              (Just x, Nothing) -> pure x
              (Just _, Just _) -> fail "Found both 'link' and 'links'"
        }

-- TODO: authors, year
data PaperOut = PaperOut
  { paperOutTitle :: !Text
  , paperOutReferences :: !(Vector Int)
  , paperOutLinks :: !(Vector Text)
  }

instance ToJSON PaperOut where
  toJSON paper =
    object
      [ "title" .= paperOutTitle paper
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

transform :: Vector PaperIn -> [(Int, PaperOut)]
transform =
  mapM transform1
    >> (`runState` S mempty mempty 0)
    >> smash Vector.toList leftovers
    >> sortOn (snd >> paperOutTitle >> Text.toLower)
 where
  smash :: Monoid c => (a -> c) -> (b -> c) -> (a, b) -> c
  smash f g (x, y) = f x <> g y

  -- All papers that were referenced but don't exist at the top level.
  leftovers :: S -> [(Int, PaperOut)]
  leftovers s =
    sPaperIds s
      & HashMap.toList
      & filter (snd >> (`IntSet.notMember` sTopLevelIds s))
      & map
          (\(title, id) ->
            (id, PaperOut
              { paperOutTitle = title
              , paperOutLinks = mempty
              , paperOutReferences = mempty
              }))

transform1 :: PaperIn -> State S (Int, PaperOut)
transform1 paper = do
  id :: Int <-
    getPaperId (paperInTitle paper)

  ids :: IntSet <-
    gets sTopLevelIds

  if IntSet.member id ids
    then error ("Found " ++ unpack (paperInTitle paper) ++ " twice")
    else modify' (\s -> s { sTopLevelIds = IntSet.insert id ids })

  references :: Vector Int <-
    mapM getPaperId (paperInReferences paper)

  pure
    (id, PaperOut
      { paperOutTitle = paperInTitle paper
      , paperOutLinks = paperInLinks paper
      , paperOutReferences = references
      })

getPaperId :: Text -> State S Int
getPaperId title = do
  S {sPaperIds, sTopLevelIds, sNextPaperId} <- get

  case HashMap.lookup title sPaperIds of
    Nothing -> do
      put S
        { sPaperIds = HashMap.insert title sNextPaperId sPaperIds
        , sTopLevelIds = sTopLevelIds
        , sNextPaperId = sNextPaperId + 1
        }
      pure sNextPaperId
    Just id ->
      pure id

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)
