{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

import Control.Monad.State hiding ((>>))
import Control.Monad.ST (ST, runST)
import Data.Aeson
  (FromJSON, ToJSON, (.:), (.:?), (.=), object, toJSON, withObject)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text, unpack)
import Data.Tuple (swap)
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
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Merge as Vector
import qualified Data.Yaml as Yaml

type Title = Text
type Author = Text
type Link = Text

type TitleId = Int
type AuthorId = Int
type LinkId = Int

data PaperIn = PaperIn
  { paperInTitle :: !Title
  , paperInAuthors :: !(Vector Author)
  , paperInYear :: !(Maybe Int)
  , paperInReferences :: !(Vector Title)
  , paperInLinks :: !(Vector Link)
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

data PaperOut = PaperOut
  { paperOutTitle :: !TitleId
  , paperOutAuthors :: !(Vector AuthorId)
  , paperOutYear :: !(Maybe Int)
  , paperOutReferences :: !(Vector TitleId)
  , paperOutLinks :: !(Vector LinkId)
  }

instance ToJSON PaperOut where
  toJSON paper =
    object
      (catMaybes
        [ pure ("title" .= paperOutTitle paper)
        , do
            guard (not (null (paperOutAuthors paper)))
            pure ("authors" .= paperOutAuthors paper)
        , ("year" .=) <$> paperOutYear paper
        , do
            guard (not (null (paperOutReferences paper)))
            pure ("references" .= paperOutReferences paper)
        , do
            guard (not (null (paperOutLinks paper)))
            pure ("links" .= paperOutLinks paper)
        ])

data PapersOut = PapersOut
  { papersOutTitles :: !(IntMap Title)
  , papersOutAuthors :: !(IntMap Author)
  , papersOutLinks :: !(IntMap Link)
  , papersOutPapers :: !(Vector PaperOut)
  }

instance ToJSON PapersOut where
  toJSON papers =
    object
      [ "titles" .= papersOutTitles papers
      , "authors" .= papersOutAuthors papers
      , "links" .= papersOutLinks papers
      , "papers" .= papersOutPapers papers
      ]

main :: IO ()
main = do
  yaml :: ByteString <-
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
  { sTitleIds :: !(HashMap Title TitleId)
  , sAuthorIds :: !(HashMap Author AuthorId)
  , sLinkIds :: !(HashMap Link LinkId)
  , sTopLevelTitles :: !IntSet -- TitleIdSet
  , sNextTitleId :: !TitleId
  , sNextAuthorId :: !AuthorId
  , sNextLinkId :: !LinkId
  }

transform :: Vector PaperIn -> PapersOut
transform =
  mapM transform1
    >> (`runState` S mempty mempty mempty mempty 0 0 0)
    >> ploop
 where
  ploop :: (Vector PaperOut, S) -> PapersOut
  ploop (papers, s) =
    PapersOut
      { papersOutTitles = titles
      , papersOutAuthors =
          foldMap
            (swap >> uncurry IntMap.singleton)
            (HashMap.toList (sAuthorIds s))
      , papersOutLinks =
          foldMap
            (swap >> uncurry IntMap.singleton)
            (HashMap.toList (sLinkIds s))
      , papersOutPapers =
          vectorSortOn
            (paperOutTitle >> flip IntMap.lookup titles >> fmap Text.toLower)
            (papers <> hanging)
      }
   where
    titles :: IntMap Title
    titles =
      foldMap
        (swap >> uncurry IntMap.singleton)
        (HashMap.toList (sTitleIds s))

    -- Papers that were referenced but not defined at the top-level are made
    -- into a 'PaperOut' consisting of only a title.
    hanging :: Vector PaperOut
    hanging =
      sTitleIds s
        & HashMap.elems
        & filter ((`IntSet.member` sTopLevelTitles s) >> not)
        & map fromTitle
        & Vector.fromList
     where
      fromTitle :: TitleId -> PaperOut
      fromTitle id =
        PaperOut
          { paperOutTitle = id
          , paperOutAuthors = mempty
          , paperOutYear = Nothing
          , paperOutReferences = mempty
          , paperOutLinks = mempty
          }

    vectorSortOn :: forall a b. Ord b => (a -> b) -> Vector a -> Vector a
    vectorSortOn f xs =
      runST go
     where
      go :: forall s. ST s (Vector a)
      go = do
        ys <- Vector.thaw xs
        Vector.sortBy (comparing f) ys
        Vector.freeze ys

transform1 :: PaperIn -> State S PaperOut
transform1 paper = do
  title_id :: TitleId <-
    getTitleId (paperInTitle paper)

  title_ids :: IntSet <-
    gets sTopLevelTitles

  if IntSet.member title_id title_ids
    then error ("Duplicate entry: " ++ unpack (paperInTitle paper))
    else modify' (\s -> s { sTopLevelTitles = IntSet.insert title_id title_ids })

  authors :: Vector AuthorId <-
    mapM getAuthorId (paperInAuthors paper)

  references :: Vector TitleId <-
    mapM getTitleId (paperInReferences paper)

  links :: Vector LinkId <-
    mapM getLinkId (paperInLinks paper)

  pure PaperOut
    { paperOutTitle = title_id
    , paperOutAuthors = authors
    , paperOutYear = paperInYear paper
    , paperOutReferences = references
    , paperOutLinks = links
    }

getTitleId :: Title -> State S TitleId
getTitleId title = do
  s <- get

  case HashMap.lookup title (sTitleIds s) of
    Nothing -> do
      put s
        { sTitleIds = HashMap.insert title (sNextTitleId s) (sTitleIds s)
        , sNextTitleId = sNextTitleId s + 1
        }
      pure (sNextTitleId s)
    Just id ->
      pure id

getAuthorId :: Author -> State S AuthorId
getAuthorId author = do
  s <- get

  case HashMap.lookup author (sAuthorIds s) of
    Nothing -> do
      put s
        { sAuthorIds = HashMap.insert author (sNextAuthorId s) (sAuthorIds s)
        , sNextAuthorId = sNextAuthorId s + 1
        }
      pure (sNextAuthorId s)
    Just id ->
      pure id

getLinkId :: Link -> State S LinkId
getLinkId link = do
  s <- get

  case HashMap.lookup link (sLinkIds s) of
    Nothing -> do
      put s
        { sLinkIds = HashMap.insert link (sNextLinkId s) (sLinkIds s)
        , sNextLinkId = sNextLinkId s + 1
        }
      pure (sNextLinkId s)
    Just id ->
      pure id

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)
