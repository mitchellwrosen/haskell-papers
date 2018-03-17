{-# language InstanceSigs        #-}
{-# language LambdaCase          #-}
{-# language NamedFieldPuns      #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}

import Control.Monad.State hiding ((>>))
import Control.Monad.ST (ST, runST)
import Data.Aeson
  (FromJSON, ToJSON, Value, (.:), (.:?), (.=), object, toJSON, withObject)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid (Endo(..))
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text, unpack)
import Data.Tuple (swap)
import Data.Vector (Vector)
import Prelude hiding ((>>), id)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import qualified Data.Aeson as Json
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Merge as Vector
import qualified Data.Yaml.Combinators as Yaml

type Author = Text
type Link = Text
type Title = Text

type AuthorId = Int
type LinkId = Int
type TitleId = Int

type AuthorIdMap a = IntMap a

type TitleIdSet = IntSet

-- | A thing that came from a line in a file.
data Loc a = Loc
  { locFile :: !Int
  , locLine :: !Int
  , locValue :: a
  }

-- | Paper object read from @papers.yaml@.
data PaperIn = PaperIn
  { paperInTitle :: !Title
  , paperInAuthors :: !(Vector Author)
  , paperInYear :: !(Maybe Int)
  , paperInReferences :: !(Vector Title)
  , paperInLinks :: !(Vector Link)
  }

paperInParser :: Yaml.Parser PaperIn
paperInParser =
  Yaml.validate
    (Yaml.object
      ((,,,,,,)
        <$> Yaml.field "title" Yaml.string
        <*> Yaml.optField "author" Yaml.string
        <*> Yaml.optField "authors" (uniqArray Yaml.string)
        <*> Yaml.optField "year" Yaml.integer
        <*> Yaml.optField "references" (uniqArray Yaml.string)
        <*> Yaml.optField "link" Yaml.string
        <*> Yaml.optField "links" (uniqArray Yaml.string)))
    validate
 where
  validate
    :: ( Title, Maybe Author, Maybe (Vector Author), Maybe Int
       , Maybe (Vector Title), Maybe Link, Maybe (Vector Link)
       )
    -> Either [Char] PaperIn
  validate (title, mauthor, mauthors, year, references, mlink, mlinks) = do
    authors :: Vector Author <-
      case (mauthor, mauthors) of
        (Nothing, Nothing) -> pure mempty
        (Nothing, Just xs) -> pure xs
        (Just x, Nothing) -> pure (pure x)
        (Just _, Just _) -> Left "a paper without both 'author' and 'authors'"

    links :: Vector Link <-
      case (mlink, mlinks) of
        (Nothing, Nothing) -> pure mempty
        (Nothing, Just xs) -> pure xs
        (Just x, Nothing) -> pure (pure x)
        (Just _, Just _) -> Left "a paper without both 'link' and 'links'"

    pure PaperIn
      { paperInTitle = title
      , paperInAuthors = authors
      , paperInYear = year
      , paperInReferences = fromMaybe mempty references
      , paperInLinks = links
      }

  -- TODO: Reject arrays with duplicate elements
  uniqArray :: Yaml.Parser a -> Yaml.Parser (Vector a)
  uniqArray =
    Yaml.array

instance FromJSON PaperIn where
  parseJSON :: Value -> Parser PaperIn
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

-- | Paper object written to @papers.json@.
data PaperOut = PaperOut
  { paperOutTitle :: !TitleId
    -- ^ Paper title.
  , paperOutAuthors :: !(Vector AuthorId)
    -- ^ Paper authors.
  , paperOutYear :: !(Maybe Int)
    -- ^ Paper year.
  , paperOutReferences :: !(Vector TitleId)
    -- ^ Paper references.
  , paperOutCitations :: TitleIdSet
    -- ^ Papers that reference this paper. Note: lazy on purpose!
  , paperOutLinks :: !(Vector LinkId)
    -- ^ Paper links.
  , paperOutFile :: !Int
    -- ^ Yaml file the paper was parsed from, e.g. 1 means "papers001.yaml"
  , paperOutLine :: !Int
    -- ^ Line number in yaml file.
  }

instance ToJSON PaperOut where
  toJSON :: PaperOut -> Value
  toJSON paper =
    object
      (catMaybes
        [ pure ("a" .= paperOutTitle paper)
        , do
            guard (not (null (paperOutAuthors paper)))
            pure ("b" .= paperOutAuthors paper)
        , ("c" .=) <$> paperOutYear paper
        , do
            guard (not (null (paperOutReferences paper)))
            pure ("d" .= paperOutReferences paper)
        , do
            guard (not (null (paperOutLinks paper)))
            pure ("e" .= paperOutLinks paper)
        , pure ("f" .= paperOutFile paper)
        , pure ("g" .= paperOutLine paper)
        , do
            guard (not (IntSet.null (paperOutCitations paper)))
            pure ("h" .= paperOutCitations paper)
        ])

-- | The entire @papers.json@ blob:
--
--   - Lookup tables for strings that we need not include over and over
--     (authors, titles, etc).
--   - Inverted index of authors
--   - Array of papers
--
data PapersOut = PapersOut
  { papersOutTitles :: !(IntMap Title)
  , papersOutAuthors :: !(IntMap Author)
  , papersOutLinks :: !(IntMap Link)
  , papersOutAuthorsIndex :: !(AuthorIdMap TitleIdSet)
  , papersOutPapers :: !(Vector PaperOut)
    -- ^ Invariant: a 'PaperOut's title, author, references, etc. will always
    -- be in 'IntMap's above.
  }

instance ToJSON PapersOut where
  toJSON papers =
    object
      [ "a" .= papersOutTitles papers
      , "b" .= papersOutAuthors papers
      , "c" .= papersOutLinks papers
      , "e" .= papersOutAuthorsIndex papers
      , "d" .= papersOutPapers papers
      ]

main :: IO ()
main = do
  papers :: Vector (Loc PaperIn) <-
    fmap mconcat . traverse decodePapersYaml =<< getArgs

  -- Make a top-level paper for each hanging reference that consists only of
  -- a title.
  let newPapers :: Vector (Loc PaperIn)
      newPapers =
        Vector.fromList (foldMap step papers)
       where
        step :: Loc PaperIn -> [Loc PaperIn]
        step =
          locValue
            >> paperInReferences
            >> Vector.toList
            >> filter (flip Set.notMember titles)
            >> map
                 (\title ->
                   Loc
                    { locFile = 0
                    , locLine = 0
                    , locValue =
                        PaperIn
                          { paperInTitle = title
                          , paperInAuthors = mempty
                          , paperInYear = Nothing
                          , paperInReferences = mempty
                          , paperInLinks = mempty
                          }
                    })

        titles :: Set Title
        titles =
          foldMap (locValue >> paperInTitle >> Set.singleton) papers


  papers
    & (<> newPapers)
    & vectorSortOn (locValue >> paperInTitle >> Text.toLower)
    & transform
    & Json.encode
    & LByteString.putStr
 where
  decodePapersYaml :: FilePath -> IO (Vector (Loc PaperIn))
  decodePapersYaml file = do
    fileNo :: Int <-
      case stripPrefix "papers" file >>= readMaybe . take 3 of
        Nothing -> do
          hPutStrLn stderr ("Wots this? " ++ file)
          exitFailure
        Just n ->
          pure n

    bytes :: ByteString <-
      ByteString.readFile file

    -- The 'yaml' library makes it hard to get source locations, so we hack it
    -- together here. We know each yaml file is an array, so just find the
    -- locations of all of the '-' (45) characters that follow newlines (10).
    --
    -- Incidentally, this hacky algorithm is the reason why all papers.yaml
    -- files must begin with a newline, because otherwise we'd miss the first
    -- entry.
    let lineNo :: Int -> Int
        lineNo =
          ByteString.elemIndices 10 bytes
            & zip [2..]
            & mapMaybe
                (\(n, c) -> do
                  guard (c < ByteString.length bytes - 1)
                  guard (ByteString.index bytes (c+1) == 45)
                  pure n)
            & Vector.fromList
            & (Vector.!)

    case Yaml.parse (Yaml.array paperInParser) bytes of
      Left err -> do
        hPutStrLn stderr err
        exitFailure
      Right values ->
        pure
          (Vector.imap
            (\i x ->
              Loc
                { locFile = fileNo
                , locLine = lineNo i
                , locValue = x
                })
            values)

vectorSortOn :: forall a b. Ord b => (a -> b) -> Vector a -> Vector a
vectorSortOn f xs =
  runST go
 where
  go :: forall s. ST s (Vector a)
  go = do
    ys <- Vector.thaw xs
    Vector.sortBy (comparing f) ys
    Vector.freeze ys

data S = S
  { sTitleIds :: !(HashMap Title TitleId)
  , sAuthorIds :: !(HashMap Author AuthorId)
  , sLinkIds :: !(HashMap Link LinkId)
  , sAuthorsIndex :: !(AuthorIdMap TitleIdSet)
  , sTopLevelTitles :: !TitleIdSet
  , sCitations :: !(IntMap IntSet)
  , sNextTitleId :: !TitleId
  , sNextAuthorId :: !AuthorId
  , sNextLinkId :: !LinkId
  }

transform :: Vector (Loc PaperIn) -> PapersOut
transform papersIn =
  let
    (papersOut, s) =
      runState (mapM (transform1 (sCitations s)) papersIn) s0
  in
    ploop (papersOut, s)
 where
  s0 :: S
  s0 = S
    { sTitleIds = mempty
    , sAuthorIds = mempty
    , sLinkIds = mempty
    , sAuthorsIndex = mempty
    , sTopLevelTitles = mempty
    , sCitations = mempty
    , sNextTitleId = 0
    , sNextAuthorId = 0
    , sNextLinkId = 0
    }

  ploop :: (Vector PaperOut, S) -> PapersOut
  ploop (papers, s) =
    PapersOut
      { papersOutTitles =
          foldMap
            (swap >> uncurry IntMap.singleton)
            (HashMap.toList (sTitleIds s))
      , papersOutAuthors =
          foldMap
            (swap >> uncurry IntMap.singleton)
            (HashMap.toList (sAuthorIds s))
      , papersOutLinks =
          foldMap
            (swap >> uncurry IntMap.singleton)
            (HashMap.toList (sLinkIds s))
      , papersOutAuthorsIndex =
          sAuthorsIndex s
      , papersOutPapers =
          papers
      }

transform1 :: IntMap IntSet -> Loc PaperIn -> State S PaperOut
transform1 citations Loc{locFile, locLine, locValue = paper} = do
  title_id :: TitleId <-
    getTitleId (paperInTitle paper)

  title_ids :: IntSet <-
    gets sTopLevelTitles

  if IntSet.member title_id title_ids
    then error ("Duplicate entry: " ++ unpack (paperInTitle paper))
    else modify' (\s -> s { sTopLevelTitles = IntSet.insert title_id title_ids })

  authors :: Vector AuthorId <-
    mapM getAuthorId (paperInAuthors paper)

  modify'
    (\s ->
      s { sAuthorsIndex =
            appEndo (foldMap (Endo . flip insertIndex title_id) authors) (sAuthorsIndex s)
        })

  references :: Vector TitleId <-
    mapM getTitleId (paperInReferences paper)

  modify'
    (\s ->
      s { sCitations =
            appEndo (foldMap (Endo . flip insertIndex title_id) references) (sCitations s)
        })

  links :: Vector LinkId <-
    mapM getLinkId (paperInLinks paper)

  pure PaperOut
    { paperOutTitle = title_id
    , paperOutAuthors = authors
    , paperOutYear = paperInYear paper
    , paperOutReferences = references
    , paperOutCitations =
        fromMaybe mempty (citations IntMap.!? title_id)
    , paperOutLinks = links
    , paperOutFile = locFile
    , paperOutLine = locLine
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

-- | Insert into an inverted index.
insertIndex :: Int -> Int -> IntMap IntSet -> IntMap IntSet
insertIndex k v =
  IntMap.alter
    (\case
      Nothing ->
        Just (IntSet.singleton v)
      Just vs ->
        Just (IntSet.insert v vs))
    k

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)
