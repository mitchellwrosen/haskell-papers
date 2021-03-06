{-# language LambdaCase #-}

import Data.List (isPrefixOf)
import Development.Shake

main :: IO ()
main =
  shakeArgs
    shakeOptions
      { shakeChange = ChangeModtimeAndDigestInput
      , shakeThreads = 2
      }
    rules

rules :: Rules ()
rules = do
  want
    [ ".shake/shake"
    , "static/main.min.js"
    , "static/nouislider-shim.min.js"
    , "static/papers.json"
    ]

  ".shake/main.js" %> \out -> do
    src <- getDirectoryFiles "" ["ui/*.elm"]
    need ("elm-package.json" : src)
    -- In dev mode, pass --debug
    getEnv "DEV" >>= \case
      Nothing ->
        cmd_ ("elm make ui/Main.elm --output=" ++ out)
      Just _ ->
        cmd_ ("elm make --debug ui/Main.elm --output=" ++ out)

  ".shake/shake" %> \_ -> do
    need ["build.sh", "shake.hs"]
    cmd_ "stack --resolver lts-10.7 ghc --package shake shake.hs -- -o .shake/shake -odir .shake -hidir .shake"

  ".shake/uglifyjs" %> \_ -> do
    let tarball = uglifyjsSHA ++ ".tar.gz"
    cmd_ (Cwd ".shake") ("curl -sL https://github.com/mishoo/UglifyJS2/archive/" ++ tarball ++ " -o uglifyjs.tar.gz")
    cmd_ (Cwd ".shake") "tar xf uglifyjs.tar.gz"
    cmd_ (Cwd (".shake/UglifyJS2-" ++ uglifyjsSHA)) "npm install"
    cmd_ ("ln -f -s UglifyJS2-" ++ uglifyjsSHA ++ "/bin/uglifyjs .shake/uglifyjs")
    cmd_ "rm .shake/uglifyjs.tar.gz"

  ".shake/yaml2json" %> \_ -> do
    need ["stack.yaml", "scripts/haskell-papers.cabal", "scripts/yaml2json.hs"]
    cmd_ "stack install --local-bin-path .shake haskell-papers:exe:yaml2json"

  "static/main.min.js" %> \out -> do
    let src = ".shake/main.js"
    getEnv "DEV" >>= \case
      Nothing -> do
        need [".shake/uglifyjs", src]
        cmd_ (uglify src out)
      Just _ ->
        copyFile' src out

  "static/nouislider-shim.min.js" %> \out -> do
    let src = "ui/nouislider-shim.js"
    getEnv "DEV" >>= \case
      Nothing -> do
        need [".shake/uglifyjs", src]
        cmd_ (uglify src out)
      Just _ ->
        copyFile' src out

  "static/papers.json" %> \out -> do
    yamls <- filter (isPrefixOf "papers") <$> getDirectoryFiles "" ["*.yaml"]
    need (".shake/yaml2json" : yamls)
    cmd_ (FileStdout out) (".shake/yaml2json " ++ unwords yamls)

-- 'uglify src dst' renders a shell command that uglifies 'src' to 'dst'.
uglify :: FilePath -> FilePath -> [Char]
uglify src dst =
  ".shake/uglifyjs " ++ src ++ " --compress --mangle --output " ++ dst

uglifyjsSHA :: [Char]
uglifyjsSHA =
  "fe51a91395f3b0a6ab812f3f42746d39efd9e80c"
