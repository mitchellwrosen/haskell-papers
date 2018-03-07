#!/usr/bin/env stack
-- stack script --resolver lts-10.6

import Development.Shake

main :: IO ()
main =
  shakeArgs shakeOptions rules

rules :: Rules ()
rules = do
  want ["main.min.js", "papers.json"]

  ".shake/main.js" %> \out -> do
    need ["elm-package.json", "Main.elm"]
    cmd_ ("elm make Main.elm --output=" ++ out)

  ".shake/uglifyjs" %> \_ -> do
    let tarball = uglifyjsSHA ++ ".tar.gz"
    cmd_ (Cwd ".shake") ("wget https://github.com/mishoo/UglifyJS2/archive/" ++ tarball)
    cmd_ (Cwd ".shake") ("tar xf " ++ tarball)
    cmd_ (Cwd (".shake/UglifyJS2-" ++ uglifyjsSHA)) "npm install"
    cmd_ ("ln -s UglifyJS2-" ++ uglifyjsSHA ++ "/bin/uglifyjs .shake/uglifyjs")
    cmd_ ("rm .shake/" ++ tarball)

  ".shake/yaml2json" %> \_ -> do
    need ["stack.yaml", "yaml2json.cabal", "yaml2json.hs"]
    cmd_ "stack install --local-bin-path .shake"

  "main.min.js" %> \out -> do
    need [".shake/main.js", ".shake/uglifyjs"]
    cmd_ (".shake/uglifyjs .shake/main.js --compress --mangle toplevel=true --output " ++ out)

  "papers.json" %> \out -> do
    need [".shake/yaml2json", "papers.yaml"]
    cmd_ (FileStdin "papers.yaml") (FileStdout out) ".shake/yaml2json"

uglifyjsSHA :: [Char]
uglifyjsSHA =
  "fe51a91395f3b0a6ab812f3f42746d39efd9e80c"
