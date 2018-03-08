import Data.List (isPrefixOf)
import Development.Shake

main :: IO ()
main =
  shakeArgs shakeOptions rules

rules :: Rules ()
rules = do
  want [".shake/shake", "main.min.js", "papers.json"]

  ".shake/main.js" %> \out -> do
    need ["elm-package.json", "Main.elm"]
    cmd_ ("elm make Main.elm --output=" ++ out)

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
    need ["stack.yaml", "haskell-papers.cabal", "yaml2json.hs"]
    cmd_ "stack install --local-bin-path .shake haskell-papers:exe:yaml2json"

  "main.min.js" %> \out -> do
    need [".shake/main.js", ".shake/uglifyjs"]
    cmd_ (".shake/uglifyjs .shake/main.js --compress --mangle toplevel=true --output " ++ out)

  "papers.json" %> \out -> do
    yamls <- filter (isPrefixOf "papers") <$> getDirectoryFiles "" ["*.yaml"]
    need (".shake/yaml2json" : yamls)
    cmd_ (FileStdout out) (".shake/yaml2json " ++ unwords yamls)

uglifyjsSHA :: [Char]
uglifyjsSHA =
  "fe51a91395f3b0a6ab812f3f42746d39efd9e80c"
