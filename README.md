https://mitchellwrosen.github.io/haskell-papers

---

**What is it?**

A collection of hyperlinks to functional programming papers.

**How does it work?**

- [`papers.yaml`](papers000.yaml) files are the source of all papers and
metadata and are edited manually by humans. These files are snipped to 300-500
lines to keep GitHub's file-editing UI responsive.
- [`yaml2json.hs`](yaml2json.hs) "compiles" all of the `papers.yaml` files to
[`papers.json`](papers.json), which de-dupes strings, creates dummy papers
out of hanging references, and adds file location information to each paper.
- [`Main.elm`](Main.elm) contains the UI code, which is compiled and minified
with [`UglifyJS`](https://github.com/mishoo/UglifyJS2) to
[`main.min.js`](main.min.js).
- GitHub hosts this `master` branch as a
[static site](https://mitchellwrosen.github.io/haskell-papers), which is
comprised of [`index.html`](index.html), [`the.css`](the.css), and
`main.min.js`.

**How can I help?**

Lots of ways!

- Add a new paper to any `papers.yaml` file ([`todo.txt`](todo.txt) contains a
  bunch of links to scour through)
- Amend metadata (e.g. year of publication) of an existing paper
- Improve the UI

**Paper schema reference**

    // The paper title
    title : Required String

    // The paper author(s)
    author : Optional String
    authors : Optional List of String

    // Year of publication
    year : Optional Integer

    // Titles of references
    references : Optional List of String

    // Hyperlink(s) in order of preference
    link : Optional String
    links : Optional List of String

**Misc. dev notes**

Build everything:

    ./build.sh

Clean up everything:

    rm papers.json
    rm -rf .shake

Find dead links (takes a while to avoid IP bans):

    stack build haskell-papers:exe:getlinks --flag haskell-papers:build-getlinks
    stack exec getlinks

Quickly iterate on `yaml2json.hs`:

    ghcid -c 'stack ghci --ghci-options=-fno-code --main-is haskell-papers:exe:yaml2json

Here are some characters that are difficult for me to type:

    ÁáâĆćÉéèëêễǴǵÍíḰḱĹĺḾḿŃńÓóÖöøṔṕŔŕŚśÚúẂẃX́x́ÝýŹź
