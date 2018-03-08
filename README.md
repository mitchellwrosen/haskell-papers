https://mitchellwrosen.github.io/haskell-papers

---

**What is it?**

A collection of hyperlinks to functional programming papers.

**How does it work?**

- [`papers.yaml`](papers.yaml) is the source of all papers and metadata and is
edited manually by humans.
- (Because GitHub's file-editing UI becomes slow on large files, `papers.yaml`
is actually snipped into smaller `papersXX.yaml` files every 500 lines or so.)
- [`yaml2json.hs`](yaml2json.hs) "compiles" `papers.yaml`, `papers01.yaml`, etc.
to [`papers.json`](papers.json), which does little more than de-dupe strings and
create dummy papers out of hanging references.
- [`Main.elm`](Main.elm) contains the UI code, which is compiled and minified to
[`main.min.js`](main.min.js).
- GitHub hosts this `master` branch as a
[static site](https://mitchellwrosen.github.io/haskell-papers), which is
comprised of [`index.html`](index.html) and `main.min.js`.

**How can I help?**

Lots of ways! If you have a paper to add, please modify `papers.yaml` and make a
pull request. A bunch of paper collections are listed in [`todo.txt`](todo.txt).
Each paper in `papers.yaml` has metadata (like `references`), which is filled
out lazily as I (or anyone) notices it's missing. The UI can always be improved
upon. New metadata fields, more automation, better architecture, better
documentation, more streamlined path to contribution, anything!

**Misc. notes**

Build everything:

    ./build.sh

Clean up everything:

    rm papers.json
    rm -rf .shake

Find dead links (takes a while to avoid IP bans):

    stack exec getlinks

Necessary dummy symlink for local noodling:

    mkdir haskell-papers
    ln -s ../main.min.js haskell-papers/main.min.js
