https://mitchellwrosen.github.io/haskell-papers

---

**What is it?**

A collection of hyperlinks to functional programming papers.

**How does it work?**

- [`papers.yaml`](papers.yaml) is the source of all papers and metadata and is
edited manually by humans.
- [`yaml2json.hs`](yaml2json.hs) "compiles" `papers.yaml` to
[`papers.json`](papers.json), which does little more than de-dupe strings and
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

Build the site: `./build.sh`

Find dead links: `stack exec getlinks`
