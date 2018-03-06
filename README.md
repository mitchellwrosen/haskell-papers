https://mitchellwrosen.github.io/haskell-papers

---

**What is it?**

A collection of hyperlinks to functional programming papers.

**How does it work?**

[`papers.yaml`](papers.yaml) is the source of all papers and metadata and edited
manually by humans. [`yaml2json.hs`](yaml2json.hs) generates
[`papers.json`](papers.json) from `papers.yaml`, which is served by the
GitHub-hosted static site comprised of of [`index.html`](index.html) and
[`main.min.js`](main.min.js) (generated from [`Main.elm`](Main.elm)).

**How can I help?**

Lots of ways! If you have a paper to add, please modify `papers.yaml` and make a
pull request. A bunch of paper collections are listed in [`TODO.md`](TODO.md).
Each paper in `papers.yaml` has metadata (like `references`), which is filled
out lazily as I (or anyone) notices it's missing. The UI can always be improved
upon. New metadata fields, more automation, dead link detection, better
architecture, better documentation, more streamlined path to contribution,
anything!
