## writer-listen-censor - Writer.listen and Writer.censor, the scoped Writer operations

- `Writer.listen(p)` answers p's value together with what p told. It
  re-tells each value in place, so the outer handler sees the same
  tells in the same order, and a raise inside p leaves the earlier tells
  told.
- `Writer.censor(p)(f)` rewrites p's whole output. p's tells are held
  back and told as `f(all)` at p's end, so a raise inside p drops them.
  TestWriterScoped pins that as the definition. A rewrite of each value
  in place was already there: `Writer.map` / `Writer.expand`.
- These are mtl's `MonadWriter` pair, the duals of `Reader.local` and
  `Throws.recover`. Spec specs/core-gaps.md stage 2; docs/guide.md
  scoped-effects section; off the sprint (writer-listen-censor).
