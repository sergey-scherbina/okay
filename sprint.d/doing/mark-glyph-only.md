- [ ] mark-glyph-only — retire the `.!?` mark (operator, 2026-09-25:
      «уберем .!? и … будем использовать именно только .?»). Since
      unwrap-glyph the direct mark has three spellings (`.reflect`,
      `.!?`, `.?`) and `.!?` stayed only because it was already
      written everywhere. Remove `def !?` from Direct (generic and
      Gen), replace it by `?` in Cont.Monadic, drop it from the
      macros' mark sets and messages, rewrite every call site
      (tests, benchmarks, docs, scaladoc) to `.?`, and pin that
      `m.!?` no longer compiles. Archives (CHANGELOG.md,
      BACKLOG-ARCHIVE.md, history.tsv, changelog.d) are records and
      stay. Spec: specs/unwrap-glyph.md, stage 5.
