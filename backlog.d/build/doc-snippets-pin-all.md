- [ ] doc-snippets-pin-all — PRIORITY: MEDIUM. The rule is "every
      snippet verbatim in a gated test" (docs-are-part-of-the-lane),
      and `TestDocSnippets` enforces it for FOUR documents
      (jvm-languages, okay-java, okay-clojure, okay-frege). Today's
      lanes put snippets into docs/guide.md, docs/continuations-in-
      practice.md, docs/modules/okay-sql.md, docs/direct-style.md and
      theory ch. 4, each mirrored in a test by hand and by nobody's
      check. The record outlives the truth exactly there. THE LANE:
      extend the pinned map to every doc with fenced Scala — guide,
      tutorial, direct-style, continuations-in-practice, the module
      pages, theory — with `<!-- not-a-test -->` on the blocks that are
      prose-shaped code; expect a first run to name dozens of lines,
      fix them or exempt them one by one with the reason. (2026-09-23)
