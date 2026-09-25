- [ ] doc-snippet-debt — PRIORITY: MEDIUM. The ratchet landed by
      doc-snippets-pin-all holds 1 231 example lines (2026-09-25; 1 359 on 09-24) that no
      compiled source contains, in `docs/snippet-debt.txt`. The pages
      paid down so far found real broken examples every time, not just
      cosmetic drift: theory ch. 4 a week stale; three in
      continuations-in-practice that did not compile; direct-style had
      Monadic's object rewritten with a retired `.?` mark, Once's
      signatures paraphrased, Gen's class definition a stage behind its
      real `chain`-based one, and a whole Fetch/Test walkthrough that
      still said `Fetch.now`/`object Test` after the real fixture
      (TestDirectOnce) renamed to `Fetch.time`/`Runner`+`test`. Largest
      remaining: tutorial (~164), building-a-chat-app (~90),
      declaring-an-api (~60), durable-workflows (~43), di (~36).
      DONE: guide (doc-snippet-debt-guide — two more broken examples:
      a `Tag` clause that did not compile, a missing `Plate[Json]`
      import), continuations-in-practice, theory ch. 4, direct-style
      (118 -> 0, doc-snippet-debt-direct-style 2026-09-25).
      THE WORK, a page per lane: a `TestDocExamples<Page>` suite holding
      the page's lines verbatim and asserting them, or the page changed
      to the line its test already has; then `OKAY_SNIPPET_DEBT=write`
      to shrink the file. A pinned line whose block returns a
      non-Unit/program value and stands bare (the page's own REPL-echo
      style) needs `@scala.annotation.nowarn("msg=unused value|discarded
      non-Unit value")` on a small private helper holding just that
      line, rather than reshaping the pinned text — direct-style hit
      this a dozen times. (2026-09-23)
