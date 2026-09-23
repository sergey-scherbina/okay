- [ ] doc-snippet-debt — PRIORITY: MEDIUM. The ratchet landed by
      doc-snippets-pin-all holds 1 410 example lines (2026-09-23) that no
      compiled source contains, in `docs/snippet-debt.txt`. The first
      pages paid down found FIVE broken examples among 33 lines (theory
      ch. 4 a week stale; three in continuations-in-practice that did
      not compile), so the debt is not cosmetic — it is where the pages
      are wrong. Largest: tutorial (~200), building-a-chat-app (~190),
      direct-style (118), declaring-an-api, di, durable-workflows,
      guide (36, fourteen blocks across core/stream/http/optics/ui).
      THE WORK, a page per lane: a `TestDocExamples<Page>` suite holding
      the page's lines verbatim and asserting them, or the page changed
      to the line its test already has; then `OKAY_SNIPPET_DEBT=write`
      to shrink the file. (2026-09-23)
