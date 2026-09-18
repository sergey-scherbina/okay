- [ ] ui-terminal-v2 — ONE capability is left, and it has two
      consumers-in-waiting. Six lanes landed on 2026-09-18 (keys,
      width, scroll, caret, mouse, column-minimum) and what they left
      is this: **`Frame.render` answers LINES, not positions.** It does
      not say where on the screen it drew each node.
      WHAT THAT COSTS, both halves of it:
      - `Ui.Scroll` does not clip its own child. The whole FRAME
        scrolls — clipped to the screen, PgUp/PgDn, following the
        focus — which is what a reader actually needs; a node that
        says "I scroll" cannot yet do it alone, because a viewport
        needs to know its share of the VERTICAL space and nothing in
        this renderer computes one.
      - DONE 2026-09-18 (ui-terminal-layout-map): `Frame.laid` answers
        the lines AND where every focusable sits, `render` is its first
        half — one walk, not two — and `hit`/`focusLine` are lookups.
        Both old limits are gone.
      WHAT IS NOT LEFT, so nobody re-opens it: the header defect
      okay-watch found by LOOKING (a column narrower than its header
      word broke the word) is FIXED — `ui-column-minimum`, 0bda68ea:
      `Frame.split` measures the columns before dividing the budget,
      and a column gets at least its longest word where the budget
      allows. That was the third question this missing pass had been
      answering, and it turned out not to need the pass at all, which
      is why the remaining two are worth waiting on rather than
      bundling: the cheap half was cheap.
      THE CAPABILITY LANDED and cost what it was said to cost: a
      second return type, paid once, with every terminal test calling
      `render` unchanged. What is left of this item is the VIEWPORT
      alone, and it needs a decision rather than mechanics — who gets
      the leftover VERTICAL space in a column (the rule on offer:
      non-Scroll children take their natural height, Scroll children
      share the remainder, a nested Scroll divides its parent's share)
      plus a scroll offset per key in the host, as the caret is.
      TRIGGER: a page that needs a scrolling REGION rather than a
      scrolling screen. That has not happened — okay-watch's analyst page fits at 80, 120 and 200
      columns, its values wrap whole, and a click on its refresh
      button finds the refresh button (their 1a1a1a6, 6d2a72f).
