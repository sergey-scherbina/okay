- [ ] ui-terminal-v2 — what is LEFT after five lanes on 2026-09-18
      (keys, width, scroll, caret, mouse): `Ui.Scroll` does not clip
      its own child, and hit-testing finds a widget by the TEXT IT
      RENDERS rather than by a layout map.
      A PER-NODE VIEWPORT needs a layout pass that knows vertical
      space, which this renderer does not have — and no page has
      wanted one, since what a reader needs is the frame-level view
      that landed.
      A LAYOUT MAP is the same missing pass seen from the other side:
      `render` answers lines, not positions, so `Frame.hit` searches
      the frame for what each focusable draws, in focus order. Two
      limits are stated where it lives: a widget whose text WRAPS is
      found only by its first line, and two widgets that render
      identically are told apart by order alone. Both go away if
      `render` ever answers positions beside its lines — which is a
      second return type on the renderer and wants a consumer first.
      AND A PRODUCT HAS NOW LOOKED (2026-09-18): okay-watch's analyst
      page draws on the terminal host and FITS at 80, 120 and 200
      columns — no line runs off, and the identifiers and timestamps
      wrap whole. One defect it found, and it is a LAYOUT one, which
      is why it belongs in this item: a column narrower than its
      header word breaks the WORD — `alert/s`, `weigh/t`, `hel/d/by`.
      That page had already met this in the browser and fixed it with
      `overflow-wrap: normal` on `th`; a terminal has no stylesheet to
      say it with. The fix is a MINIMUM COLUMN WIDTH — each column at
      least its longest header word, where the budget allows — which
      `Frame.split` cannot compute today because it divides a budget
      without ever measuring what is in the columns. The same missing
      pass as the two above, seen a third time. okay-watch pins the
      current behaviour as a PENDING test, so the day the minimum
      lands, that test fails and becomes the real one.
      TRIGGER for the rest: a product that runs on the terminal host
      for a PERSON.
