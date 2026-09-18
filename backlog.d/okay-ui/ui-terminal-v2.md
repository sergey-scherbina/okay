- [ ] ui-terminal-v2 — what is LEFT after four lanes on 2026-09-18
      (keys, width, scroll, caret): `Ui.Scroll` does not clip its own
      child, and there is no mouse.
      A PER-NODE VIEWPORT needs a layout pass that knows vertical
      space, which this renderer does not have — and no page has
      wanted one, since what a reader needs is the frame-level view
      that landed (the frame is clipped to the screen, PgUp/PgDn move
      it, and it follows the focus).
      The MOUSE needs SGR reporting (`ESC [ < b ; x ; y M`, which
      `Frame.feed` would decode — that half is an afternoon) AND
      hit-testing a rendered frame back to a widget, which nothing
      here does: a rendered line does not remember which node made it,
      so `render` would have to answer a map from (row, column) to key
      beside its lines. That is the real content of the lane and it is
      a second return type on the renderer, not a key table.
      TRIGGER unchanged: a product that runs on the terminal host for
      a PERSON.
