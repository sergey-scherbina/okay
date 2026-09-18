- [ ] ui-terminal-v2 — what is LEFT after ui-terminal-keys read the
      escape sequences, ui-terminal-width gave the layout a budget and
      ui-terminal-scroll gave the HOST a view (all 2026-09-18): an
      `Input` edits by append and backspace with no CURSOR, `Ui.Scroll`
      does not clip its own child, and there is no mouse.
      WHAT EACH NEEDS NOW. A CARET is host state, and the decision in
      front of it is a key conflict rather than code: Left/Right are
      reserved for it already, but Home/End would have to mean
      line-start/line-end while an `Input` is focused instead of the
      ends of the tab order. A PER-NODE VIEWPORT (`Scroll` clipping its
      own child) needs a layout pass that knows vertical space, which
      this renderer does not have — and no page has wanted one, since
      what a reader actually needs is the frame-level view that landed.
      The MOUSE needs SGR reporting (`ESC [ < b ; x ; y M`, which
      `Frame.feed` would decode) AND hit-testing a rendered frame back
      to a widget, which nothing here does: a rendered line does not
      remember which node made it. TRIGGER unchanged: a product that
      runs on the terminal host for a PERSON.
