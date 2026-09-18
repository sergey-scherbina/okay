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
      TRIGGER unchanged: a product that runs on the terminal host for
      a PERSON.
