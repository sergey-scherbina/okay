- [ ] ui-terminal-v2 — what is LEFT after ui-terminal-keys read the
      escape sequences and ui-terminal-width gave the layout a budget
      (both 2026-09-18): an `Input` edits by append and backspace with
      no CURSOR, `Scroll` renders its child whole and clips nothing,
      and there is no mouse.
      WHAT EACH NEEDS NOW, since the size question is answered:
      a CARET is host state (Left/Right are reserved for it already,
      and Home/End would have to mean line-start/line-end while
      focused on an `Input` rather than the ends of the tab order — a
      conflict to decide, not just code); SCROLL needs the HEIGHT
      budget and an offset per keyed Scroll, which is the same shape
      the width took and is the cheapest of the three now; the MOUSE
      needs SGR reporting (`ESC [ < b ; x ; y M`, which `Frame.feed`
      would decode) AND hit-testing a rendered frame back to a widget,
      which no part of this host does — a rendered line does not
      remember which node made it. TRIGGER unchanged: a product that
      runs on the terminal host for a PERSON.
