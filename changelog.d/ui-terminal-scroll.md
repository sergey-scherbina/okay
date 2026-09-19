## ui-terminal-scroll - a frame taller than the screen, and a view that follows the focus

The terminal painted every line of every frame, so a page taller than
the window simply lost its top — and Tab walking down a long form moved
a focus the reader could no longer see. The view is the HOST's business
(the tree says nothing about where a reader is looking), so the pure
half is three small functions a host composes, and all three are
values:

- `Frame.focusLine` — which line the focused widget is on, found by the
  ONE difference between the marked frame and the unmarked one. Marking
  is the only thing `focus` changes, so the first differing line is the
  answer. That reuses the renderer instead of threading a line counter
  through every case of it, and it cannot drift from what is drawn.
- `Frame.clip` — a screen of the frame, pulled back when the top is
  past the end, and PADDED when the frame is shorter than the screen so
  the previous paint does not show through underneath.
- `Frame.follow` — the top that keeps a line visible, moving as little
  as it can and only when it must.

`Terminal.host` composes them: it clips to the height `stty size` gave
it, follows the focus on every repaint, and reads `PageUp`/`PageDown`
(`ESC [ 5 ~` / `ESC [ 6 ~`, decoded by `Frame.feed`) ITSELF — those two
keys move no focus and say nothing to the application, which is what
makes them the host's and not the tree's.

WHAT IS STILL NOT DRAWN, and now for a sharper reason: `Ui.Scroll` does
not clip its own child. A per-node viewport needs a layout pass that
knows vertical space, which this renderer does not have — and a
frame-level view is what a reader actually asked for. BACKLOG says so,
beside the caret and the mouse.

TestTerminalKeys gains four cases.
