## ui-terminal-caret - a caret in the focused input, and v1 is it standing still

v1 could append and backspace, so fixing a typo in the middle of a
value meant deleting back to it. The caret is the HOST's state — a
position inside the focused `Input` — and the editing is a value:
`Frame.edit(value, caret, key)` answers the caret after the key and
the value when the key changed one.

**v1's behaviour is this function with the caret at the END**, which is
why there is one implementation and not two: a host without a caret is
a host whose caret never moved. `interpretAt` is the host's door
(focus, caret, event) and `interpret` stays exactly as it was, so every
caller and every existing test is untouched — asserted by a test that
runs both over the same keys.

WHICH KEYS THE CARET TAKES, stated as a SET rather than read off
whether the value changed: Left/Right/Home/End and any printable
character or delete, while an `Input` has the focus. The keys lane had
deliberately left Left/Right alone inside an `Input` for exactly this.
The set is explicit because `End` at the end of a value changes
nothing, and if that fell through to the tree it would jump the focus —
so End pressed twice would do two unrelated things, which is not a
keyboard anyone can learn.

THE CARET IS REVERSE VIDEO, which is what a terminal's own cursor is,
and it costs NO COLUMNS: `Frame.width` strips escapes, so a caret
cannot push a value out of its column. Past the end of the value it
sits on a space, where the next character goes.

AND ONE THING TYPING ITSELF REQUIRED: an edit rebuilds the tree on
every keystroke, so a caret that jumped to the end of the value each
time a tree arrived would make editing the middle impossible.
`Frame.clampCaret` keeps it where it is, clamps it to the value it is
now in, and puts it at the end only for a widget that has just taken
the focus.

The compiler found the same defect shape as the width lane: the caret
was dropped by every recursive `render` call (E221, "recursive call
used a default argument"), so a focused input deeper than the root
would not have shown it. Threaded now.

TestTerminalKeys gains six cases.
