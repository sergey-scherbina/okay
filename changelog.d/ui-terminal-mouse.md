## ui-terminal-mouse - a click is a key, and a widget is found by what it draws

SGR mouse reports (`ESC [ < button ; column ; row M`) decode in
`Frame.feed` beside the arrows, and a click is `Key.Click(row, col)` —
a key like any other, so the host still reads ONE door. Only the LEFT
button's PRESS is a click: a release would deliver every click twice,
and a wheel or a damaged report is dropped rather than guessed at.

A DAMAGED REPORT STAYS INSIDE ITS OWN STATE. The first cut bailed to
`Plain` on the first byte that was not a digit, which leaked the rest
of the report into the stream AS KEYSTROKES — a garbled click typing
for the user. A report now ends at `M` or `m` and nowhere else; the
test that caught it asserts the whole of a malformed one is consumed.

WHAT A CLICK MEANS is Enter's answer: the widget under it takes the
focus, and then `interpret` is asked what pressing it means. So a
button presses, a check toggles, and there is ONE table of meanings
rather than a second one for the mouse.

HIT-TESTING, and the honest shape of it. The renderer answers lines,
not positions, so `Frame.hit` searches the frame for the text each
focusable DRAWS, scanned in focus order (document order, left to
right, never backwards). The first attempt used the trick that found
the focused line — marked frame minus unmarked frame — and it failed
where it mattered: an `Input`'s focus mark is one character at its
END, so a click on the value itself missed. Two limits of what
replaced it are stated where it lives: a widget whose text WRAPS is
found by its first line only, and two widgets that render identically
are told apart by order alone. Both go away if `render` ever answers
positions beside its lines, which is a second return type and wants a
consumer first — BACKLOG says so.

The host turns reporting on INSIDE the raw-mode bracket and off again
on the way out, because a terminal left reporting clicks to a shell is
one somebody has to reset by hand. A click names a cell of the SCREEN,
so the row is offset by the view's top before the tree sees it.

TestTerminalKeys gains three cases.
