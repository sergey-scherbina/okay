## ui-scroll-viewport - a Scroll clips its own child, and ui-terminal-v2 closes

`Ui.Scroll` said "this box scrolls" and did nothing: the terminal host
clipped the whole FRAME, which is what a reader needs and not what the
node means. It clips its own child now, and the item that has run all
day closes with it.

THE RULE FOR VERTICAL SPACE, which was the decision this needed rather
than more mechanics: in a column with a height, every child that is not
a `Scroll` takes its NATURAL height and the `Scroll` children share
what is left, evenly, the remainder to the last; a nested `Scroll`
divides its own share the same way. It is what a browser and every
terminal application do, and it needs nothing new in the tree — which
is why it beat giving `Scroll` a weight, since a height in the tree
would be a pixel by another name.

`Frame.View` carries what the screen gives a frame: width, height,
caret, and where each keyed `Scroll` is scrolled to. Zero is unbounded
for both budgets, so every caller that knows nothing about screens
passes nothing and gets v1's layout — which is why 201 existing tests
did not move.

THE PLACEMENTS MOVE WITH THE VIEW, and the ones scrolled out of it are
dropped: a click can only land on what a reader can SEE, which is the
same rule the capability list states for the wire. That fell out of
having the layout map — it was a lookup, not new code.

AND THE HOST PAGES WHAT THE READER IS IN. `Frame.scrollAt` names the
innermost `Scroll` around the focus, so PgUp/PgDn move that region when
the focus is in a list and the whole frame when it is not. Without it
this would have been half a feature: a region that clips and cannot be
scrolled.

okay-ui 207, okay-script 207. `ui-terminal-v2` is closed — what is left
of the terminal is a resize nobody can hear (SIGWINCH) and Windows raw
mode, which has its own entry and needs a Windows box.
