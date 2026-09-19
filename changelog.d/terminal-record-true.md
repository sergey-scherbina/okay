## terminal-record-true - the terminal item said a fixed defect was open

`ui-terminal-v2` still described okay-watch's header defect — a column
narrower than its header word breaking the word — as the fix that was
owed. It landed the same day (`ui-column-minimum`, 0bda68ea), and the
product's PENDING test became the real one an hour later. A backlog
entry that names finished work sends the next agent looking for it,
which is the failure `backlog-cleanup` was written about.

Rewritten to say what is actually left, which is ONE capability rather
than a list: **`Frame.render` answers lines, not positions.** Two
things wait on it — a `Ui.Scroll` that clips its own child, and
hit-testing that reads a map instead of searching for the text each
widget draws — and both have their limits written where they live.

AND THE USEFUL PART OF THE CORRECTION: the column minimum was the
THIRD question that had been pointing at this same missing pass, and
it turned out not to need it — `Frame.split` measures the columns
itself. So the entry now says why the remaining two are worth waiting
on rather than bundling: the cheap half was already cheap, and what is
left is a second return type on the function every terminal test
calls.
