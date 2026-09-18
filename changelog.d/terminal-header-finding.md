## terminal-header-finding - the first product page on the terminal, and what it showed

The five terminal lanes were tested as values, which is what made them
cheap. A page is not a value test, so okay-watch's analyst page was
pointed at the host and LOOKED AT — the thing this repository's own
`ui-product` arc exists to insist on.

IT FITS. At 80, 120 and 200 columns no line runs off the screen, and
the two values a reader must be able to check — an identifier and a
timestamp — arrive whole, wrapped across lines rather than cut. That is
the width budget doing exactly what it was built for, on nine- and
ten-column tables that were designed for a browser.

AND ONE DEFECT, which is a LAYOUT defect and therefore lands in
`ui-terminal-v2` beside the other two: a column narrower than its
header word breaks the WORD — `alert/s`, `weigh/t`, `hel/d/by`. That
page had already met this in the browser and fixed it there with
`overflow-wrap: normal` on `th`; a terminal has no stylesheet to say it
with. The fix is a minimum column width — each column at least its
longest header word, where the budget allows — and `Frame.split`
cannot compute one today because it divides a budget without ever
measuring what is in the columns. **That is the same missing pass as
the per-node viewport and the layout map, seen a third time**, which is
the useful part of this finding: three different questions with one
answer behind them.

okay-watch carries the evidence (their 1a1a1a6): the fit tests, a click
found on a real screen, an `OKAY_SHOW=<cols>` viewer for a person, and
the header defect pinned as PENDING so it asserts what happens today
and fails the day the minimum lands.
