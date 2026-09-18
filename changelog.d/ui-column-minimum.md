## ui-column-minimum - a column is never narrower than its longest word

okay-watch's analyst page on an 80-column terminal broke its column
HEADERS mid-word — `alert/s`, `weigh/t`, `hel/d/by`. That page had met
the same defect in a browser and fixed it there with
`overflow-wrap: normal` on `th`; a terminal has no stylesheet to say it
with, so the LAYOUT says it.

`Frame.split` divided a budget by weight without ever measuring what
was in the columns. It measures now: each child is rendered unbudgeted
first, and a column gets at least its LONGEST WORD where the budget
allows — a word being the smallest thing wrapping must not split. The
room comes from columns with SLACK, widest first, because that is where
a character costs a reader least.

WHEN THE MINIMUMS DO NOT FIT, the shares stand and the words break: a
screen too narrow to hold them is one where breaking is the honest
answer, not an error. Tested at 8 columns as well as at 40.

A MINIMUM IS A FLOOR, NOT A WIDTH — a long sentence in a wide column
still wraps onto more lines, which the test pins beside the header
case.

This is the first of the three questions that all named the same
missing pass (terminal-header-finding) to be answered, and it is
answered the cheap way: `split` measures, rather than `render`
answering positions. The other two — a per-node viewport and a real
layout map — still want that pass and still want a consumer.
