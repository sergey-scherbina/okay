## facade-frame-seam - rows through the facade take Python's own road; a Table goes to the wire as itself

The tier-2 cell that read 37% worse than Python's own road
(foreign-facade-5) was a comparison against a PRE-BUILT frame with no
rows on either side, which no caller has. Measured against the honest
own road (rows to a PyFrame, over, rows back — PyStage's), rows through
the facade are within 3%: `Frames` now has `rows` beside `frame`, by
default through `frame` and the Rows codec, and Python overrides it with
that road, which `Road.rows` takes. What was a seam is the Table road:
`Frames.frame` built a PyFrame from the Table and the worker built a
Table from it again where Arrow is spoken — two conversions of the same
columns. `ForeignWorker.frameTable` sends a Table as itself on the Arrow
road and converts once on the JSON road (152 ms against the bare frame's
140 here, at load 42.7). R's twin and the Arrow cells stay in the
backlog item.
