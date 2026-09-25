## foreign-facade-2 - Frames[M]: a Table there and back on every road, by reference on the JVM; the threshold withdrawn

Stage 2 of specs/foreign-facade.md. `Frames[-M]` takes an
`okay.arrow.Table` and answers one, over the workers' own `frame` op for
`PyModule` and `RModule` (Arrow where the worker negotiated it, columnar
JSON otherwise; a column the frame cannot say is refused by name before
the wire) and by REFERENCE for `JvmModule` (`JvmModule.frame(name)(f)`;
`TestFacade` holds `eq`). `Road.rows[M, A, B]` is the door for rows: one
frame each way. The conformance body `frames` runs over the JVM in the
default gate and over python3 here (Live; R skips, not installed).

A finding replaced a plan: the spec's `Frame.Threshold` — rows past it
would cross as a frame, below it as calls — cannot exist, because a
far-side function written for a frame and one written for a record are
different functions, and no count turns one into the other. The SHAPE
picks the tier (Decision 6); what a frame costs per size is stage 5's
measurement, not a switch.

The empty table found a seam: the JSON frame road cannot type a column
with no cells (`Nulls(0)`), which `Rows.rows[A]` refused; a table of no
rows now reads as no rows whatever its columns say (okay-arrow) — the
first place the Arrow road and the JSON road differed in what they
carry, recorded in the spec's Results.
