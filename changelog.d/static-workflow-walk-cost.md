## static-workflow-walk-cost - the cursor's gate had the wrong number behind it

Stage 5's third item, the O(1) cursor, is gated on "a term whose WALK
is measured to cost more than its next leaf's activity". The Decisions
section answered that by citing the appendix — "already priced O(1)
restore against a microsecond prefix and found the benefit inverted" —
and the appendix's number is about chapter 22's forty answers through
the MONADIC replay, a program being re-run. `Proc.walk` is a different
function over a different structure and had never been timed. A number
about a neighbouring shape, standing where this one's should be.

`MeasureProcWalk` takes it. **183 ns a record, linear** — 4 000
records take 9.3x the time of 400, which is what linear looks like. So
a walk is 35 µs at 40 records, 167 µs at 400, 1.1 ms at 4 000 and
6.3 ms at 40 000.

Against "its next leaf's activity" that is a crossover and not a
verdict, so the file prints the crossover: an activity of 100 µs is
worth 546 records of journal, one of 1 ms is worth 5 460, one of 10 ms
is worth 54 601. A workflow whose leaves are outside calls needs
thousands of records of UNBROKEN journal before a walk costs as much
as one of them — and `continueAs` collapses the history at no cost,
which is the cheaper answer to the same problem. The trigger is
therefore: sub-millisecond leaves AND more than ~5 000 records between
`continueAs` calls. Nobody here has that, so the cursor stays unbuilt
— with a number behind the decision instead of a citation of the wrong
measurement.

THE INSTRUMENT MEASURED ITSELF FIRST, and the control caught it
without being able to say whose fault it was. The first cut carried
the loop's answers in a `List` and did `got :+ answer` and
`got.length` per round — both O(k) at round k — and reported the walk
as QUADRATIC: 4.05 seconds at 40 000 records, 72.6x the time of a
tenth of the journal. The superlinearity was real and it belonged to
the term. The loop carries an `Int` now, and the file says so where
somebody writing the next measurement will read it.

The measurement prints what it walked beside how long it took, and
asserts the walk consumed the whole journal — a walk that stopped at
the first record would otherwise be the fastest of all.
