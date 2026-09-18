## dataflow-horizon-seek - a windowed sink seeks after all, bounded by its horizon

Stage 11 box 2 gave a SEEKABLE sink's sessions a position to open at,
and said in the same breath that a windowed one cannot use it: its open
panes live inside the partition and a fresh session at a position has
none of them. So every windowed resume, and every windowed replacement
worker, replayed its partition from zero. `dataflow-windowed-seek`
measured the two roads out of that and chose road B — the
horizon-bounded replay, 16 KB once per resume against 3.3 KB every
epoch for ever. This is road B.

`Sink.horizon` is the new number and the whole idea: HOW FAR BACK OF
ITS OWN MAXIMUM this sink's state reaches. A window answers `size +
lateness`, because its operator closes a pane when `start + size <=
max - lateness`, so no pane still open starts below `max - (size +
lateness)` and no element below that point belongs to one. A fold or a
keyed sink answers 0 and goes on answering `seekable` instead; `and`
takes the furthest of the two.

`Folded.marks` carries what a seek needs: per epoch, where every
partition stood and how high each partition's event time had reached.
`opening` then picks, PER PARTITION, the newest mark whose own maximum
is a horizon below where that partition stands now. Per partition and
not globally, because the operator holding the panes is per partition —
a global watermark is the slowest partition's clock and would refuse
every seek on a feed whose partitions cover different times, which is
exactly the feed the test uses.

THE SEED THE SPEC ASKED FOR TURNED OUT NOT TO BE NEEDED, and the reason
is the mechanism that was already there. A session opened behind the
requested epoch catches up by replaying and DISCARDING (`Job.advance`),
and every pane that could have lost a skipped element is closed before
the requested epoch — `start <= mark.max <= now - horizon` is exactly
the closing condition — so the wrong copies are all thrown away, and
the epoch the coordinator actually folds is computed under the same
maximum, hence the same late-drop decisions.

COUNT THE DOORS: the first cut put `horizon` on `Sink` and forgot the
five `Wire` wrappers, which forward `seekable` one by one. `Wire.tumbling`
answered 0, no seek fired, and the old test passed for the wrong reason.
A test now asserts that every wrapper forwards both answers.

`TestSeek` asserts the answer first and the reads second: the resumed
windowed job answers identically (value and drops) and reads 15 904 of
20 000 records instead of all of them, exactly `total - Σmark.positions`;
a mid-run replacement re-reads 512 elements — one epoch, the newest mark
that clears a 1300-wide horizon on a feed whose epochs are 5120 apart —
where the replay from zero re-read the 2 560 that partition had already
consumed. Two new tests, 106 green in okay-cluster.
