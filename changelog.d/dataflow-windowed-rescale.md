## dataflow-windowed-rescale - a windowed job changes width, by replaying its panes rather than journalling them

Stage 13 box 2 was a refusal with a reason: a windowed sink keeps its
open panes in the WORKER, so a re-cut — which skips the prefix instead
of replaying it — would lose the panes open at the stop point, and the
box said they must be journalled first. Box 2b's horizon
(`dataflow-horizon-seek`) gives a second answer: replay them. The two
were already priced against each other in `MeasureWindowedSeek` — 16 KB
once per resume against 3.3 KB every epoch for ever — and this is the
cheaper one built.

THE OBVIOUS VERSION IS WRONG IN A WAY NOTHING WOULD SHOW, and it was
written before it was refuted. Road B's same-width seek works because
the replay reads the same elements in the same epochs, so its
watermark follows the same trajectory and the catch-up discards
exactly what the coordinator already folded. A RE-CUT BREAKS THAT: new
partition j reads every parts-th element, so its maximum is a sample
of the global one and it closes a slightly different SET of panes.
Each misclassification is silent — a pane the original closed and the
replay keeps open is merged into a coordinator that already has it
(double count); one the original kept open and the replay closes is
discarded with its early elements (under count). Replaying "to the
stop POSITION and discarding" only moves the boundary; the worker
still has to decide which panes the coordinator holds, and after a
re-cut its watermark is not the one those decisions were made under.

SO THE WORKER DISCARDS NOTHING. The sessions open at the horizon
mark's re-striped prefix, at the stop epoch, and the ordinary epoch
loop hands the replayed elements over like any other work. Two rules
at the COORDINATOR, where retirement is the local decision and the
number it used is known:

- `Sink.reopen(s)` drops its OPEN panes and keeps the folded answer.
  It can, because the horizon rule puts every still-open pane above
  the mark's maximum, so the replay rebuilds each of them in full; a
  partial copy left behind would be added to a full one.
- `Sink.sift(w, below)` drops contributions to panes already RETIRED,
  with `below` the watermark retirement used. Everything above it is a
  pane the coordinator no longer has, so merging is right whatever the
  replaying worker's watermark was doing.

Neither needs the two watermarks to agree, which is the property a
re-cut cannot have.

BOTH RULES HAVE A CONTROL THAT FIRES, which is the only reason either
is in the tree. `TestRescale` rescales a windowed job 4->6, 4->2, 6->3
and 2->8 to the batch answer; removing `sift` makes that red at once.
Removing `reopen` did NOT — and the reason is worth keeping: with an
in-order feed the partitions' clocks agree so closely that no pane
boundary falls between "one partition closed it" and "the slowest
partition passed it", so the coordinator never holds a partial copy
and there is nothing to drop. A probe said so: `reopen` was called
four times and dropped zero panes. Swept over jitter, take and stop
epoch, the case appears at the feed's declared lateness limit — sixteen
handed-but-unretired panes — and that test is now in the suite and
fails without `reopen`.

A run too young to have a mark a horizon back still refuses, and the
message names the horizon and where the run stands rather than
repeating the old "open panes are not in the journal".

Nine doors forwarded the two new answers (five `Wire` wrappers, `and`,
the staging sink), and `sift`'s default is the identity with one
comparison a pane for every run that is not a re-cut. 109 green in
okay-cluster.
