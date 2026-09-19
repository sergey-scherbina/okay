## dataflow-durable-stage - the two-phase writer was losing panes, silently, and the measurement came first

Stage 9's last box asked for "a writer whose stage is DURABLE, so the
two-phase commit survives the writer's own death as well as the
coordinator's" and said nothing here had asked yet. Somebody asked
what it meant, and the answer turned out to be a defect rather than a
feature.

`recover()` learns what already landed from the OUTPUT, by the highest
epoch in it. That rule is sound only if an epoch is in the output
ENTIRELY or not at all — and the writer appended one record per PANE,
under a comment claiming "the log's append is the atomicity a
two-phase writer needs". True of each record; false of the batch.

MEASURED BEFORE IT WAS FIXED. A probe killed the writer three panes
into epoch 4. The successor, a fresh process with no memory, read the
output, saw epoch 4 in it, dropped the re-move as a duplicate — and
**93 panes of 3 204 were never written, while the run reported all
3 204**. A silent short write: the answer is right, the output is
short, and nothing says so. That is precisely the failure stage 9
exists to prevent, sitting inside the example that demonstrates it.

ONE APPEND PER EPOCH removes the state instead of detecting it. There
is no third place for the writer to die — before its append or after
it — and both are now tested: before, the successor writes the epoch
in full; after, it reads the epoch as complete and drops the re-move,
which is correct because it IS complete.

THE GUARD IS STRUCTURAL, because a guard that can only be checked by
killing something at the right microsecond is not a guard: `records ==
distinct epochs`. Reverted to per-pane appends it fails immediately,
at "3 204 records for 11 epochs".

WHAT IT COSTS, stated rather than discovered: an epoch must fit in one
record. The largest here is about 64 KB, well under Kafka's 1 MB
default. A job whose epoch does not fit needs chunking with a
per-epoch completion marker and a reader that ignores an unmarked
tail, or a transactional writer — the Kafka interop has transactions
and `TestKafkaEos` exercises them. Neither is built, because nothing
here has an epoch that big.

A SIDE EFFECT WORTH THE LINE: the Live Kafka battery got two to three
times faster (the quiet run 3.9 s to 1.9 s, the four-death
exactly-once case 16.1 s to 6.5 s), because an epoch is one append
instead of about 350.
