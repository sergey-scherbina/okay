## ring-standing-receiver — zero-allocation ring wakes tried and dropped; the chunk-on-ring work paused

After ring-chunk-bimodal-forks named the sides' wake-ups as the chunk
road's slow regime, the cheaper of the two fixes was built: a single
`Pending` object per registration (callback, race cell and parking
place), an int ring of wake-ups instead of a queue, and the source's
continuation applied by the merge's own drive instead of the producer's
thread. Measured, 10 forks: the chunk road's slow regime unchanged
(okayChunked 6/10 forks ~250 us); 5 forks x 2 rounds on the elementwise
merge: parity with master at every capacity. Dropped — the code stays
the simpler one. The wake work was not the cost; the channel handing a
parked receiver ONE element per wake is. The operator paused the
chunk-on-ring work ("потом вернемся"): `ring-standing-receiver` is back
in the backlog with that finding, and `ready-merge-chunk-forward` notes
it. Rows in `src/jmh/history.d/…-ring-standing-receiver.tsv`.
