## batch-coordinator-resume — a batch run survives its coordinator

`Cluster.run(…, journal = j)` writes each partition's partial into `j`
as it arrives (with the pre-pass's bounds), in saves that coalesce, and
a run over the same journal asks only for the partitions `j` lacks.
`Cluster.runLeading` is `leading`'s lease-and-fence seat for it. A
journal holding another run (job, parameters, width, or a stream's
fold) is refused by name and left untouched; a finished one starts
afresh; no journal, no cost. Reverses the decline specs/dataflow.md had
kept since stage 5 — the operator's case is partitions that are an hour
of an R or Python model each. `TestBatchResume`: coordinator killed as
its fourth partial arrives (a lockstep fixture — dying by save count
flaked, since coalesced saves land anywhere) over 4 workers, the successor sends a `Run` for exactly
the partitions the journal lacked (the mutant that ignores the journal
turns that red), and zero `Extent` requests.
