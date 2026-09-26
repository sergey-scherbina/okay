## cross-process-exchange — the hash exchange between machines

A `Shuffled` job (a first keyed stage, `key`/`agg`, and a second stage,
`andThen`, over its `(key, value)` output) runs with
`Cluster.shuffle(job, p, parts, reducers, peers)`: the map side holds
its hash buckets on the worker (`Req.Shuffle`), each reducer fetches
its bucket of every partition from its holder directly (`Req.Fetch`,
worker to worker, by address) and answers the second stage's partial;
the coordinator sees sizes and partials, never a bucket. A dead
reducer's share is asked of a survivor; a dead holder's buckets are
reported `Lost` and exactly those partitions re-mapped (bounded at 16
rounds). `Cluster.exchanging(self, dial)` is the worker, and
`WorkerMain` serves it. Buckets are placed by a hash of the key's
encoding, not `##`. specs/dataflow.md stage 14; `TestShuffle` (six
in-process tests plus four real processes, one killed as the reduce
side starts, Live).
