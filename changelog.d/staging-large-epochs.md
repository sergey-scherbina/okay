## staging-large-epochs — exactly-once output larger than one record

okay-kafka's `EpochLog(bootstrap, topic, transactionalId, chunkBytes)`
writes a dataflow epoch of any size as length-prefixed chunks inside
ONE Kafka transaction; a `read_committed` reader sees each epoch whole
or not at all. A writer that dies mid-epoch is fenced by its successor
under the same transactional id, which aborts the open epoch and learns
the last committed epoch from the log; `move` of a committed epoch is
skipped. It plugs into a stream through `Sink.staging(...)(move)`, the
seam stage 9 left. `TestEpochLog` (Live): 100 MB epoch, writer killed
half way, reader sees nothing then the whole epoch once (7.8 s on a
local broker); a stream's panes staged into it, each once; the framing
without a broker. The mutant that commits per chunk is caught.
