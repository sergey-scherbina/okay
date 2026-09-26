## engine-kafka-source — Kafka as a source of the cluster engine

okay-kafka's `KafkaSource`: a topic's Kafka partitions are a job's
partitions (`KafkaSource.spans` snapshots `[from, until)` per partition
as the job's parameters, `KafkaSource.flow` is the flow). A partition
opens at its position and SEEKS there, so a replacement worker or a
resumed coordinator reads exactly the records the journal has not
folded; positions live in the engine's journal, not a consumer group.
Offsets with holes (transactions, compaction) are refused by name, or
read by skipping with `contiguous = false`. okay-cluster gained
`Flow.opened` — a source partition that holds a resource, opened at its
position and closed with the partition's scope — and okay-kafka now
depends on okay-cluster in compile scope. `TestKafkaSource` (Live): 1M
records over 4 partitions, a worker killed mid-epoch, every record
counted once and the replacement seeking (counted); `TestFlowOpened`.
