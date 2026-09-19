## dataflow-kafka-eos - stage 11's last box was waiting on a broker, and this machine can have one

The box read "the same exactly-once run on `KafkaStore` when a broker
is available (Live) — still open, and it needs a broker rather than a
decision". Docker is on this machine, so the broker was a `docker run`
away and the box was open for want of asking.

WHAT IT ASSERTS IS NOTHING NEW, and that is the design rather than a
shortcut. `TestStagingTopic`'s four scenarios became
`StagingTopicSuite`, a battery over whatever `Topic` the suite
supplies; okay-cluster supplies a `MemoryStore` one and okay-kafka's
new `TestDataflowKafka` supplies a `KafkaStore` one. The two runs
cannot drift into asserting different things, which is what a second
hand-written Kafka suite would eventually have done.

Against a single-node `apache/kafka:3.9.0`: a quiet run writes every
pane once (3.9 s); a coordinator dying between the append and the
journal commit, at four epochs, is succeeded by a process with NO
memory of its own that learns what landed by reading the output's tail
(16.1 s); a death after the commit appends nothing again (14.6 s); a
fresh writer over a filled log knows the epoch (6.0 s).

The only thing a real log can break that a memory one cannot is the
READING BACK — offsets, `TooEarly`, a tail that is eventually
consistent — and the whole dedup road stands on exactly that, which is
why this box was worth the broker.

THE BUILD EDGE IS TEST->TEST AND ONE WAY. okay-kafka borrows
okay-cluster's test battery; okay-cluster still does not know what a
Kafka is, and its compile graph still stops at okay-codec — the
`Checkpoint` seam is two methods over bytes on purpose, and a
`stagingTo(topic)` in the engine would undo that.

It skips in 16 ms when nothing is listening (`TestKafkaSupport`'s raw
TCP probe, ahead of the Kafka client's own generous timeouts), and the
skip was checked by pointing it at a dead port rather than assumed —
a Live suite that silently passes with no broker is worse than none.
