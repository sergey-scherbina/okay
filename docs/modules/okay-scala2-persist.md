# okay-scala2-persist

okay-persist for **Scala 2.13**. The engine is okay-persist's own and is
used directly from Scala 2: `new MemoryStore`, `FileStore.open(dir)`,
`topic.append`, `topic.read`, `Offsets`, `Snapshots`, `Typed`. `Persist`
supplies what the TASTy reader does not carry over:

| | |
|---|---|
| `Persist.topic(store, name, partitions)` | `Store.topic` with its defaults, which a trait method hides from Scala 2 |
| `Persist.typed[A](topic, version)` | the typed view, Scala 3's `topic.of[A]` extension |
| `Persist.stream(topic, partition, from)` | a `Source[Record]` to the end of the log |
| `Persist.tail(topic, partition, from)` | a `Source[Record]` that waits for new records |

The walkthrough is section 8k of
[okay from Scala 2.13](../scala2.md#8k-the-durable-log-okay-persist), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
