- [ ] engine-kafka-source — Kafka as a SOURCE of the cluster engine
      (operator, 2026-09-26: many R/Python risk and fraud models reading
      large data). Today a `Flow` reads an okay-persist `Topic`
      (`Streams.chunks`, stage 11) and okay-kafka's `KafkaStore` is tested
      only as the exactly-once staging OUTPUT; `KafkaInterop` is a client
      (a consumer as chunks, manual `commitSync`, at-least-once) that no
      `Flow` uses. Needs: a topic's Kafka partitions as the flow's
      partitions, offsets as the positions a streaming session seeks by
      (stage 11) and commits at the epoch the sink commits (stage 9), so a
      replay after a worker's death reads the same records. Gate: a
      streaming job over a Live Kafka topic of 1M records, a worker killed
      mid-epoch, every record counted once in the output.
