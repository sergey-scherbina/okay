- [ ] refine-dispatch-kafka — specs/refine-dispatch.md stage 2: a split's
      lanes to Kafka topics (`KafkaRouting.toTopics`: an encoder per lane,
      rejects to a dead-letter topic with reasons, commit after the chunk
      is routed), on MockConsumer/MockProducer. Waits for stage 1.
