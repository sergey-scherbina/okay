## spark-evidence-adapter — successful partition summaries and sink receipts

`SparkEvidence.run` collects counts and framed SHA-256 digests from successful
Spark tasks, sorts partition summaries and publishes `Committed` after the
caller-owned sink acknowledges its commit. Attempt IDs are diagnostic only.
The digest is stable for the same partition layout and record order.
Two real local Spark tests pass: repeated execution, empty partitions, framing
and sink failure. Implementation: 2e971f3dd. Spec: specs/spark-evidence.md.
