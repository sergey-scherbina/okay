# Spark evidence adapter

The adapter in `okay-spark` computes partition summaries as Spark action
results. Spark's scheduler selects successful task attempts; attempt numbers
are diagnostic identifiers, never a rule for choosing winners. The driver
passes the collected summaries to a caller-owned sink and publishes a receipt
only after that sink returns a durable commit acknowledgement.

`SparkEvidence.run(rdd, runId)(canonical)(sink)` records partition IDs,
successful task attempt IDs, counts and length-framed SHA-256 input digests.
The sink receives sorted summaries and returns a receipt identifying its
committed output. A thrown exception produces no committed evidence value.
This adapter commits summaries; it does not claim atomicity with an unrelated
Spark data write. Applications needing both must provide a sink that commits
output and manifest in one transaction/idempotent publication protocol.

Same partition layout and record order yield the same logical digest. Changing
partition layout or shuffle order can change it; no partition-independent
digest or bitwise floating-point replay is promised. Empty partitions count.
Canonical encoding is a serializable, deterministic function supplied by the
caller. No payloads or credentials enter task logs.

## Behavior

- [ ] Real local Spark returns one summary per partition and correct counts.
- [ ] Repeated execution has the same logical digest, ignoring attempt IDs.
- [ ] Record framing distinguishes ambiguous concatenations.
- [ ] Sink failure yields no committed receipt.

## Scope

Neutral Spark records bridge to `okay-fin/evidence` through an application
adapter. Durable storage, IAM, signatures and regulatory policy remain caller
responsibilities. Flink integration is a separate lane.
