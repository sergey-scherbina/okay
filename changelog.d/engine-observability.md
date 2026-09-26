## engine-observability — a job says where its time went

Every coordinator (`Cluster.run`, `stream`, `leading`, `runLeading`,
`shuffle`, `Job.lead`) takes a `probe` and emits `Seen` events: the
run, its phases (plan, extent, partitions, merge; map, reduce; each
stream epoch with its watermark lag; close), every attempt, every lost
attempt and burial. `JobStats` renders them as Prometheus text
(`okay_job_*`), `JobTrace` as a span tree. `Cluster.measured` wraps a
worker so each answer carries its rows and its time, split into engine
and foreign (okay-foreign-cluster's `Attempts.run` meters every foreign
call); the wire is the rest of the round trip. okay-pool serves the
metrics on `/metrics` and each run's trace as OTLP JSON at
`GET /pool/runs/{id}/trace`; its members answer measured by default.
Found on the way: a measured peer made every exchange fetch read as
lost (fixed), and building a run is 6% of a cold 350 ms run (now its
own phase). specs/dataflow.md stage 15; TestObserved, TestPoolObserved,
TestForeignStage.
