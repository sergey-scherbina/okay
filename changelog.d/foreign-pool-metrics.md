## foreign-pool-metrics — the foreign pools say what they did

okay-py's `Pool` counts interpreters opened, deaths and restarts (an
open that replaced a dead one), says how many it holds and lends
(`live`, `borrowed`), and tells `Pool.listen` listeners of each open on
the thread that caused it; `Pool.held` sums every live pool of the JVM.
okay-cluster's `Meter` takes `opened(restart)` and a `holding` reader,
`Work` carries foreignOpened/foreignRestarts/interpreters/borrowed, and
`JobStats` renders `okay_job_foreign_interpreters_opened_total`,
`okay_job_foreign_restarts_total` and the per-worker gauges
`okay_job_foreign_interpreters` / `okay_job_foreign_borrowed`; the trace
spans carry the counts. okay-foreign-cluster installs the listener with
its first foreign call (and on the stateful road). TestPoolCounters (no
interpreter) and TestPoolMetrics (Live: python3 killed mid-run → one
restart in the metrics, answer unchanged); restart mutant caught.
specs/dataflow.md stage 15, docs/modules/okay-cluster.md.
