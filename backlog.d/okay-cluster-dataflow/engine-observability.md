- [ ] engine-observability — metrics and traces of a JOB (operator,
      2026-09-26). okay-pool's `/metrics` has one gauge (`okay_pool_queued`);
      okay-cluster uses neither okay-ops nor okay-obs, so a running job says
      nothing about itself: per-partition rows and bytes, time in the
      foreign function vs the wire vs the engine, worker burials and
      recomputes, epoch lag in streaming, interpreter restarts and leases in
      the foreign pools. Needs: those as Prometheus metrics on the pool
      process, and a trace per job (okay-obs OTLP) with a span per partition
      and per foreign call. Gate: one job's trace shows where its time went
      within 5% of its wall clock; a killed worker shows as a burial and a
      recompute.
