- [ ] foreign-pool-metrics — the foreign pools say what they did
      (found by engine-observability, 2026-09-26). A job's time in foreign
      code reaches the coordinator (stage 15's `Meter`), but the pools
      under it — okay-py's `PyWorkers`/`SupervisedWorker` — report
      nothing: interpreters opened, restarted after a death, leases held
      by stateful stages. Needs: counters in the pool, read through the
      same `Meter` (or beside it) so a measured worker answers them and
      `JobStats` renders them. Gate: a job whose interpreter is killed
      mid-run shows one restart in `/metrics`.
