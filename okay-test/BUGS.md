## load-owned-thread-check — JVM-wide count mistakes unrelated threads for leaked burners
<!-- status: open
     lane: load-owned-thread-check
     area: testkit
     gate: okayTest/testOnly okay.testkit.TestLoadStress -->

CI 20261005T201449Z failed TestLoadStress line 19, including in an isolated rerun. The preceding burner dump assertion passed. The global JVM thread count can grow independently of Load; Load clears its flag and joins its own threads. Reproduce with three unrelated threads started inside the body; replace the global count with captured burner identity/liveness checks for normal and exceptional exit.
