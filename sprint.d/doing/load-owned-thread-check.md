- [ ] load-owned-thread-check — unblock evidence publication by checking Load-owned threads, not the JVM-wide thread count.
      CI 20261005T201449Z reproduced TestLoadStress line 19 alone.
      Capture the burner identities during the body; prove each terminates after normal and exceptional exit while unrelated threads remain alive.
      Deterministically reproduce the old assertion with three unrelated threads started inside the body. Adopt Diagnosed and preserve failure snapshots.
      Gate: okayTest/testOnly okay.testkit.TestLoadStress; runner alone owns the full-build push.
