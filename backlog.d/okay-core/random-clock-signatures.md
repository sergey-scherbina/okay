- [ ] random-clock-signatures — PRIORITY: LOW (decision). Found by the
      core review 2026-09-26. Randomness exists only INSIDE Prob.scala
      (a distribution effect) and Sim.scala (a simulator's own ops), and
      time exists in Provide (a clock given as a dependency) and in
      okay-async (sleep, timeout). There is no plain `Random` or `Clock`
      signature a program can perform and a test can answer
      deterministically. The decision to make: whether they belong in the
      core (tiny signatures + a real handler + a fixed/seeded one) or in
      a module next to their consumers. Against the core: the real
      handler touches the platform (`System.nanoTime`,
      `SecureRandom` on JVM, `Math.random` on JS), and Provide already
      answers "give me the clock" by dependency. For: a deterministic
      test of any retry/backoff/jitter/id code needs exactly these two,
      and every consumer re-invents them. Survey first: grep for
      `System.nanoTime`, `currentTimeMillis`, `Random` in main code, and
      count how many would move.
      SURVEYED 2026-09-26: 54 main files read the time directly
      (nanoTime/currentTimeMillis/Instant.now), mostly timeouts, metrics
      and logs. 19 read randomness, one per module, and much of that is
      SecureRandom for keys and tokens (x402, security), which must NOT
      become a swappable effect. DECIDED with the operator: not in the
      core. A small `Clock` with a deterministic handler goes where
      retry/backoff/timeout live (okay-resilience/okay-async) when the
      first test needs it; no `Random` signature.
