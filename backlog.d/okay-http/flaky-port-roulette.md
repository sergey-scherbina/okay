- [ ] flaky-port-roulette — the full-matrix port/readiness family,
      one ledger: TestMcpHttp 503 (2026-09-01), TestResumable first
      subscribe, TestHttp first GET 404, and TestWire reading
      literal "HTTP" bytes at its handshake (a foreign server
      answered on the expected port) — all green alone, all under
      parallel suites in one sbt JVM; suspect ephemeral-port reuse
      between a closing listener and a dialing client
