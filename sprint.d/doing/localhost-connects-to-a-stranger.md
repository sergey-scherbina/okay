- [ ] localhost-connects-to-a-stranger — a test that binds `ServerSocket(0)`
      (the wildcard address) and connects to `"localhost"` can reach ANOTHER
      process. On macOS "localhost" tries `::1` first, and on this shared box
      a neighbour's test can hold the same ephemeral port number over IPv6.
      remote-arrow-frames (2026-09-25) saw both outcomes measuring
      okay-cluster's Remote:
      - a Broken pipe on the sender, when the stranger closed;
      - a stall, the sender done and our listener still in `accept`. The
        in-JVM thread dump showed it (`HotSpotDiagnosticMXBean.dumpThreads`).

      Binding to `InetAddress.getLoopbackAddress` and connecting to that
      same address ended it: two full runs clean where two of four had
      failed. The same shape remains in TestAcme, TestCluster and TestTls
      (a "localhost" connect beside a port-0 server). Move each to the
      loopback address, or to `server.getInetAddress`, as TestRemote did.
      (2026-09-25)
