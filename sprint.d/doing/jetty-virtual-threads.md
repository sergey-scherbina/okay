- [ ] jetty-virtual-threads — okay-jetty's Server(...) uses Jetty 12's
      own VirtualThreadPool instead of the default QueuedThreadPool
      (platform threads) for its main connector.

      WHY: operator asked "delai esli nuzhno" after the Scoped MRJar
      work -- assessed: yes. okay-http's own JDK backend already
      forks a virtual thread per request (Executors.
      newVirtualThreadPerTaskExecutor); okay-jetty's streaming-body
      write path already does too (Jetty.scala:228, ad-hoc
      Thread.startVirtualThread); only the MAIN connector's request
      handling was still on ordinary platform threads. This closes
      that gap AND is what makes the JDK25 ScopedValue side of
      okay.Scoped (script-scoped-state-mrjar) actually pay off --
      ThreadLocal-vs-ScopedValue overhead is close to zero on a
      bounded platform-thread pool, and grows with virtual-thread
      volume, which this is the piece that was missing.

      HOW: `Server()` -> `Server(new VirtualThreadPool())`
      (org.eclipse.jetty.util.thread.VirtualThreadPool, confirmed
      present in jetty-util 12.0.13, implements ThreadPool, Server's
      own constructor already accepts one -- no new dependency).
      Costs nothing on our JDK21 floor (Jetty 12 requires JDK17+, and
      VirtualThreadPool itself needs the same JDK21 virtual-thread
      APIs this project already requires everywhere via Loom).

      DONE WHEN: okay-jetty's test suite passes unchanged (no test
      depends on thread-pool internals, checked); gate green.
