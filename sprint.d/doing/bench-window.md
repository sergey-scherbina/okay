- [ ] bench-window — a benchmark gets a quiet box BY PROTOCOL, not by
      luck: every gate/runner holds a token while it runs; a waiting
      JMH lane files a request, after which NEW gates wait at their
      start (running ones finish); when no token is left the window
      opens and the queued lanes run one by one under a time budget,
      then gates resume. Readers–writers with writer preference; both
      waits bounded. First measurement: whether demoting running gates
      to E-cores (`taskpolicy -b`) instead of draining them keeps the
      control lane quiet. Found by ready-merge: 101 jmh-lane tries in
      an hour, never a quiet window. specs/bench-window.md.
      (2026-09-26, operator ask)
