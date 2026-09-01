# Bugs — okay-http

Defects owned by this module. Status lives in the machine-readable
header, never in prose.

## nio-serve-stall — a write completion that never fires under channel churn
<!-- status: open
     lane: nio
     area: okay-http/src/main/scala-jvm/okay/http/Nio.scala
     gate: none -->

Found 2026-09-01 by the `ClusterTransportBenchmark.nio` lane (3/6 forks
failed the sum assertion); investigated the same day (master 56959b7).
Repro: loop a listen/connect round — server sends 100 lines of ~480
bytes, per-line, then closes; client folds lines to EOF. At ~1.3/1000
rounds the serve fiber STALLS: its next `ch.write` completion never
fires, `onComplete` never runs (stall, not death), after 0–4 completed
writes; the client usually sees a premature EOF, sometimes a pure hang.
Once: the listener's accept failed with AsynchronousCloseException
while the client still received data.

Cleared: okay's Async driver (Await cell CAS protocol read line by
line; sound). Prime suspect: the DEFAULT AsynchronousChannelGroup's
handler dispatch under rapid channel create/close churn (macOS/KQueue).
Next: (a) dedicated-group isolation experiment, (b) shutdownOutput +
drain before close in Nio.Conn, (c) jstack of the stall.

Harness pitfall (recorded so it is not relearned): a watchdogged round
that times out leaks its listener and thread — under such a harness
only a run's FIRST failure is trustworthy.
