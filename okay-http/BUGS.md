# Bugs — okay-http

Defects owned by this module. Status lives in the machine-readable
header, never in prose.

## nio-serve-stall — a write completion that never fires under channel churn
<!-- status: fixed
     lane: nio
     area: okay-http/src/main/scala-jvm/okay/http/Nio.scala
     gate: okay-http/src/test/scala-jvm/okay/http/TestNio.scala (churn test)
     fixed-in: 198c802
     confirmed: no -->

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

Root cause, found 2026-09-01 by elimination: the OS. Under rapid
LISTENER churn (bind/listen/close cycles) macOS loses a freshly
established connection at ~1.2/1000 rounds — the kernel completes the
handshake into the backlog, never delivers it to accept, and closes it
with a clean FIN. Measured identically on blocking and asynchronous
channels (stage counters: accepted=false, nothing sent, nothing read;
the parked blocking accept never woke while its client connected and
got EOF; sequential ports rule out reuse collisions). With ONE stable
listener: 8000/8000 connections clean. So the loss is below both
channel APIs; no transport code can prevent or even detect it (the
FIN is indistinguishable from a server choosing to close).

Fix: Nio rewritten on blocking channels parked on virtual threads
(specs/nio.md) — not because that stops the OS, but because it is
simpler, measured equal (docs/benchmarks.md, cluster transport), and
cannot ADD userland completion-dispatch loss on top. The regression
gate holds the guarantee the code can make: one listener, 500
connections, every line delivered (8000 at fix time).

### Original report (superseded 2026-09-01)
The entry above initially blamed the JDK asynchronous layer and then
`AsynchronousSocketChannel.close()`; both were eliminated by the
blocking rewrite reproducing the loss at the same rate. A harness
pitfall recorded on the way, so it is not relearned: a watchdogged
round that times out leaks its listener and thread — under such a
harness only a run's FIRST failure is trustworthy.
