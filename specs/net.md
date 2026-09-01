# Net: one socket leg for every wire

## Overview

Three protocols in this stack speak raw TCP — the cluster's frames,
persist's wire, Postgres v3 — and until now each JVM client held a
`java.net.Socket` of its own while the JS side had one bespoke
`net`-module dance (the cluster's acceptance client). The promised
non-JVM consumers (persist-wire-node, sql-pg-node) all need the
same missing piece: a CROSS-PLATFORM byte-stream seam, small
enough to be honest on every platform.

`Net` is that seam. A `given` per platform (the Timer/CanBlock
pattern): the JVM and NATIVE leg is a blocking socket behind
`Async.Run` — virtual threads make it real on the JVM, and Scala
Native ships `java.net.Socket`, so ONE file in `scala-jvm-native`
serves both; the JS leg wraps Node's `net` with buffered `data`
events and `Async.await` pulls — nothing blocks, the event loop
drives (the cluster client's proven shape, made reusable).

```scala
trait NetConn:
  def readFully(n: Int): Array[Byte] ! Async  // exactly n, or throws at EOF
  def write(bytes: Array[Byte]): Unit ! Async
  def close(): Unit

trait Net:
  def connect(host: String, port: Int): NetConn ! Async
// given Net per platform; Net.connect summons it
```

Framing stays with the protocol that owns it — the seam moves
bytes. What this unlocks immediately: persist's Wire protocol
(`[len][CBOR]`, a documented surface) moves to SHARED code —
`WireProtocol` holds Version/Req/Resp once, `export` keeps the
existing `Wire.*` paths compiling — and a cross-platform
`WireClient` speaks it over `Net` from any platform. The openness
acceptance is literal: a JS test runs a SCRIPTED NODE SERVER
(Node's `net.createServer`) answering canned frames encoded with
the SAME shared enums, and the client talks to it with no JVM
anywhere in the process.

sql-pg-node stays its own claim, honestly: `PgSql`'s message pump
is written against synchronous reads (correct on the blocking
leg), and porting it to Node means restructuring the pump around
async pulls — real work this seam enables but does not do.

## Behavior

- [x] the JVM leg: `WireClient` over `Net` passes the wire battery
      against the existing `Wire.Server` (hello/capabilities,
      byte-exact append/read, refusals by name, TooEarly through)
- [x] the JS leg: the same `WireClient` code talks to a scripted
      Node `net` server answering frames built with the SAME shared
      enums — Granted's capability list arrives, an Append answers
      its offset, a Refused throws by name; no JVM in the process
- [x] `readFully` at EOF mid-frame throws naming the shortfall —
      a half-frame is damage at the transport, not a hang
- [x] `Wire.*` call sites compile unchanged (the export), and the
      jvm server suite (TestWire) is untouched and green

## Out of scope

- TLS — rides wire-tls at this same seam when it lands (stated in
  every wire spec already)
- sql-pg-node — the PgSql pump restructure, its own claim
- connection pooling, timeouts-as-policy — callers' compositions

## Decisions

- **One jvm-native file** — Scala Native ships java.net.Socket;
  writing the blocking leg once in `scala-jvm-native` is the same
  move the core's Channel already makes. Rejected: three legs.
- **The seam moves bytes, protocols keep their frames** — pg's
  tagged messages and persist's `[len][CBOR]` differ; a framing
  opinion in the seam would fit one and warp the other. Rejected:
  a message-level transport.
- **Node reads are buffered pulls, not push callbacks** — the
  client pumps are written as sequential programs (read frame,
  decide, write frame); an event-push API would force every
  protocol into callback shape. `data` events fill a buffer, an
  Await drains it — the adapter absorbs the impedance once.
  Rejected: exposing the event API.
- **Additive** — Wire.Server and the jvm Remote stay; WireClient
  arrives beside them; PgSql is untouched. (The operator's rule.)

## Results

Landed (wire-node, 2026-09-01). Net in the core: the trait plus
`NetEof` shared; ONE blocking file in scala-jvm-native serves the
JVM and Native; the Node leg buffers `data` events behind
Async.await pulls (the cluster client's dance, made a given).
persist's protocol moved to shared `WireProtocol` — Version, the
enums, the frame helpers over `NetConn`, and the cross-platform
`Client` — while `export` kept every `Wire.*` path compiling and
the jvm server suite green untouched. The headline test is the
sentence the persist spec promised: the SAME client code talks to
a scripted Node `net` server answering frames encoded with the
SAME shared enums — capabilities, offsets, TooEarly and a refusal
by name crossing the wire — with no JVM in the process. sql-pg-node
now has its transport; the pump restructure remains its claim.
