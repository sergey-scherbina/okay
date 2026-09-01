# TLS for the own wires

## Overview

The stack is growing wire protocols of its own — the pg client
(specs/sql.md), the remote Topic client (specs/persist.md), RESP
(specs/cache.md), QAP1 (specs/r.md), the cluster's remote channels —
and none of them had a transport-security story: the 2026-09-01
audit's most serious finding, because "внешняя база клиента" in
production means TLS or it means nothing. This spec states the
answer ONCE, at the layer where it belongs: encryption wraps the
TRANSPORT, not the protocols — every protocol above the Async
transport gets TLS from one seam and adds nothing of its own.

## The seam

- `Tls.client(cfg)` / `Tls.server(cfg)` wrap an Async-transport
  connection before any protocol bytes flow. Platform fills the
  crypto (the specs/security.md doctrine — no own crypto, ever):
  JVM = platform TLS over the transport the own wires ACTUALLY use
  — which today is the blocking `java.net.Socket` (pg-wire, Loom
  parks in it), so the JVM leg is `SSLSocket`; the `SSLEngine`
  state machine joins when an NIO consumer appears (adjusted at
  wire-tls, recorded below). Node = the `tls` module; Native =
  platform TLS via interop, staged.
- **Vocabulary: postgres's `sslmode`, adopted stack-wide** because
  it is the one operators already know, and it names the honest
  levels: `disable` | `require` (encrypt, no identity check) |
  `verify-ca` | `verify-full` (hostname too). The default
  everywhere is `verify-full`; anything weaker is a NAMED decision
  in config (the Ack pattern, security edition).
- Config per specs/conf.md: cert/CA paths are plain fields, PRIVATE
  KEYS are `Secret` references (`file:` under 0400) — invariant 2's
  sibling: a private key never inlines.
- Server side: a wire server (persist-wire) terminates TLS itself
  via the same seam, OR deployment terminates at a proxy — a stated
  legitimate mode, not a workaround; the wire's auth (bearer, per
  specs/persist.md) does not depend on which.
- mTLS (client certificates) is the staged second step: the seam's
  config carries an optional client identity from day one so adding
  it changes no signatures.

## Behavior

- [x] `verify-full` refuses a wrong hostname and an unknown CA,
      each named as data; `verify-ca` accepts the wrong hostname
      and the spec says so out loud
- [x] `require` refuses a plaintext server; `disable` connects and
      is loggable as the named decision it is
- [ ] (pg lane) the pg driver speaks sslmode through this seam (postgres's
      STARTTLS-style SSLRequest dance lives in the pg driver; the
      session it hands over is this seam's)
- [x] (persist-wire lane) persist-wire over TLS passes the same acceptance suite as
      plaintext (persist-wire-tls, landed): the wire's transport is
      INJECTABLE — `Wire.Server` takes a `ServerSocket` (an
      SSLServerSocket from `Tls.serverSocket`), `Wire.Remote.connect`
      takes a `Socket => Socket` wrap (the `Tls.client` wrap) — so
      the handshake, capability grant, frames and refusals are
      byte-for-byte the plaintext behaviour with every byte encrypted
      underneath, and okay-persist keeps its core-only compile graph
      (okay-tls is TEST scope). TestWireTls: encrypted grant, append/
      read round-trip, refuse-by-name, and a PLAINTEXT client refused
      by the TLS server — live over an openssl localhost identity,
      skips where openssl is absent
- [x] a private key configured inline (not a Secret ref) is refused
      at config decode (the seam refuses a ref that smuggles PEM,
      client and server)

## Out of scope

- own cipher/protocol choices beyond the platform's defaults —
  TLS 1.2+ as the platform provides; no cipher-suite tuning API
  until an operator names a need
- certificate issuance/rotation automation (ACME) — deployment's
  concern; the seam reloads on reconnect, which is enough for
  rotation-by-replacement

## Decisions

- **One seam at the transport, not per protocol** — N protocols ×
  1 TLS instead of N TLS stories; also what makes the
  proxy-termination mode uniform. Rejected: per-protocol TLS
  options.
- **pg's sslmode vocabulary stack-wide** — operator-legible,
  honest about the weak modes by naming them. Rejected: a
  boolean `ssl: true` (hides verify-vs-not, the distinction that
  matters).
- **verify-full as the only default** — weak modes exist for real
  topologies (sidecars, tunnels) but are opt-in by name. Rejected:
  require-by-default (the industry's quiet MITM).
- **Platform crypto only** — JCA/node:tls/platform TLS; the
  security spec's rule extended to transport. Rejected: bundling a
  TLS implementation.
- **SSLSocket before SSLEngine** (wire-tls) — the wires this seam
  exists for run on blocking sockets under virtual threads, and
  `SSLSocket` IS the platform's TLS for that transport; driving an
  `SSLEngine` state machine for an NIO consumer that does not exist
  yet would be machinery for a need nobody named. The seam's
  signature (wrap a connected socket) survives the addition.

## Results (the seam)

Shipped 2026-09-01 (wire-tls): okay-tls (jvm, depends on okay-conf
for Secret/Secrets). The whole sslmode ladder proven against LIVE
handshakes with an openssl-generated identity (the suite skips
where openssl is absent): verify-full completes with the CA and
refuses the wrong hostname and the unknown CA by name; verify-ca
accepts the wrong hostname and the TEST says so out loud; require
tunnels and refuses plaintext; disable is the named decision.
Private keys travel as Secret refs — PEM smuggled into the ref is
refused at the seam, both halves. The server half terminates TLS
from a PEM cert + PKCS#8 key ref. mTLS fields ride in TlsConfig
from day one, so adding it changes no signatures.

persist-wire over TLS landed (persist-wire-tls, 2026-09-01): the
first consumer of the seam. The wire's transport became INJECTABLE
rather than TLS-aware — `Wire.Server` accepts a `ServerSocket`
(pass the SSLServerSocket from `Tls.serverSocket`), and
`Wire.Remote.connect` accepts a `Socket => Socket` wrap (pass the
`Tls.client` wrap, whose contract is exactly "wrap the connected
socket BEFORE any protocol byte flows"). So okay-persist gains
encryption without a compile dependency on okay-tls — it is TEST
scope only, and the SSLSocket is built by the caller. The acceptance
is that NOTHING in the wire changed: the same handshake, grant,
frames and refusals run byte-for-byte over the encrypted transport
(TestWireTls), plus a plaintext client is refused by the TLS server.
Still open: the pg lane's sslmode dance (SSLRequest preamble then
the same seam), for that lane.
