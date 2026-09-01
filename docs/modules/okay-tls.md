# okay-tls

TLS for the own wires (specs/tls.md): one seam at the TRANSPORT, not
per protocol — every wire above gets TLS from it and adds nothing of
its own. The vocabulary is postgres's `sslmode`, adopted stack-wide
because operators already know it and it names the weak modes
honestly; `verify-full` is the only default.

| | |
|---|---|
| `SslMode` | `Disable` / `Require` (a tunnel, not authentication) / `VerifyCa` (the chain, NOT the hostname — said out loud) / `VerifyFull` |
| `TlsConfig` | CA/cert paths are plain fields; the PRIVATE KEY is a `Secret` reference — PEM smuggled into a ref refuses at the seam; mTLS fields ride from day one so adding it changes no signatures |
| `Tls.client` | wrap a connected socket BEFORE protocol bytes flow; refusals are values naming what failed |
| `Tls.serverSocket` | the server half: terminate from a PEM cert + PKCS#8 key ref |

The JVM leg is `SSLSocket` — the platform's TLS for the blocking
sockets our wires actually run on (a virtual thread parks in the
handshake); the `SSLEngine` machine waits for an NIO consumer,
recorded in the spec's Decisions. The whole sslmode ladder is proven
against live handshakes with a locally generated identity.

`Tls.serverSocket(port, cert, key)` also reads an ambient `Secrets`
(ctx-everywhere) — the resolver is the flow's environment.
