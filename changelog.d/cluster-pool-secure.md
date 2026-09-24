## cluster-pool-secure - mTLS, a capability, a NetworkPolicy

Stage 4 of specs/cluster-pool.md: the door and the wire, secured.
mTLS between pool members through `okay.tls.Tls` (`mutualContext`/
`mutualServerSocket`/`mutualClient`), one shared certificate as both
identity and trust anchor on every member (`SslMode.VerifyCa` — no
hostname check, since a pool member dials another by a Kubernetes DNS
name or a bare IP no single certificate names); an `okay-security`
`Capability` checked at `POST /pool/jobs/{name}` before the job name is
even looked up, so a stranger with none learns nothing; `PoolConf`
refusing to start with neither TLS nor a capability configured unless
`OKAY_POOL_INSECURE=true`; a NetworkPolicy on `cluster`.

A real openssl-generated-certificate test (`TestPoolSecureLive`) found
that `setNeedClientAuth(true)` alone does not refuse a peer presenting
no certificate at all — TLS 1.3 (RFC 8446 §4.4.2) leaves that up to
the server, and this JDK's SunJSSE completes the handshake anyway.
Fixed with `Tls.verifyingClientAuth`: every accepted connection's peer
certificates are checked before it is ever handed to `Served.serve`,
closing and moving on to the next connection rather than trusting the
handshake's own success — which also stops one bad peer from taking
the whole listener down with an exception the accept loop did not
expect.

The NetworkPolicy reused a field the model already had rather than
inventing one: `Need.Neighbour` already says one service reaches
another, so it names the submitter. `cluster` admits the worker
protocol's private port from the pool's own pods and any sibling
service naming the pool a neighbour, no `namespaceSelector` (already
"this namespace only"); the HTTP door (`public`) is left alone since
narrowing it would contradict its own declaration, and a `Need.Peers`
service with no private port renders no policy. Proven with real
`helm lint`/`helm template` against a fixture shaped like `okay-pool`
itself.

218 tests across `okay-pool`/`okay-tls`/`okay-deploy`'s default suites
(fresh `clean` first), clean compile, six real `Live` tests (three
socket-level mTLS, three `helm`).
