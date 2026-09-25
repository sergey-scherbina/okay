## localhost-connects-to-a-stranger - tests connect to the loopback address a server is bound to

On macOS "localhost" tries `::1` first, and on this shared box a
neighbour's socket can hold the same ephemeral port number over IPv6.
remote-arrow-frames saw that give a Broken pipe once and a stall once.
Its tests were already moved.

- `TestCluster` ("a socket worker dies mid-stream"): the server is bound to
  the loopback address and the client connects to it by that address. The
  test binds a port, so it is now Live-tagged (integration-test-gate); it
  was in the default gate.
- `TestTls`: all six clients connect to the loopback address, and the two
  plain servers bind to it. "localhost" stays as the name the certificate
  is checked against.
- `TestAcme` is unchanged: its "localhost" is a certificate domain, not a
  connect address.
