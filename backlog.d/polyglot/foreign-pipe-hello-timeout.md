- [ ] foreign-pipe-hello-timeout — a worker started over PIPES that never
      says hello blocks its opener for ever (`WireLink.Streams.hello` reads
      a line with no limit). Seen twice on 2026-09-26 (foreign-host-streams'
      Live runs, load 32–35): R's docker shim started containers that never
      printed the handshake, `TestR` waited in `readLine`, and only the
      gate's stall watchdog ended it (480 s). TCP already has `helloMillis`;
      pipes want the same, refused by name ("the worker said nothing for
      N ms"), and a supervisor then retries the open. Gate: a command that
      sleeps before its hello is refused within the limit, the process
      destroyed.
