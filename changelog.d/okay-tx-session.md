## okay-tx-session - the JDBC bracket's connection a guarded session

- okay-tx: `Sessions[T]`, the JDBC transactor with its resource a `Session` (`execute`, `queryInt`,
  `withConnection`) — closed when the bracket commits or rolls back, before the connection: a session kept past its
  bracket throws `Session.Closed` naming the statement, never runs it on a closed or pooled connection.
  `Sessions.execute`/`queryInt` on the bracket's session; `Session` a `caps.SharedCapability` (its own,
  capture-checked file — the kernel's programs are not). `Jdbc` unchanged. TestSessions (4) on H2;
  docs/modules/okay-tx.md, okay-caps.md, specs/okay-caps.md.
