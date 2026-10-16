- [ ] okay-tx-session — specs/okay-caps.md, "In okay-tx": the JDBC
      bracket's connection handed out as a guarded Session (a
      caps.SharedCapability), closed when the bracket commits or rolls
      back — a statement on a session kept past its bracket is
      Session.Closed, not a statement on a connection back in its pool;
      beside Jdbc, which stays.
