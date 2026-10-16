- [ ] pg-channel-binding — `channel_binding=require` in a Postgres URL is
      ignored by `PgTarget.parse` today (an unknown parameter is dropped),
      and SCRAM proceeds WITHOUT binding to the TLS channel. Neon's
      connection strings carry it by default. Either implement
      SCRAM-SHA-256-PLUS (`tls-server-end-point`: the hash of the server
      certificate, RFC 5929 §4, as libpq does) or refuse the URL by name. A
      requirement the caller wrote must never be silently dropped. Found
      while reading the driver for pg-neon (2026-10-16).
