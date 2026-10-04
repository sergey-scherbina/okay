## okay2-pg - the Postgres wire driver and its crypto seam on okay2

Third lane of okay2-jdbc-tails (operator: "do everything needed for
okay2, all at once"). okay-pg ported as `okay2-pg` (JVM and Scala.js),
with okay-crypto's primitive seam as `okay2-crypto`:

- `okay2-crypto`: `Crypto` (HMAC-SHA256, SHA-256, PBKDF2, randomness),
  JCA on the JVM and node:crypto on JS; TestCrypto's published vectors
  pass on both.
- `Scram` (phase objects and the one-object adapter; the RFC 7677 vector
  on JVM and JS), `PgSql` (startup + SCRAM over okay2-platform's `Net`,
  portals, describe with catalog nullability, transactions with the
  COMMIT tag read, COPY IN, composites and arrays, MAXDIM as the parser's
  bound), `Load`, `PgTarget`, `PgTls` (JVM).
- The TLS client half okay-tls has lives in okay2-pg's JVM sources;
  okay-conf's `Secret` is not ported, so the mTLS client key is a file
  path, and a value holding PEM is refused by name.
- The Live suites, tagged `Live`: TestPg, TestPgComposite, TestCopy,
  TestAcceptance (one typed program over PgSql and JdbcSql/H2),
  TestPgTls, TestPgMtls, and TestPgNode on JS.
- The seven bounded recursions of PgSql are in
  specs/stack-safety-okay2.tsv with okay-pg's bounds.

Spec: specs/okay2.md stage 42, "okay2-jdbc-tails, lane 3"; docs §34.
