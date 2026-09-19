# okay-conf

Configuration as data, secrets as references (specs/conf.md): a config is a case class with a derived `Schema`; a `Secret` is a REFERENCE (`env:PG_PASSWORD`, `file:/run/secrets/pg`) — the value exists only in the gap between `Secrets.get` and a constructor argument, so nothing okay-owned can ever persist it. Cross-built; depends on okay-codec only.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-conf.md`](../docs/modules/okay-conf.md) | what it is, and the reasoning |
| [`specs/conf.md`](../specs/conf.md) | the design and its decisions |
