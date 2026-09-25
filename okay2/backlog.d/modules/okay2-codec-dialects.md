- [ ] okay2-codec-dialects: the rest of okay-codec on okay2, once a
      caller needs it. okay2-codec (stage 41) ported `Schema` and JSON
      only. Still unported: `Cbor` (RFC 8949, and the BigInt preferred
      serialization TestBigInt pins), `JsonSchema` (the fold's first
      algebra, with `$defs`/`$ref` and `enum` from a vocabulary; the
      JsonSchema parts of TestSchemaFold and TestEnumeration wait on
      it), `Validate`, `Digest`/`Compat`, `Edn`, `Yaml`, `Markdown`,
      `Columns`, the staged codecs, and the `Codecs` provider registry.
      XML is `okay2-xml`. (2026-09-25)
