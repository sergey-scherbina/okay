## okay2-refine-yaml-cbor - okay2-refine's format level reads YAML and CBOR

okay2-refine's `Format.detect` is okay-refine's level now:
`cbor <|> (text andThen (json <|> xml <|> yaml))`, over okay2-codec's
YAML tree and CBOR reader (both since okay2-codec-text and
okay2-codec-cbor). `Doc` gains `Yaml` and `Cbor`; `Format.yaml` declines
flow style and an XML prolog before parsing and a root scalar after it,
as okay's does; `Format.cbor` takes exactly one well-formed item;
`Format.value` projects YAML into the same `Json` as JSON and declines
CBOR ("no value projection for cbor without a schema").

Behaviour change for callers reading refusals: every detection now
carries a `cbor` refusal first and a `text/yaml` one beside the others
(TestFormat and TestSchemaPattern restated). TestFormat 6 -> 10.

The four sentences that promised it "the day okay2-codec reads them"
(Format.scala, okay2-refine/README.md, okay2/build.sbt, docs/okay2.md
section 31) are corrected; specs/refine.md's okay2 note says it happened.
Closes okay2/backlog.d/modules/okay2-refine-yaml-cbor.md.
