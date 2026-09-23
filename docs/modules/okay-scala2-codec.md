# okay-scala2-codec

okay-codec for **Scala 2.13**. It replaces the two parts of okay-codec
that a Scala 2 compiler cannot use: the `derives Schema` macro, and the
`Json` value type, whose TASTy scalac 2.13's reader cannot read.

| | |
|---|---|
| `Schemas` | `product1` … `product16`, `constant`, `sum`/`variant`: a product's, a case object's and a sealed hierarchy's `Schema`, built in the style of circe's `forProductN` |
| `Json` | `write`, `read`, `readStrict` over text |
| `JsonSchema` | `of(schema)`: the JSON Schema declaration, as text |

Everything else is okay-codec, used directly from Scala 2:
`okay.codec.Schema` (with its instances, which Scala 2's implicit search
finds), `Cbor`, `Yaml` and `Validate`. The wire format is okay-codec's,
so Scala 2 and Scala 3 services read each other's bytes.

The walkthrough, with the example model, is section 8a of
[okay from Scala 2.13](../scala2.md#8a-codecs-json-cbor-json-schema). The
signatures are in [okay-scala2](okay-scala2.md#api-reference), and the
probe results are in [specs/scala2-facade.md](../../specs/scala2-facade.md),
stage 6.
