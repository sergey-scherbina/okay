## scala2-codec - okay-codec from Scala 2.13: Schemas in place of derives Schema, JSON as text

The first of the five areas the operator asked to wrap (HTTP, SQL,
codecs, agents, UI). Codecs came first because the other areas speak
`Schema`. It is also the area where the rule "probe before wrapping"
paid off most.

- Probed from scalac 2.13.18 against the published jars: okay-codec
  is MOSTLY usable as it is. `okay.codec.Schema` and its cases, `wrap`,
  `refine`, `enumeration`, `Cbor`, `Yaml` and `Validate` all work, and
  Scala 2's implicit search finds okay-codec's Scala 3 givens (even
  `Schema[Option[Vector[Long]]]`). Three things do not work. The
  `derives Schema` macro. `okay.codec.Json`, whose TASTy crashes the
  2.13 reader (`MatchError/49`), taking `JsonSchema.of` with it. And
  any Scala 3 top-level definition, which Scala 2 cannot see at all.
- The new module okay-scala2-codec replaces exactly those, in package
  `okay.scala2`: `Schemas.product1..16` (generated, circe `forProductN`
  style, with by-name implicit field schemas so recursive types work),
  `constant`, `sum`/`variant` (a `ClassTag` per case), and `Json`
  (`write`/`read`/`readStrict`) and `JsonSchema.of` over text.
- `TestCodecFromScala2`: a product, a sealed hierarchy with a case
  object, a recursive tree, a JSON Schema and a decode error, all from
  Scala 2. The probe has 52 tests, under `-Xlint -Werror`.
- Docs: section 8a of docs/scala2.md (copied from the probe), a module
  page, API reference, typepedia (including the `Schemas.field` cast
  in the registry), and spec stage 6. HTTP, SQL, agents and UI are
  queued as stages 7–10.
