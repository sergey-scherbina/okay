## okay2-codec - Schema and JSON for okay2

Second of the modules the operator asked for (json, xml, sql, http).
This lane ports okay-codec's `Schema` and JSON as `okay2-codec` on JVM,
Scala.js and Scala Native (spec stage 41, docs §33).

`Schema[A]` has the Scala 3 cases. Scala 3 matches them with GADT
refinement, which Scala 2 does not do for a case object, so dispatch
goes through a visitor, `visit(Visit[F]): F[A]`. `fold`, `Folded` and
the `Step` walk sit on top of it. Derivation is a blackbox macro:
- a case class becomes a product, a case object an empty product, and a
  sealed type a sum;
- `scala.` and `java.` types are never derived;
- defaults come from the companion and are applied to the type's
  arguments, so a generic product's default works where Scala 3's does
  not.

The JSON half covers:
- the value and `print`;
- `parse`: the fast road, falling back to the lossless CST, with the two
  agreeing on every corpus document and every prefix;
- `cst`, `render` and `lossless`;
- `mergePatch` (RFC 7396);
- `encode`, `decode`, `read` and `write`;
- `JsonStrict`;
- the opt-in `literals`.

Every walk is native below `Codecs.NativeThreshold` and a `Cont.defer`
trampoline above it. It is safe at 100 000 levels; a mutant without the
trampoline fails all six depth tests. 86 tests on each platform.

Found along the way: in Scala 2 a `Double => Json` view silently accepts
a `Long` through numeric widening. The numeric views are exact now
(`=:=`). The unported dialects (Cbor, JsonSchema, Validate, …) are filed
as `okay2-codec-dialects`.
