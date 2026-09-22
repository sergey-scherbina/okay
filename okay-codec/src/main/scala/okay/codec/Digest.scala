package okay.codec

/**
 * A SCHEMA'S SHAPE, WITHOUT THE SCHEMA (specs/federation.md, stage 3
 * — "schema at the door").
 *
 * `Compat.compare` needs two LIVE `Schema` values, and a `Schema` is
 * not itself Schema-derivable: `SProduct`'s `make`/`parts` and every
 * field's own thunk are FUNCTIONS, and no codec serialises a
 * function. Two processes that do not share a build cannot exchange
 * a `Schema[A]` — only a description of its shape.
 *
 * `Digest` is that description: a small, `derives Schema` mirror of
 * exactly what `Compat.walk` reads to compare two schemas — names,
 * nesting, field presence, whether a default exists — and nothing it
 * does not (never a value, never a function). Reading `Compat.walk`
 * proves this is enough: it inspects `.name`, `.fields`/`.cases`
 * (thunked SCHEMAS, recursed into — never called for a VALUE) and
 * `.defaults` (`.isDefined`, a presence check, never the thunk
 * itself). Nothing it does is a value-level operation, so a
 * data-only mirror of the same shape carries everything the
 * comparison needs.
 */
enum Digest derives Schema:
  case Prim(shape: String)
  case Opt(of: Digest)
  case Lst(of: Digest)
  case Vec(of: Digest)
  case Prod(name: String, fields: Vector[(String, Digest)], defaulted: Vector[Boolean])
  case Sm(name: String, cases: Vector[(String, Digest)])

object Digest:

  private def under(s: Schema[?]): Schema[?] = s match
    case Schema.SIso(u, _, _) => under(u())
    case other => other

  /**
   * BUILD A DIGEST FROM A LIVE SCHEMA.
   *
   * A SELF-REFERENTIAL SCHEMA DOES NOT LOOP. A recursive type (a
   * tree) re-enters its own thunk without limit; `Compat.walk` stops
   * this with a recursion guard (a repeated name-pair returns
   * immediately, before touching `.fields`), and building a `Digest`
   * needs the same guard for the same reason — nothing else would
   * ever terminate. A REPEATED name is truncated to an empty
   * product/sum here. That is safe rather than merely convenient:
   * `Compat.walk`'s guard fires on the name pair *before* it ever
   * inspects a repeated occurrence's fields (proven by reading
   * `Compat.walk` — `if seen(key) then Vector.empty` runs first), so
   * the truncated stub is provably never consulted. The FIRST,
   * outer occurrence of any name always carries its real fields.
   */
  def of(s: Schema[?]): Digest = of(s, Set.empty)

  private def of(s: Schema[?], seen: Set[String]): Digest = under(s) match
    case Schema.SInt => Prim("Int")
    case Schema.SLong => Prim("Long")
    case Schema.SDouble => Prim("Double")
    case Schema.SBool => Prim("Boolean")
    case Schema.SString => Prim("String")
    case Schema.SChar => Prim("Char")
    case Schema.SBytes => Prim("Bytes")
    case Schema.SBigInt => Prim("BigInt")
    case Schema.SOption(of0) => Opt(of(of0(), seen))
    case Schema.SList(of0) => Lst(of(of0(), seen))
    case Schema.SVector(of0) => Vec(of(of0(), seen))
    case p: Schema.SProduct[?] =>
      if seen(p.name) then Prod(p.name, Vector.empty, Vector.empty)
      else
        val seen2 = seen + p.name
        Prod(p.name, p.fields.map((n, sc) => n -> of(sc(), seen2)),
          p.fields.indices.map(i => p.defaults.lift(i).flatten.isDefined).toVector)
    case su: Schema.SSum[?] =>
      if seen(su.name) then Sm(su.name, Vector.empty)
      else
        val seen2 = seen + su.name
        Sm(su.name, su.cases.map((n, sc) => n -> of(sc(), seen2)))
    case Schema.SIso(_, _, _) =>
      // unreachable: `under` already stripped every SIso before this
      // match runs; named so a future SIso-under-SIso surprise fails
      // loudly instead of silently mis-describing a wrapper as itself
      throw IllegalStateException("Digest.of: under() left an SIso in place")

  /**
   * THE DIGEST, AS A SCHEMA — the one isolated, defended cast this
   * file needs. `Compat.walk`/`compareField` never call
   * `SProduct.make`/`.parts` or `SSum.caseOf` (verified by reading
   * `Compat.scala`: they read only `.name`, `.fields`, `.defaults` as
   * a presence flag, and `.cases`), so a SHELL whose value-level
   * functions throw is safe to hand to `Compat.compare` — the throw
   * never fires, and if a future change to `Compat` ever called one,
   * it says so loudly rather than corrupting a comparison silently.
   */
  private def toSchema(d: Digest): Schema[Any] =
    def boom(name: String): Nothing = throw IllegalStateException(
      s"Digest.toSchema's shell for '$name' was asked for a VALUE — Compat.walk never does " +
        "this; something new is reading a shell schema as though it were real")
    (d match
      case Prim("Int") => Schema.SInt
      case Prim("Long") => Schema.SLong
      case Prim("Double") => Schema.SDouble
      case Prim("Boolean") => Schema.SBool
      case Prim("String") => Schema.SString
      case Prim("Char") => Schema.SChar
      case Prim("Bytes") => Schema.SBytes
      case Prim("BigInt") => Schema.SBigInt
      case Prim(other) => throw IllegalArgumentException(s"not a primitive digest: $other")
      case Opt(of0) => Schema.SOption(() => toSchema(of0))
      case Lst(of0) => Schema.SList(() => toSchema(of0))
      case Vec(of0) => Schema.SVector(() => toSchema(of0))
      case Prod(name, fields, defaulted) =>
        Schema.SProduct[Any](name, fields.map((n, f) => n -> (() => toSchema(f))),
          _ => boom(name), _ => boom(name),
          defaulted.map(b => if b then Some(() => boom(name)) else None))
      case Sm(name, cases) =>
        Schema.SSum[Any](name, cases.map((n, c) => n -> (() => toSchema(c))), _ => boom(name))
    ).asInstanceOf[Schema[Any]]

  /**
   * MY SCHEMA (LIVE) AGAINST THE REMOTE SIDE'S DIGEST — the check a
   * party runs before writing a byte: will the reader on the other
   * end, with ITS schema, be able to decode what I write with MINE?
   * `Report.backward` is exactly that question, with `local` playing
   * "old" (the producer) and `remote` playing "new" (the reader) in
   * `Compat`'s own naming.
   */
  def compare(local: Schema[?], remote: Digest): Compat.Report =
    Compat.compare(local, toSchema(remote))
