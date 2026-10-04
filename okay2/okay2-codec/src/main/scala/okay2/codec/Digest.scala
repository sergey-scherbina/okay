package okay2.codec

/**
 * A SCHEMA'S SHAPE, WITHOUT THE SCHEMA (okay-codec's Digest.scala).
 * A `Schema` holds functions (`make`, `parts`, thunks), so two processes
 * that do not share a build cannot exchange one — only a description of
 * its shape. `Digest` is that description: a derivable mirror of exactly
 * what `Compat.walk` reads — names, nesting, field presence, whether a
 * default exists — and never a value or a function.
 */
sealed trait Digest

object Digest {
  final case class Prim(shape: String) extends Digest
  final case class Opt(of: Digest) extends Digest
  final case class Lst(of: Digest) extends Digest
  final case class Vec(of: Digest) extends Digest
  final case class Prod(name: String, fields: Vector[(String, Digest)], defaulted: Vector[Boolean]) extends Digest
  final case class Sm(name: String, cases: Vector[(String, Digest)]) extends Digest

  /**
   * Written by hand, the shape `Schema.derived` would give it (a sum of
   * products, cases and fields by name): the derivation macro cannot
   * expand in the module that defines it. Lazy, because it is recursive.
   */
  implicit lazy val schema: Schema[Digest] = {
    val self = Schema.once(schema)
    val pair: Schema[(String, Digest)] =
      Hand.two[(String, Digest), String, Digest]("Tuple2", "_1", Schema.once(Schema.string), "_2", self,
        (a, b) => (a, b), p => (p._1, p._2))
    val pairs: Schema[Vector[(String, Digest)]] = Schema.vector(pair)
    val prim = Hand.one[Prim, String]("Prim", "shape", Schema.once(Schema.string), Prim(_), _.shape)
    val opt = Hand.one[Opt, Digest]("Opt", "of", self, Opt(_), _.of)
    val lst = Hand.one[Lst, Digest]("Lst", "of", self, Lst(_), _.of)
    val vec = Hand.one[Vec, Digest]("Vec", "of", self, Vec(_), _.of)
    val prod = Hand.three[Prod, String, Vector[(String, Digest)], Vector[Boolean]]("Prod",
      "name", Schema.once(Schema.string), "fields", Schema.once(pairs),
      "defaulted", Schema.once(Schema.vector(Schema.bool)), Prod(_, _, _), p => (p.name, p.fields, p.defaulted))
    val sm = Hand.two[Sm, String, Vector[(String, Digest)]]("Sm",
      "name", Schema.once(Schema.string), "cases", Schema.once(pairs), Sm(_, _), s => (s.name, s.cases))
    Schema.SSum[Digest]("Digest",
      Vector("Prim" -> (() => prim), "Opt" -> (() => opt), "Lst" -> (() => lst), "Vec" -> (() => vec),
        "Prod" -> (() => prod), "Sm" -> (() => sm)),
      {
        case _: Prim => 0
        case _: Opt => 1
        case _: Lst => 2
        case _: Vec => 3
        case _: Prod => 4
        case _: Sm => 5
      })
  }

  /** small typed products: `make` and `parts` of `SProduct` speak `Any`
   * because a product's fields are a heterogeneous list; here each
   * field's part is read back at the type the SAME call declared for
   * it, through `part`, the one cast */
  private object Hand {
    private def part[X](v: Any): X = v.asInstanceOf[X]

    def one[A, X](name: String, n1: String, s1: () => Schema[X], make: X => A, get: A => X): Schema[A] =
      Schema.SProduct[A](name, Vector(n1 -> s1), ps => make(part[X](ps(0))), a => Seq(get(a)))

    def two[A, X, Y](name: String, n1: String, s1: () => Schema[X], n2: String, s2: () => Schema[Y],
                     make: (X, Y) => A, get: A => (X, Y)): Schema[A] =
      Schema.SProduct[A](name, Vector(n1 -> s1, n2 -> s2),
        ps => make(part[X](ps(0)), part[Y](ps(1))), a => { val (x, y) = get(a); Seq(x, y) })

    def three[A, X, Y, Z](name: String, n1: String, s1: () => Schema[X], n2: String, s2: () => Schema[Y],
                          n3: String, s3: () => Schema[Z], make: (X, Y, Z) => A, get: A => (X, Y, Z)): Schema[A] =
      Schema.SProduct[A](name, Vector(n1 -> s1, n2 -> s2, n3 -> s3),
        ps => make(part[X](ps(0)), part[Y](ps(1)), part[Z](ps(2))),
        a => { val (x, y, z) = get(a); Seq(x, y, z) })
  }

  @scala.annotation.tailrec
  private def under(s: Schema[_]): Schema[_] = s match {
    case i: Schema.SIso[_, _] => under(i.under())
    case other => other
  }

  /**
   * BUILD A DIGEST FROM A LIVE SCHEMA. A self-referential schema does
   * not loop: a REPEATED product or sum name is truncated to an empty
   * one, which `Compat.walk`'s own guard never consults (it fires on
   * the name pair before reading fields). The recursion is bounded by
   * the number of distinct names plus the containers between them.
   */
  def of(s: Schema[_]): Digest = of(s, Set.empty)

  private def of(s: Schema[_], seen: Set[String]): Digest = under(s) match {
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
    case p: Schema.SProduct[_] =>
      if (seen(p.name)) Prod(p.name, Vector.empty, Vector.empty)
      else {
        val seen2 = seen + p.name
        Prod(p.name, p.fields.map { case (n, sc) => n -> of(sc(), seen2) },
          p.fields.indices.map(i => p.defaults.lift(i).flatten.isDefined).toVector)
      }
    case su: Schema.SSum[_] =>
      if (seen(su.name)) Sm(su.name, Vector.empty)
      else {
        val seen2 = seen + su.name
        Sm(su.name, su.cases.map { case (n, sc) => n -> of(sc(), seen2) })
      }
    case _: Schema.SIso[_, _] =>
      // unreachable: `under` stripped every SIso; named so a surprise
      // fails loudly instead of mis-describing a wrapper
      throw new IllegalStateException("Digest.of: under() left an SIso in place")
  }

  /**
   * THE DIGEST, AS A SCHEMA. `Compat.walk` never calls `make`/`parts`
   * or `caseOf`, so a SHELL whose value-level functions throw is safe
   * to hand to `Compat.compare`; if something ever did call one, it
   * says so loudly. Built as `Schema[Any]` throughout: the leaves are
   * the case objects at their own types, widened by `shell`, the one
   * cast here — the shell is never asked for a value of any type.
   */
  private def toSchema(d: Digest): Schema[Any] = {
    def boom(name: String): Nothing = throw new IllegalStateException(
      s"Digest.toSchema's shell for '$name' was asked for a VALUE — Compat.walk never does " +
        "this; something new is reading a shell schema as though it were real")
    d match {
      case Prim("Int") => shell(Schema.SInt)
      case Prim("Long") => shell(Schema.SLong)
      case Prim("Double") => shell(Schema.SDouble)
      case Prim("Boolean") => shell(Schema.SBool)
      case Prim("String") => shell(Schema.SString)
      case Prim("Char") => shell(Schema.SChar)
      case Prim("Bytes") => shell(Schema.SBytes)
      case Prim("BigInt") => shell(Schema.SBigInt)
      case Prim(other) => throw new IllegalArgumentException(s"not a primitive digest: $other")
      case Opt(of0) => shell(Schema.SOption(() => toSchema(of0)))
      case Lst(of0) => shell(Schema.SList(() => toSchema(of0)))
      case Vec(of0) => shell(Schema.SVector(() => toSchema(of0)))
      case Prod(name, fields, defaulted) =>
        Schema.SProduct[Any](name, fields.map { case (n, f) => n -> (() => toSchema(f)) },
          _ => boom(name), _ => boom(name),
          defaulted.map(b => if (b) Some(() => boom(name)) else None))
      case Sm(name, cases) =>
        Schema.SSum[Any](name, cases.map { case (n, c) => n -> (() => toSchema(c)) }, _ => boom(name))
    }
  }

  /** a shape-only schema seen at `Any`: never decoded, never encoded */
  private def shell(s: Schema[_]): Schema[Any] = s.asInstanceOf[Schema[Any]]

  /** my schema (live) against the remote side's digest: `backward` is
   * "will the reader on the other end decode what I write" */
  def compare(local: Schema[_], remote: Digest): Compat.Report =
    Compat.compare(local, toSchema(remote))
}
